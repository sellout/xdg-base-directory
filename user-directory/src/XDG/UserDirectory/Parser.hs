{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Parser for the @user-dirs.dirs@ file format.
--
-- The file format consists of lines like:
--
-- > XDG_DESKTOP_DIR="$HOME/Desktop"
-- > XDG_DOWNLOAD_DIR="$HOME/Downloads"
--
-- Values can be:
--
-- - @$HOME/path@ - relative to the home directory
-- - @/absolute/path@ - an absolute path
--
-- Escape sequences @\\\\@ and @\\"@ are supported within quoted values.
module XDG.UserDirectory.Parser
  ( -- * Parsing
    parseUserDirs,
    pathToDirectoryValue,
    ParseError,

    -- * Serialization
    serializeUserDirs,
    serializeDirectoryValue,

    -- * Config manipulation
    setDirectory,
    configFileFormat,

    -- * Types
    DirectoryValue (..),
    UserDirsConfig,
  )
where

import "base" Control.Applicative (pure, (*>), (<*), (<|>))
import "base" Control.Category ((.))
import "base" Control.Monad (void, (=<<))
import "base" Data.Bool (Bool (False), not, (&&))
import "base" Data.Char (Char, isSpace)
import "base" Data.Either (Either)
import "base" Data.Eq (Eq, (/=))
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor, fmap, (<$), (<$>))
import qualified "base" Data.Kind as Kind
import "base" Data.List (unlines)
import "base" Data.Maybe (Maybe (Nothing), catMaybes)
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Ord (Ord)
import "base" Data.Semigroup ((<>))
import "base" Data.String (IsString, String)
import "base" Data.Traversable (Traversable)
import "base" Data.Type.Equality (type (~))
import "base" GHC.Generics (Generic, Generic1)
import "base" System.IO (IO)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import qualified "containers" Data.Map.Strict as Map
import "megaparsec" Text.Megaparsec
  ( anySingle,
    between,
    chunk,
    eof,
    manyTill,
    optional,
    sepEndBy,
    takeWhile1P,
    try,
  )
import qualified "megaparsec" Text.Megaparsec as MP
import "megaparsec" Text.Megaparsec.Char (char, newline, space1)
import qualified "megaparsec" Text.Megaparsec.Char.Lexer as L
import qualified "megaparsec" Text.Megaparsec.Error as E
import "pathway" Data.Path (Path, Type (Dir))
import qualified "pathway" Data.Path as Path
import "pathway" Data.Path.Format (Format)
import qualified "pathway" Data.Path.Format as Format
import qualified "pathway" Data.Path.Parser as Parser
import "pathway" Data.Path.Relativity (Relativity (Abs, Any, Rel))
-- FIXME: Probably shouldn’t be using strict `Maybe` for this in Pathway.
import "strict" Data.Strict.Maybe (maybe)
import qualified "xdg-base-directory" XDG.BaseDirectory.Internal as FS
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import "this" XDG.UserDirectory.Type
  ( DirectoryValue (Absolute, HomeRelative),
    UserDirectory (UserDirectory),
    UserDirsConfig,
  )

-- | Parse error type.
--
--  __FIXME__: `InvalidDir` (and perhaps other parse errors) shouldn’t be
--             errors, but just collected as warnings. Any line that can’t be
--             parsed is just ignored by @xdg-user-dir@. However, we still want
--             to record the warning, so we can report that it _might_ be the
--             case that we failed to parse a line that should have been what we
--             wanted.
--
--             We may also want different severities – a line that completely
--             failed to parse would be low, but one that parsed the
--             `UserDirectory`, then failed to parse the `DirectoryValue` would
--             be more likely to be a bug in the file.
type ParseError = E.ParseErrorBundle String (InvalidDir String) :: Kind.Type

-- | Skip whitespace and comments.
--
--  __FIXME__: There are no comments, this should just skip spaces and tabs.
spaceConsumer :: (MP.MonadParsec v s p, MP.Token s ~ Char, IsString (MP.Tokens s)) => p ()
spaceConsumer = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

data InvalidDir (rep :: Kind.Type) = InvalidDir Relativity (Path 'Any 'Dir rep)
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving stock (Foldable, Functor, Generic1, Traversable)

type role InvalidDir nominal

relDir ::
  ( MP.MonadParsec (InvalidDir (MP.Tokens s)) s p,
    IsString (MP.Tokens s),
    Monoid (MP.Tokens s),
    MP.Token s ~ Char
  ) =>
  p (Path ('Rel 'False) 'Dir (MP.Tokens s))
relDir =
  ( \dir -> case Path.anchor $ Path.forgetType dir of
      Path.RelDir rd -> pure rd
      _ -> MP.customFailure $ InvalidDir (Rel False) dir
  )
    =<< Parser.directory configFileFormat

absDir ::
  ( MP.MonadParsec (InvalidDir (MP.Tokens s)) s p,
    IsString (MP.Tokens s),
    Monoid (MP.Tokens s),
    MP.Token s ~ Char
  ) =>
  p (Path 'Abs 'Dir (MP.Tokens s))
absDir =
  ( \dir -> case Path.anchor $ Path.forgetType dir of
      Path.AbsDir ad -> pure ad
      _ -> MP.customFailure $ InvalidDir Abs dir
  )
    =<< Parser.directory configFileFormat

-- | Parse a value (either $HOME/... or /...).
parseValue ::
  ( MP.MonadParsec (InvalidDir (MP.Tokens s)) s p,
    IsString (MP.Tokens s),
    Monoid (MP.Tokens s),
    MP.Token s ~ Char
  ) =>
  p (DirectoryValue (MP.Tokens s))
parseValue =
  between (char '"') (char '"') $
    (chunk "$HOME/" *> (HomeRelative <$> relDir))
      <|> (Absolute <$> absDir)

-- | Parse an XDG variable name and extract the directory type.
--
--   Expects format: @XDG_<TYPE>_DIR=@
parseVarName :: (MP.MonadParsec v String p) => p (Maybe UserDirectory)
parseVarName = do
  _ <- chunk "XDG_"
  name <- takeWhile1P Nothing (\c -> not (isSpace c) && c /= '=')
  -- FIXME: I _think_ the upstream parser actually allows the @=@ to be omitted,
  --        and also arbitrary space before the quoted value.
  _ <- chunk "_DIR="
  pure . pure $ UserDirectory name

-- | Parse a single assignment line.
parseLine ::
  (MP.MonadParsec (InvalidDir String) String p) =>
  p (Maybe (UserDirectory, DirectoryValue String))
parseLine = do
  mDir <- parseVarName
  value <- parseValue
  pure $ (,value) <$> mDir

-- | Skip unknown lines (lines that don't parse as XDG user dirs).
--   Requires at least one character to be skipped (won't match empty at EOF).
skipLine :: (MP.MonadParsec v s p, MP.Token s ~ Char) => p ()
skipLine = void $ anySingle *> manyTill anySingle (void newline <|> eof)

-- | Parse a line or skip it if it doesn't match.
parseOrSkipLine ::
  (MP.MonadParsec (InvalidDir String) String p) =>
  p (Maybe (UserDirectory, DirectoryValue String))
parseOrSkipLine = try parseLine <|> (Nothing <$ skipLine)

-- | Parse the entire user-dirs.dirs file.
parseUserDirs ::
  (MP.MonadParsec (InvalidDir String) String p) => p (UserDirsConfig String)
parseUserDirs =
  spaceConsumer
    *> (Map.fromList . catMaybes <$> sepEndBy parseOrSkipLine (void $ optional newline))
    <* eof

-- | Regardless of system, the user-dirs.dirs file is always written in POSIX
--   format, with a minimal set of substitutions.
--
--   When displaying paths to the user, we should still use `Format.local`.
configFileFormat :: (IsString rep, Monoid rep, Ord rep) => Format rep
configFileFormat =
  Format.posix
    { -- Ensure we never output `./` at the start of a path.
      Format.current = mempty,
      -- These are the only substitutions supported by xdg-user-dirs.
      Format.substitutions = Map.fromList [("\"", "\\\""), ("\\", "\\\\")]
    }

-- | Serialize a directory value to its config file representation.
--
--   Examples:
--
--   - @HomeRelative "Desktop"@ becomes @\"$HOME/Desktop\"@
--   - @Absolute "/tmp/test"@ becomes @\"/tmp/test\"@
--
--  __FIXME__: Export `Substible` from Pathway, so we don’t have to hardcode `rep` here.
serializeDirectoryValue ::
  ( IsString rep,
    rep ~ String -- Monoid rep, Ord rep, Path.Substible rep
  ) =>
  DirectoryValue rep -> rep
serializeDirectoryValue =
  ("\"" <>) . (<> "\"") . \case
    HomeRelative path -> "$HOME/" <> Path.toText configFileFormat path
    Absolute path -> Path.toText configFileFormat path

-- | Serialize a user directories config to the file format.
--
--   Produces output like:
--
--   > XDG_DESKTOP_DIR="$HOME/Desktop"
--   > XDG_DOWNLOAD_DIR="$HOME/Downloads"
serializeUserDirs :: UserDirsConfig String -> String
serializeUserDirs config =
  unlines $ serializeLine <$> Map.toList config
  where
    serializeLine (UserDirectory name, value) =
      "XDG_" <> name <> "_DIR=" <> serializeDirectoryValue value

-- | Parse a command-line path argument into a DirectoryValue.
--
--   Handles various path formats:
--
--   - @/absolute/path@ becomes @Just (Absolute "/absolute/path")@
--   - @$HOME/path@ becomes @Just (HomeRelative "path")@
--   - anything else becomes @Nothing@ (rejected)
pathToDirectoryValue ::
  (Eq rep, System.Rep rep) =>
  Path 'Abs 'Dir rep ->
  IO (Either FS.Error (DirectoryValue rep))
pathToDirectoryValue path =
  fmap (maybe (Absolute path) HomeRelative . (`Path.routePrefix` path))
    <$> FS.getHomeDirectory

-- | Set a directory in the config.
--
--   Updates the config with the specified directory value, adding it if
--   it doesn't exist or replacing it if it does.
setDirectory :: UserDirectory -> DirectoryValue rep -> UserDirsConfig rep -> UserDirsConfig rep
setDirectory = Map.insert
