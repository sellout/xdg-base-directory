{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-term-variable-capture #-}

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
import "base" Data.Bool (Bool (False), (&&))
import "base" Data.Char (Char)
import "base" Data.Either (Either)
import "base" Data.Eq (Eq, (/=))
import "base" Data.Function (($))
import "base" Data.Functor (fmap, (<$), (<$>))
import qualified "base" Data.Kind as Kind
import "base" Data.List (unlines)
import "base" Data.Maybe (Maybe (Nothing), catMaybes)
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Ord (Ord)
import "base" Data.Semigroup ((<>))
import "base" Data.String (IsString, String)
import "base" Data.Type.Equality (type (~))
import "base" System.IO (IO)
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
import "pathway" Data.Path (Path, Relativity (Abs, Rel), Type (Dir))
import qualified "pathway" Data.Path as Path
import "pathway" Data.Path.Format (Format)
import qualified "pathway" Data.Path.Format as Format
import qualified "pathway" Data.Path.Parser as Parser
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
type ParseError (s :: Kind.Type) (e :: Kind.Type) = E.ParseErrorBundle s e :: Kind.Type

-- | Skip whitespace and comments.
spaceConsumer :: (MP.MonadParsec void s p, MP.Token s ~ Char, IsString (MP.Tokens s)) => p ()
spaceConsumer = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

-- |
--
--  __FIXME__: Pathway should be using `MP.MonadParsec`, not `MP.Parsec` directly.
anchoredPath :: (Ord void, MP.Stream s, IsString (MP.Tokens s), Monoid (MP.Tokens s), MP.Token s ~ Char) => MP.Parsec void s (Path.Anchored (MP.Tokens s))
anchoredPath =
  Path.anchor . Path.forgetType <$> Parser.directory configFileFormat

relDir :: (MP.Stream s, IsString (MP.Tokens s), Monoid (MP.Tokens s), MP.Token s ~ Char) => MP.Parsec String s (Path ('Rel 'False) 'Dir (MP.Tokens s))
relDir =
  ( \case
      Path.RelDir rd -> pure rd
      _ -> MP.customFailure "Not the path we were looking for."
  )
    =<< anchoredPath

absDir :: (MP.Stream s, IsString (MP.Tokens s), Monoid (MP.Tokens s), MP.Token s ~ Char) => MP.Parsec String s (Path 'Abs 'Dir (MP.Tokens s))
absDir =
  ( \case
      Path.AbsDir ad -> pure ad
      _ -> MP.customFailure "Not the path we were looking for."
  )
    =<< anchoredPath

-- | Parse a value (either $HOME/... or /...).
parseValue :: (MP.Stream s, IsString (MP.Tokens s), Monoid (MP.Tokens s), MP.Token s ~ Char) => MP.Parsec String s (DirectoryValue (MP.Tokens s))
parseValue =
  between (char '"') (char '"') $
    (chunk "$HOME/" *> (HomeRelative <$> relDir))
      <|> (Absolute <$> absDir)

-- | Parse an XDG variable name and extract the directory type.
--
--   Expects format: @XDG_<TYPE>_DIR=@
parseVarName :: (MP.MonadParsec void s p, MP.Token s ~ Char, MP.Tokens s ~ String) => p (Maybe UserDirectory)
parseVarName = do
  _ <- chunk "XDG_"
  name <- takeWhile1P Nothing (\c -> c /= '=' && c /= '_')
  _ <- chunk "_DIR="
  pure . pure $ UserDirectory name

-- | Parse a single assignment line.
parseLine :: (MP.Stream s, IsString (MP.Tokens s), Monoid (MP.Tokens s), MP.Token s ~ Char, MP.Tokens s ~ String) => MP.Parsec String s (Maybe (UserDirectory, DirectoryValue (MP.Tokens s)))
parseLine = do
  mDir <- parseVarName
  value <- parseValue
  pure $ (,value) <$> mDir

-- | Skip unknown lines (lines that don't parse as XDG user dirs).
--   Requires at least one character to be skipped (won't match empty at EOF).
skipLine :: (MP.MonadParsec void s p, MP.Token s ~ Char) => p ()
skipLine = void $ anySingle *> manyTill anySingle (void newline <|> eof)

-- | Parse a line or skip it if it doesn't match.
parseOrSkipLine :: (MP.Stream s, IsString (MP.Tokens s), Monoid (MP.Tokens s), MP.Token s ~ Char, MP.Tokens s ~ String) => MP.Parsec String s (Maybe (UserDirectory, DirectoryValue (MP.Tokens s)))
parseOrSkipLine = try parseLine <|> (Nothing <$ skipLine)

-- | Parse the entire user-dirs.dirs file.
parseUserDirs :: (MP.Stream s, IsString (MP.Tokens s), Monoid (MP.Tokens s), MP.Token s ~ Char, MP.Tokens s ~ String) => MP.Parsec String s (UserDirsConfig (MP.Tokens s))
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
    Monoid rep,
    Ord rep,
    rep ~ String -- Path.Substible rep
  ) =>
  DirectoryValue rep -> rep
serializeDirectoryValue = \case
  HomeRelative path -> "\"$HOME/" <> Path.toText configFileFormat path <> "\""
  Absolute path -> "\"" <> Path.toText configFileFormat path <> "\""

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
