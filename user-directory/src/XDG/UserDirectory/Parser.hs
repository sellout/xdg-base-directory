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
    parsePathToDirectoryValue,
    ParseError,

    -- * Serialization
    serializeUserDirs,
    serializeDirectoryValue,

    -- * Config manipulation
    setDirectory,

    -- * Types
    DirectoryValue (..),
    UserDirsConfig,
  )
where

import "base" Control.Applicative (pure, (*>), (<*), (<|>))
import "base" Control.Category ((.))
import "base" Control.Monad (void)
import "base" Data.Bool (otherwise, (&&))
import "base" Data.Char (Char)
import "base" Data.Either (Either)
import "base" Data.Eq (Eq, (/=))
import "base" Data.Function (($))
import "base" Data.Foldable (concatMap)
import "base" Data.Functor ((<$), (<$>))
import "base" Data.List (drop, isPrefixOf, length, unlines)
import qualified "base" Data.Kind as Kind
import "base" Data.Maybe (Maybe (Just, Nothing), catMaybes)
import "base" Data.Ord (Ord)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" GHC.Generics (Generic)
import "base" Text.Show (Show)
import qualified "containers" Data.Map.Strict as Map
import "megaparsec" Text.Megaparsec
  ( Parsec,
    anySingle,
    between,
    chunk,
    eof,
    manyTill,
    noneOf,
    optional,
    parse,
    sepEndBy,
    takeWhile1P,
    try,
  )
import "megaparsec" Text.Megaparsec.Char (char, newline, space1)
import qualified "megaparsec" Text.Megaparsec.Char.Lexer as L
import qualified "megaparsec" Text.Megaparsec.Error as E
import qualified "text" Data.Text as T
import "this" XDG.UserDirectory.Type (UserDirectory (UserDirectory))

-- | A directory value from the config file.
data DirectoryValue
  = -- | A path relative to @$HOME@
    HomeRelative String
  | -- | An absolute path
    Absolute String
  deriving stock (Eq, Generic, Ord, Show)

-- | Parsed user directories configuration.
type UserDirsConfig = Map.Map UserDirectory DirectoryValue :: Kind.Type

-- | Parse error type.
type ParseError = E.ParseErrorBundle T.Text () :: Kind.Type

-- | Parser type.
type Parser = Parsec () T.Text :: Kind.Type -> Kind.Type

-- | Skip whitespace and comments.
spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment (T.pack "#"))
    (L.skipBlockComment (T.pack "/*") (T.pack "*/"))

-- | Parse an escape sequence within a quoted string.
escapeChar :: Parser Char
escapeChar =
  char '\\'
    *> ( ('\\' <$ char '\\')
           <|> ('"' <$ char '"')
           <|> ('$' <$ char '$')
           <|> anySingle
       )

-- | Parse the content of a quoted string.
quotedContent :: Parser String
quotedContent = manyTill (escapeChar <|> noneOf ['"', '\\']) (char '"')

-- | Parse a value (either $HOME/... or /...).
parseValue :: Parser DirectoryValue
parseValue =
  between (char '"') (pure ()) $
    (HomeRelative <$> (chunk (T.pack "$HOME") *> optional (char '/') *> quotedContent))
      <|> (Absolute <$> quotedContent)

-- | Parse an XDG variable name and extract the directory type.
--
--   Expects format: @XDG_<TYPE>_DIR=@
parseVarName :: Parser (Maybe UserDirectory)
parseVarName = do
  _ <- chunk (T.pack "XDG_")
  name <- T.unpack <$> takeWhile1P Nothing (\c -> c /= '=' && c /= '_')
  _ <- chunk (T.pack "_DIR=")
  pure $ Just $ UserDirectory name

-- | Parse a single assignment line.
parseLine :: Parser (Maybe (UserDirectory, DirectoryValue))
parseLine = do
  mDir <- parseVarName
  value <- parseValue
  pure $ (\ud -> (ud, value)) <$> mDir

-- | Skip unknown lines (lines that don't parse as XDG user dirs).
--   Requires at least one character to be skipped (won't match empty at EOF).
skipLine :: Parser ()
skipLine = void $ anySingle *> manyTill anySingle (void newline <|> eof)

-- | Parse a line or skip it if it doesn't match.
parseOrSkipLine :: Parser (Maybe (UserDirectory, DirectoryValue))
parseOrSkipLine = try parseLine <|> (Nothing <$ skipLine)

-- | Parse the entire user-dirs.dirs file.
parseUserDirs :: T.Text -> Either ParseError UserDirsConfig
parseUserDirs =
  parse
    ( spaceConsumer
        *> (Map.fromList . catMaybes <$> sepEndBy parseOrSkipLine (void (optional newline)))
        <* eof
    )
    "user-dirs.dirs"

-- | Serialize a directory value to its config file representation.
--
--   Examples:
--
--   - @HomeRelative "Desktop"@ becomes @\"$HOME/Desktop\"@
--   - @Absolute "/tmp/test"@ becomes @\"/tmp/test\"@
serializeDirectoryValue :: DirectoryValue -> String
serializeDirectoryValue = \case
  HomeRelative path -> "\"$HOME/" <> escapeString path <> "\""
  Absolute path -> "\"" <> escapeString path <> "\""

-- | Escape special characters in a string for the config file format.
escapeString :: String -> String
escapeString = concatMap escapeChar'
  where
    escapeChar' '\\' = "\\\\"
    escapeChar' '"' = "\\\""
    escapeChar' '$' = "\\$"
    escapeChar' c = [c]

-- | Serialize a user directories config to the file format.
--
--   Produces output like:
--
--   > XDG_DESKTOP_DIR="$HOME/Desktop"
--   > XDG_DOWNLOAD_DIR="$HOME/Downloads"
serializeUserDirs :: UserDirsConfig -> T.Text
serializeUserDirs config =
  T.pack $ unlines $ serializeLine <$> Map.toList config
  where
    serializeLine (UserDirectory name, value) =
      "XDG_" <> name <> "_DIR=" <> serializeDirectoryValue value

-- | Parse a command-line path argument into a DirectoryValue.
--
--   Handles various path formats:
--
--   - @/absolute/path@ becomes @Absolute "/absolute/path"@
--   - @$HOME/path@ becomes @HomeRelative "path"@
--   - @~/path@ becomes @HomeRelative "path"@
--   - @relative/path@ becomes @HomeRelative "relative/path"@
parsePathToDirectoryValue :: String -> DirectoryValue
parsePathToDirectoryValue path
  | "/" `isPrefixOf` path = Absolute path
  | "$HOME/" `isPrefixOf` path = HomeRelative (drop (length "$HOME/") path)
  | "$HOME" `isPrefixOf` path = HomeRelative (drop (length "$HOME") path)
  | "~/" `isPrefixOf` path = HomeRelative (drop (length "~/") path)
  | "~" `isPrefixOf` path = HomeRelative (drop (length "~") path)
  | otherwise = HomeRelative path

-- | Set a directory in the config.
--
--   Updates the config with the specified directory value, adding it if
--   it doesn't exist or replacing it if it does.
setDirectory :: UserDirectory -> DirectoryValue -> UserDirsConfig -> UserDirsConfig
setDirectory = Map.insert
