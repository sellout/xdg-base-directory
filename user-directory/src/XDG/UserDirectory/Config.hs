{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Configuration loading from @$XDG_CONFIG_HOME/user-dirs.dirs@.
module XDG.UserDirectory.Config
  ( loadConfig,
    ConfigError (..),
  )
where

import "base" Control.Applicative (empty, pure)
import "base" Control.Category ((.))
import "base" Control.Exception (tryJust)
import "base" Data.Either (Either (Left, Right))
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.List.NonEmpty (NonEmpty)
import "base" Data.String (String)
import "base" GHC.Generics (Generic)
import qualified "base" System.IO as IO
import "base" System.IO.Error (isDoesNotExistError)
import "base" Text.Show (Show)
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir), (</>))
import qualified "pathway" Data.Path.Directory as Directory
import qualified "text" Data.Text.IO as TIO
import "these" Data.These (These, these)
import qualified "xdg-base-directory" XDG.BaseDirectory as BaseDir
import "xdg-base-directory" XDG.BaseDirectory.Internal (Error)
import qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import "this" XDG.UserDirectory.Parser
  ( ParseError,
    UserDirsConfig,
    parseUserDirs,
  )

-- | Errors that can occur when loading the configuration.
data ConfigError
  = -- | Could not determine the config home directory.
    ConfigHomeError (NonEmpty Error)
  | -- | Failed to parse the config file.
    ParseFailed ParseError
  | -- | The config file does not exist or could not be read.
    ConfigFileNotFound
  deriving stock (Eq, Generic, Show)

-- | Load the user directories configuration from @$XDG_CONFIG_HOME/user-dirs.dirs@.
loadConfig ::
  IO.IO (Either ConfigError UserDirsConfig)
loadConfig = do
  configHomeResult <- configHomeString
  case theseToEither configHomeResult of
    Left errs -> pure . Left $ ConfigHomeError errs
    Right configHome -> do
      filename <- fromStringLiteralString "user-dirs.dirs"
      let configFile = configHome </> Directory.selectFile Directory.current filename
      result <-
        tryJust
          (\e -> if isDoesNotExistError e then pure () else empty)
          (Patch.withFile configFile IO.ReadMode TIO.hGetContents)
      case result of
        Left () -> pure $ Left ConfigFileNotFound
        Right text ->
          pure $ case parseUserDirs text of
            Left err -> Left $ ParseFailed err
            Right config -> Right config
  where
    theseToEither :: These (NonEmpty a) b -> Either (NonEmpty a) b
    theseToEither = these Left Right (\_ b -> Right b)

    configHomeString :: IO.IO (These (NonEmpty Error) (Path 'Abs 'Dir String))
    configHomeString = BaseDir.configHome

    fromStringLiteralString :: String -> IO.IO String
    fromStringLiteralString = System.fromStringLiteral
