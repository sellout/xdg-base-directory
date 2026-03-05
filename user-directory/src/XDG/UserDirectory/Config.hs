{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

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
import "base" Control.Monad ((=<<))
import "base" Data.Bifunctor (first)
import "base" Data.Either (Either (Left), either)
import "base" Data.Eq (Eq)
import "base" Data.Function (const, ($))
import "base" Data.Functor ((<$>))
import "base" Data.List.NonEmpty (NonEmpty)
import "base" Data.String (String)
import "base" GHC.Generics (Generic)
import qualified "base" System.IO as IO
import "base" System.IO.Error (isDoesNotExistError)
import "base" Text.Show (Show)
import "pathway" Data.Path ((</>))
import "pathway" Data.Path.TH (posix)
import qualified "text" Data.Text.IO as TIO
import "these" Data.These (These, these)
import qualified "xdg-base-directory" XDG.BaseDirectory as BaseDir
import "xdg-base-directory" XDG.BaseDirectory.Internal (Error)
import qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
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
loadConfig :: IO.IO (Either ConfigError UserDirsConfig)
loadConfig =
  either
    (pure . Left . ConfigHomeError)
    ( \configHome ->
        either
          (\() -> Left ConfigFileNotFound)
          (first ParseFailed . parseUserDirs)
          <$> tryJust
            (\e -> if isDoesNotExistError e then pure () else empty)
            -- Force the text to be fully read before the handle is closed
            ( Patch.withFile @_ @String
                (configHome </> [posix|user-dirs.dirs|])
                IO.ReadMode
                TIO.hGetContents
            )
    )
    . theseToEither
    =<< BaseDir.configHome
  where
    theseToEither :: These (NonEmpty a) b -> Either (NonEmpty a) b
    theseToEither = these Left pure $ const pure
