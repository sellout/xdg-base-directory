{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Configuration loading from @$XDG_CONFIG_HOME/user-dirs.dirs@.
module XDG.UserDirectory.Config
  ( load,
    loadFrom,
    defaultFile,
    Error (..),
  )
where

import "base" Control.Applicative (empty, pure)
import "base" Control.Category ((.))
import "base" Control.Exception (tryJust)
import "base" Control.Monad (unless, (=<<))
import "base" Data.Bifunctor (first)
import "base" Data.Either (Either (Left), either)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (length)
import "base" Data.Function (const, ($))
import "base" Data.Functor (fmap, (<$>))
import "base" Data.List.NonEmpty (NonEmpty)
import "base" Data.String (IsString, String)
import "base" GHC.Generics (Generic)
import "base" System.IO (IO)
import qualified "base" System.IO as IO
import "base" System.IO.Error (isDoesNotExistError)
import "base" Text.Show (Show)
import qualified "megaparsec" Text.Megaparsec as MP
import "pathway" Data.Path (Path, Relativity (Abs), Type (File), (</>))
import qualified "pathway" Data.Path as Path
import qualified "pathway" Data.Path.Format as Format
import "pathway" Data.Path.TH (posix)
import "these" Data.These (These, these)
import qualified "xdg-base-directory" XDG.BaseDirectory as BaseDir
-- FIXME: Shouldn’t need to import from ".Internal".
import qualified "xdg-base-directory" XDG.BaseDirectory.Internal as BaseDir
import qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import "this" XDG.UserDirectory.Parser
  ( ParseError,
    UserDirsConfig,
    parseUserDirs,
  )

-- | Errors that can occur when loading the configuration.
--
-- @since 0.0.1.0
data Error
  = -- | Could not determine the config home directory.
    ConfigHomeError (NonEmpty BaseDir.Error)
  | -- | Failed to parse the config file.
    ParseFailed ParseError
  | -- | The config file does not exist or could not be read.
    ConfigFileNotFound
  deriving stock (Eq, Generic, Show)

-- | The location of the XDG user dirs config file.
--
-- @since 0.0.1.0
defaultFile ::
  (IsString rep, System.Rep rep) =>
  IO (These (NonEmpty BaseDir.Error) (Path 'Abs 'File rep))
defaultFile = fmap (</> [posix|user-dirs.dirs|]) <$> BaseDir.configHome

parseFile ::
  MP.Parsec e String a ->
  Path 'Abs 'File String ->
  IO (Either (MP.ParseErrorBundle String e) a)
parseFile p file =
  Patch.withFile file IO.ReadMode \h -> do
    text <- IO.hGetContents h
    -- FIXME: Don’t do this – use `with` operations to make sure everything is
    --        done while the handle is open.
    let !_ = length text
    pure $ MP.parse p (Path.toText Format.local file) text

-- | Load the user directories configuration from the provided file.
--
-- @since 0.0.1.0
loadFrom :: Path 'Abs 'File String -> IO (Either Error (UserDirsConfig String))
loadFrom file = do
  IO.print file
  fmap (either (\() -> Left ConfigFileNotFound) (first ParseFailed))
    . tryJust (\e -> unless (isDoesNotExistError e) empty)
    $ parseFile parseUserDirs file

-- | Load the user directories configuration from
--   @$XDG_CONFIG_HOME/user-dirs.dirs@.
--
-- @since 0.0.1.0
load :: IO (Either Error (UserDirsConfig String))
load =
  either (pure . Left . ConfigHomeError) loadFrom . theseToEither
    =<< defaultFile
  where
    theseToEither :: These a b -> Either a b
    theseToEither = these Left pure $ const pure

