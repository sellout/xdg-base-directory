{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Update functionality for XDG user directories.
--
-- This module provides functions to ensure user directories exist, creating
-- them if necessary. This is similar to the @xdg-user-dirs-update@ command
-- from the reference implementation.
module XDG.UserDirectory.Update
  ( ensureUserDirectory,
    ensureAllUserDirectories,
    UpdateError (..),
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Monad ((<=<))
import "base" Data.Bifunctor (bimap)
import "base" Data.Either (Either (Left), either)
import "base" Data.Eq (Eq)
import "base" Data.Functor ((<$>))
import "base" Data.Ord (Ord)
import "base" Data.String (String)
import "base" Data.Traversable (traverse)
import "base" GHC.Generics (Generic)
import "base" System.IO (IO)
import "base" Text.Show (Show)
import qualified "containers" Data.Map.Strict as Map
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir))
import qualified "pathway-system" Filesystem.Path as Dir
import "transformers" Control.Monad.Trans.Except (runExceptT)
-- FIXME: Shouldn’t need to import from ".Internal".
import qualified "xdg-base-directory" XDG.BaseDirectory.Internal as BaseDir
import qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import qualified "this" XDG.UserDirectory as UD
import qualified "this" XDG.UserDirectory.Config as Config
import "this" XDG.UserDirectory.Type
  ( DirectoryValue,
    UserDirectory,
    UserDirsConfig,
  )

-- | Errors that can occur when updating user directories.
--
-- @since 0.0.1.0
data UpdateError
  = ConfigError Config.Error
  | -- | Could not look up the directory configuration.
    LookupError BaseDir.Error
  | -- | Failed to create the directory.
    CreationError Dir.MaybeParentCreationFailure
  deriving stock (Eq, Generic, Show)

ensureDirectoryValue ::
  (Ord rep, System.Rep rep) =>
  DirectoryValue rep -> IO (Either UpdateError (Path 'Abs 'Dir rep))
ensureDirectoryValue =
  either
    (pure . Left . LookupError)
    ( \path ->
        bimap CreationError (\() -> path)
          <$> runExceptT (Patch.createDirectoryWithParentsIfMissing path)
    )
    <=< UD.resolveDirectoryValue

-- | Ensure a user directory exists, creating it if necessary.
--
--   Returns the path to the directory on success. If the directory could not
--   be looked up or created, returns an error.
--
-- @since 0.0.1.0
ensureUserDirectory ::
  UserDirectory -> IO (Either UpdateError (Path 'Abs 'Dir String))
ensureUserDirectory dir = do
  config <- Config.load
  either
    (pure . Left . ConfigError)
    ( either
        (pure . Left . LookupError)
        ( \path ->
            bimap CreationError (\() -> path)
              <$> runExceptT (Patch.createDirectoryWithParentsIfMissing path)
        )
        <=< UD.getUserDirectory dir
    )
    config

-- | Ensure all well-known user directories exist, creating them if necessary.
--
--   Returns a map of all user directories to their resolved paths or errors.
--
-- @since 0.0.1.0
ensureAllUserDirectories ::
  UserDirsConfig String ->
  IO (Map.Map UserDirectory (Either UpdateError (Path 'Abs 'Dir String)))
ensureAllUserDirectories = traverse ensureDirectoryValue
