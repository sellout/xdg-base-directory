{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
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
import "base" Data.Traversable (mapM)
import "base" Data.Either (Either (Left, Right))
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Ord (Ord)
import "base" GHC.Generics (Generic)
import qualified "base" System.IO as IO
import "base" Text.Show (Show)
import qualified "containers" Data.Map.Strict as Map
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir))
import qualified "pathway-system" Filesystem.Path as Dir
import "transformers" Control.Monad.Trans.Except (runExceptT)
import qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import qualified "this" XDG.UserDirectory as UD
import "this" XDG.UserDirectory.Type
  ( UserDirectory,
    wellKnownDirectories,
  )

-- | Errors that can occur when updating user directories.
data UpdateError
  = -- | Could not look up the directory configuration.
    LookupError UD.LookupError
  | -- | Failed to create the directory.
    CreationError Dir.MaybeParentCreationFailure
  deriving stock (Eq, Generic, Show)

-- | Ensure a user directory exists, creating it if necessary.
--
--   Returns the path to the directory on success. If the directory could not
--   be looked up or created, returns an error.
ensureUserDirectory ::
  (System.Rep rep, Ord rep) =>
  UserDirectory ->
  IO.IO (Either UpdateError (Path 'Abs 'Dir rep))
ensureUserDirectory dir = do
  pathResult <- UD.getUserDirectory dir
  case pathResult of
    Left err -> pure $ Left $ LookupError err
    Right path -> do
      createResult <- runExceptT $ Patch.createDirectoryWithParentsIfMissing path
      case createResult of
        Left err -> pure $ Left $ CreationError err
        Right () -> pure $ Right path

-- | Ensure all well-known user directories exist, creating them if necessary.
--
--   Returns a map of all user directories to their resolved paths or errors.
ensureAllUserDirectories ::
  (System.Rep rep, Ord rep) =>
  IO.IO (Map.Map UserDirectory (Either UpdateError (Path 'Abs 'Dir rep)))
ensureAllUserDirectories = do
  results <- mapM (\d -> (,) d <$> ensureUserDirectory d) wellKnownDirectories
  pure $ Map.fromList results
