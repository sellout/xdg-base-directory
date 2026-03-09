{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Implementation of the XDG user directories specification (xdg-user-dirs).
--
-- This module provides access to well-known user directories (Desktop,
-- Documents, Downloads, etc.) as defined in the @user-dirs.dirs@ configuration
-- file.
--
-- See <https://www.freedesktop.org/wiki/Software/xdg-user-dirs/> for the
-- specification.
module XDG.UserDirectory
  ( -- * Types
    UserDirectory (..),
    LookupError (..),

    -- * Lookup functions
    getUserDirectory,
    getAllUserDirectories,
    resolveDirectoryValue,

    -- * Re-exports
    envVarName,
  )
where

import "base" Control.Category ((.))
import "base" Data.Either (Either)
import "base" Data.Eq (Eq, (==))
import "base" Data.Functor (Functor, fmap, (<$>))
import "base" Data.Functor.Identity (Identity (Identity), runIdentity)
import "base" Data.String (IsString, String)
import "base" GHC.Generics (Generic)
import "base" System.IO (IO)
import "base" Text.Show (Show)
import qualified "containers" Data.Map.Strict as Map
import qualified "megaparsec" Text.Megaparsec as MP
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir), (</>))
import qualified "pathway" Data.Path.Directory as Dir
import "pathway" Data.Path.TH (posix)
import qualified "xdg-base-directory" XDG.BaseDirectory.Internal as BaseDir
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import "this" XDG.UserDirectory.Common (desktop)
import "this" XDG.UserDirectory.Parser (DirectoryValue (Absolute, HomeRelative))
import "this" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
    UserDirsConfig,
    envVarName,
  )

-- | Errors that can occur when looking up a user directory.
data LookupError
  = -- | The requested directory was not found in the config.
    DirectoryNotConfigured UserDirectory
  | -- | Could not determine the home directory.
    HomeDirectoryError BaseDir.Error
  | -- | A path error occurred (e.g., relative path where absolute expected).
    PathError BaseDir.Error
  | InvalidPath (MP.ParseErrorBundle String String)
  | NotRelativeDir
  deriving stock (Eq, Generic, Show)

-- | Look up a user directory.
--
--   Returns the configured path for the given user directory type. If the
--   directory is not configured, returns the fallback (which is @$HOME@ for
--   most directories, or @$HOME/Desktop@ for 'desktop').
--
--   The fallback behavior follows the reference implementation:
--
--   - 'desktop' falls back to @$HOME/Desktop@
--   - All other directories fall back to @$HOME@
getUserDirectory ::
  (IsString rep, System.Rep rep) =>
  UserDirectory ->
  UserDirsConfig rep ->
  IO (Either BaseDir.Error (Path 'Abs 'Dir rep))
getUserDirectory dir =
  resolveDirectoryValue . Map.findWithDefault (defaultDir dir) dir

-- | Look up all configured user directories.
--
--   Returns a map of all user directories to their resolved paths. Directories
--   that are not configured or failed to resolve are omitted.
getAllUserDirectories ::
  (System.Rep rep) =>
  UserDirsConfig rep ->
  IO (Either BaseDir.Error (Map.Map UserDirectory (Path 'Abs 'Dir rep)))
getAllUserDirectories = resolveDirectoryValues

-- | Resolve a directory value to an absolute path.
resolveDirectoryValue ::
  (System.Rep rep) =>
  DirectoryValue rep -> IO (Either BaseDir.Error (Path 'Abs 'Dir rep))
resolveDirectoryValue =
  fmap (runIdentity <$>) . resolveDirectoryValues . Identity

-- |
--
--   Any failure here is due to being unable to resolve the user’s home
--   directory.
resolveDirectoryValues ::
  (Functor f, System.Rep rep) =>
  f (DirectoryValue rep) -> IO (Either BaseDir.Error (f (Path 'Abs 'Dir rep)))
resolveDirectoryValues dirs =
  fmap
    ( fmap
        ( \home ->
            fmap
              ( \case
                  HomeRelative rel -> home </> rel
                  Absolute abs -> abs
              )
              dirs
        )
    )
    BaseDir.getHomeDirectory

defaultDir :: (IsString rep) => UserDirectory -> DirectoryValue rep
defaultDir dir =
  HomeRelative
    if dir == desktop
      then [posix|Desktop/|]
      else Dir.current
