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

import safe "base" Control.Category ((.))
import safe "base" Data.Either (Either)
import safe "base" Data.Eq ((==))
import safe "base" Data.Functor (Functor, fmap, (<$>))
import safe "base" Data.Functor.Identity (Identity (Identity), runIdentity)
import safe qualified "base" Data.Kind as Kind
import safe "base" Data.String (IsString, String)
import safe "base" GHC.Generics (Generic)
import safe "base" System.IO (IO)
import safe qualified "containers" Data.Map.Strict as Map
import safe qualified "megaparsec" Text.Megaparsec as MP
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (Dir), (</>))
import safe qualified "pathway" Data.Path.Directory as Dir
import safe "pathway" Data.Path.TH (posix)
import safe qualified "pathway-system" System.Path as Path
import "variant" Data.Variant (V)
import safe qualified "xdg-base-directory" XDG.BaseDirectory.Internal as BaseDir
import safe "this" XDG.UserDirectory.Common (desktop)
import safe "this" XDG.UserDirectory.Parser (DirectoryValue (Absolute, HomeRelative))
import safe "this" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
    UserDirsConfig,
    envVarName,
  )

-- | Errors that can occur when looking up a user directory.
--
--  __TODO__: Restore `Eq` and `Show` instances.
data LookupError (rep :: Kind.Type)
  = -- | The requested directory was not found in the config.
    DirectoryNotConfigured UserDirectory
  | -- | Could not determine the home directory.
    HomeDirectoryError (BaseDir.Error rep)
  | -- | A path error occurred (e.g., relative path where absolute expected).
    PathError (BaseDir.Error rep)
  | InvalidPath (MP.ParseErrorBundle String String)
  | NotRelativeDir
  deriving stock (Generic)

type role LookupError nominal

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
  (IsString rep, Path.Rep rep) =>
  UserDirectory ->
  UserDirsConfig rep ->
  IO (Either (V (Path.GetUserDirectoryFailure rep)) (Path 'Abs 'Dir rep))
getUserDirectory dir =
  resolveDirectoryValue . Map.findWithDefault (defaultDir dir) dir

-- | Look up all configured user directories.
--
--   Returns a map of all user directories to their resolved paths. Directories
--   that are not configured or failed to resolve are omitted.
getAllUserDirectories ::
  (Path.Rep rep) =>
  UserDirsConfig rep ->
  IO
    ( Either
        (V (Path.GetUserDirectoryFailure rep))
        (Map.Map UserDirectory (Path 'Abs 'Dir rep))
    )
getAllUserDirectories = resolveDirectoryValues

-- | Resolve a directory value to an absolute path.
resolveDirectoryValue ::
  (Path.Rep rep) =>
  DirectoryValue rep ->
  IO (Either (V (Path.GetUserDirectoryFailure rep)) (Path 'Abs 'Dir rep))
resolveDirectoryValue =
  fmap (runIdentity <$>) . resolveDirectoryValues . Identity

-- |
--
--   Any failure here is due to being unable to resolve the user’s home
--   directory.
resolveDirectoryValues ::
  (Functor f, Path.Rep rep) =>
  f (DirectoryValue rep) ->
  IO (Either (V (Path.GetUserDirectoryFailure rep)) (f (Path 'Abs 'Dir rep)))
resolveDirectoryValues dirs =
  fmap
    ( \home ->
        fmap
          ( \case
              HomeRelative rel -> home </> rel
              Absolute abs -> abs
          )
          dirs
    )
    <$> Path.getHomeDirectory

defaultDir :: (IsString rep) => UserDirectory -> DirectoryValue rep
defaultDir dir =
  HomeRelative
    if dir == desktop
      then [posix|Desktop/|]
      else Dir.current
