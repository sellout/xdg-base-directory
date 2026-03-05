{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
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

    -- * Well-known directories
    desktop,
    download,
    templates,
    publicShare,
    documents,
    music,
    pictures,
    videos,
    wellKnownDirectories,

    -- * Re-exports
    envVarName,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False))
import "base" Data.Either (Either (Left, Right))
import "base" Data.Eq (Eq, (==))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.String (String)
import "base" Data.Traversable (mapM)
import "base" GHC.Generics (Generic)
import qualified "base" System.IO as IO
import "base" Text.Show (Show)
import qualified "containers" Data.Map.Strict as Map
import "pathway" Data.Path (Path, Relativity (Abs, Rel), Type (Dir), (</>))
import qualified "pathway" Data.Path.Directory as Directory
import "xdg-base-directory" XDG.BaseDirectory.Internal
  ( Error (RelativeDirectory),
    getHomeDirectory,
  )
import qualified "xdg-base-directory" XDG.BaseDirectory.Internal as BaseDir
import qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import "this" XDG.UserDirectory.Config (ConfigError, loadConfig)
import "this" XDG.UserDirectory.Parser (DirectoryValue (Absolute, HomeRelative))
import "this" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
    desktop,
    documents,
    download,
    envVarName,
    music,
    pictures,
    publicShare,
    templates,
    videos,
    wellKnownDirectories,
  )

-- | Errors that can occur when looking up a user directory.
data LookupError
  = -- | Could not load the config file.
    ConfigLoadError ConfigError
  | -- | The requested directory was not found in the config.
    DirectoryNotConfigured UserDirectory
  | -- | Could not determine the home directory.
    HomeDirectoryError BaseDir.Error
  | -- | A path error occurred (e.g., relative path where absolute expected).
    PathError BaseDir.Error
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
  (System.Rep rep) =>
  UserDirectory ->
  IO.IO (Either LookupError (Path 'Abs 'Dir rep))
getUserDirectory dir = do
  configResult <- loadConfig
  case configResult of
    Left err -> fallback dir (pure . Left $ ConfigLoadError err)
    Right config ->
      case Map.lookup dir config of
        Nothing -> fallback dir (pure $ Left $ DirectoryNotConfigured dir)
        Just value -> resolveDirectoryValue value

-- | Look up all configured user directories.
--
--   Returns a map of all user directories to their resolved paths. Directories
--   that are not configured or failed to resolve are omitted.
getAllUserDirectories ::
  (System.Rep rep) =>
  IO.IO (Map.Map UserDirectory (Either LookupError (Path 'Abs 'Dir rep)))
getAllUserDirectories = do
  configResult <- loadConfig
  case configResult of
    Left err -> pure $ Map.fromList [(d, Left $ ConfigLoadError err) | d <- wellKnownDirectories]
    Right config -> do
      let lookupDir d = case Map.lookup d config of
            Nothing -> pure $ Left $ DirectoryNotConfigured d
            Just value -> resolveDirectoryValue value
      results <- mapM (\d -> (,) d <$> lookupDir d) wellKnownDirectories
      pure $ Map.fromList results

-- | Resolve a directory value to an absolute path.
resolveDirectoryValue ::
  (System.Rep rep) =>
  DirectoryValue ->
  IO.IO (Either LookupError (Path 'Abs 'Dir rep))
resolveDirectoryValue value = case value of
  HomeRelative relPath -> do
    homeResult <- getHomeDirectory
    case homeResult of
      Left err -> pure . Left $ PathError err
      Right home -> do
        -- Parse the relative path and combine with home
        relDir <- parseRelativeDir relPath
        pure $ Right $ home </> relDir
  Absolute absPath -> do
    rep <- System.fromStringLiteral absPath
    let parsed = Patch.parseDirectory rep
    case Patch.anchorType parsed of
      Patch.Abs abs -> pure $ Right abs
      Patch.Rel _ -> pure . Left $ PathError RelativeDirectory
      Patch.Reparented _ -> pure . Left $ PathError RelativeDirectory

-- | Parse a relative directory path from a string.
--
--   This uses 'Directory.descendTo' to build the path component by component.
parseRelativeDir ::
  (System.Rep rep) =>
  String ->
  IO.IO (Path ('Rel 'False) 'Dir rep)
parseRelativeDir relPath = do
  rep <- System.fromStringLiteral relPath
  let components = System.splitDirectories rep
  -- Build the relative path by descending through each component
  buildRelativeDir components

-- | Build a relative directory path from a list of components.
buildRelativeDir ::
  [rep] ->
  IO.IO (Path ('Rel 'False) 'Dir rep)
buildRelativeDir components =
  pure $ foldComponents Directory.current components
  where
    foldComponents base [] = base
    foldComponents base (c : cs) = foldComponents (Directory.descendTo base c) cs

-- | Fallback for unconfigured directories.
--
--   Per the reference implementation:
--   - Desktop falls back to @$HOME/Desktop@
--   - All other directories fall back to @$HOME@
fallback ::
  (System.Rep rep) =>
  UserDirectory ->
  IO.IO (Either LookupError (Path 'Abs 'Dir rep)) ->
  IO.IO (Either LookupError (Path 'Abs 'Dir rep))
fallback dir onError = do
  homeResult <- getHomeDirectory
  case homeResult of
    Left _ -> onError
    Right home ->
      if dir == desktop
        then do
          desktopDir <- parseRelativeDir "Desktop"
          pure $ Right $ home </> desktopDir
        else pure $ Right home
