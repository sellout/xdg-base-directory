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
import "base" Control.Monad ((=<<))
import "base" Data.Bool (Bool (False))
import "base" Data.Either (Either (Left), either)
import "base" Data.Eq (Eq, (==))
import "base" Data.Function (const, ($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (maybe)
import "base" Data.String (String)
import "base" Data.Traversable (traverse)
import "base" GHC.Generics (Generic)
import "base" System.IO (IO)
import "base" Text.Show (Show)
import qualified "containers" Data.Map.Strict as Map
import qualified "megaparsec" Text.Megaparsec as MP
import "pathway" Data.Path (Path, Relativity (Abs, Rel), Type (Dir), anchor, (</>))
import qualified "pathway" Data.Path as Path
import qualified "pathway" Data.Path.Format as Format
import qualified "pathway" Data.Path.Parser as Parser
import "pathway" Data.Path.TH (posix)
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
  UserDirectory -> IO (Either LookupError (Path 'Abs 'Dir String))
getUserDirectory dir =
  either
    (fallback dir . pure . Left . ConfigLoadError)
    ( maybe
        (fallback dir . pure . Left $ DirectoryNotConfigured dir)
        resolveDirectoryValue
        . Map.lookup dir
    )
    =<< loadConfig

-- | Look up all configured user directories.
--
--   Returns a map of all user directories to their resolved paths. Directories
--   that are not configured or failed to resolve are omitted.
getAllUserDirectories ::
  IO (Map.Map UserDirectory (Either LookupError (Path 'Abs 'Dir String)))
getAllUserDirectories =
  either
    ( \err ->
        -- TODO: This case should return a single error, not an error for some
        --       arbitrary set of directories.
        pure . Map.fromList $
          (,Left $ ConfigLoadError err) <$> wellKnownDirectories
    )
    ( \config ->
        let lookupDir d =
              maybe
                (pure . Left $ DirectoryNotConfigured d)
                resolveDirectoryValue
                $ Map.lookup d config
         in Map.fromList
              <$> traverse (\d -> (,) d <$> lookupDir d) wellKnownDirectories
    )
    =<< loadConfig

-- | Resolve a directory value to an absolute path.
resolveDirectoryValue ::
  DirectoryValue -> IO (Either LookupError (Path 'Abs 'Dir String))
resolveDirectoryValue = \case
  HomeRelative relPath ->
    either
      (Left . PathError)
      -- Parse the relative path and combine with home
      (\home -> (home </>) <$> parseRelativeDir relPath)
      <$> getHomeDirectory
  Absolute absPath -> do
    rep <- System.fromStringLiteral absPath
    let parsed = Patch.parseDirectory rep
    case Patch.anchorType parsed of
      Patch.Abs abs -> pure $ pure abs
      Patch.Rel _ -> pure . Left $ PathError RelativeDirectory
      Patch.Reparented _ -> pure . Left $ PathError RelativeDirectory

-- | Parse a relative directory path from a string.
--
--   This uses 'Directory.descendTo' to build the path component by component.
parseRelativeDir ::
  String -> Either LookupError (Path ('Rel 'False) 'Dir String)
parseRelativeDir =
  either
    (Left . InvalidPath)
    ( ( \case
          Path.RelDir rd -> pure rd
          _ -> Left NotRelativeDir
      )
        . anchor
        . Path.forgetType
    )
    . MP.parse (Parser.directory Format.posix) ""

-- | Fallback for unconfigured directories.
--
--   Per the reference implementation:
--   - Desktop falls back to @$HOME/Desktop@
--   - All other directories fall back to @$HOME@
fallback ::
  (System.Rep rep) =>
  UserDirectory ->
  IO (Either LookupError (Path 'Abs 'Dir rep)) ->
  IO (Either LookupError (Path 'Abs 'Dir rep))
fallback dir onError =
  either
    (const onError)
    ( \home ->
        if dir == desktop
          then
            pure . (home </>)
              <$> traverse System.fromStringLiteral [posix|Desktop/|]
          else pure $ pure home
    )
    =<< getHomeDirectory
