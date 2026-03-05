{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- The well-known user directory types from the xdg-user-dirs specification.
-- See <https://www.freedesktop.org/wiki/Software/xdg-user-dirs/>.
module XDG.UserDirectory.Type
  ( UserDirectory (..),
    envVarName,

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
  )
where

import "base" Data.Eq (Eq)
import "base" Data.Ord (Ord)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" GHC.Generics (Generic)
import "base" Text.Read (Read)
import "base" Text.Show (Show)

-- | A user directory type from the xdg-user-dirs specification.
--
--   The spec defines a set of well-known directories, but the format is
--   extensible. Use the provided constants ('desktop', 'download', etc.) for
--   standard directories, or construct a 'UserDirectory' directly for custom
--   ones.
--
--   The string should be the middle part of the variable name, e.g.,
--   @\"DESKTOP\"@ for @XDG_DESKTOP_DIR@.
newtype UserDirectory = UserDirectory String
  deriving stock (Eq, Generic, Ord, Read, Show)

-- | Get the environment variable name for a user directory type.
--
--   This returns the variable name as it appears in @user-dirs.dirs@, e.g.,
--   @\"XDG_DESKTOP_DIR\"@ for 'desktop'.
envVarName :: UserDirectory -> String
envVarName (UserDirectory name) = "XDG_" <> name <> "_DIR"

-- | @XDG_DESKTOP_DIR@ - The user\'s desktop directory
desktop :: UserDirectory
desktop = UserDirectory "DESKTOP"

-- | @XDG_DOWNLOAD_DIR@ - The user\'s downloads directory
download :: UserDirectory
download = UserDirectory "DOWNLOAD"

-- | @XDG_TEMPLATES_DIR@ - The user\'s templates directory
templates :: UserDirectory
templates = UserDirectory "TEMPLATES"

-- | @XDG_PUBLICSHARE_DIR@ - The user\'s public share directory
publicShare :: UserDirectory
publicShare = UserDirectory "PUBLICSHARE"

-- | @XDG_DOCUMENTS_DIR@ - The user\'s documents directory
documents :: UserDirectory
documents = UserDirectory "DOCUMENTS"

-- | @XDG_MUSIC_DIR@ - The user\'s music directory
music :: UserDirectory
music = UserDirectory "MUSIC"

-- | @XDG_PICTURES_DIR@ - The user\'s pictures directory
pictures :: UserDirectory
pictures = UserDirectory "PICTURES"

-- | @XDG_VIDEOS_DIR@ - The user\'s videos directory
videos :: UserDirectory
videos = UserDirectory "VIDEOS"

-- | All well-known user directory types from the spec.
wellKnownDirectories :: [UserDirectory]
wellKnownDirectories =
  [desktop, download, templates, publicShare, documents, music, pictures, videos]
