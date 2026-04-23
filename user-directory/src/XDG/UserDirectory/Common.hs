{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- The user directories defined in the default defaults for xdg-user-dirs.
module XDG.UserDirectory.Common
  ( desktop,
    download,
    templates,
    publicshare,
    documents,
    music,
    pictures,
    videos,
  )
where

import "this" XDG.UserDirectory.Type (UserDirectory (UserDirectory))

desktop,
  download,
  templates,
  publicshare,
  documents,
  music,
  pictures,
  videos ::
    UserDirectory

-- | @XDG_DESKTOP_DIR@ - The user’s desktop directory
--
-- @since 0.0.1.0
desktop = UserDirectory "DESKTOP"

-- | @XDG_DOWNLOAD_DIR@ - The user’s downloads directory
--
-- @since 0.0.1.0
download = UserDirectory "DOWNLOAD"

-- | @XDG_TEMPLATES_DIR@ - The user’s templates directory
--
-- @since 0.0.1.0
templates = UserDirectory "TEMPLATES"

-- | @XDG_PUBLICSHARE_DIR@ - The user’s public share directory
--
-- @since 0.0.1.0
publicshare = UserDirectory "PUBLICSHARE"

-- | @XDG_DOCUMENTS_DIR@ - The user’s documents directory
--
-- @since 0.0.1.0
documents = UserDirectory "DOCUMENTS"

-- | @XDG_MUSIC_DIR@ - The user’s music directory
--
-- @since 0.0.1.0
music = UserDirectory "MUSIC"

-- | @XDG_PICTURES_DIR@ - The user’s pictures directory
--
-- @since 0.0.1.0
pictures = UserDirectory "PICTURES"

-- | @XDG_VIDEOS_DIR@ - The user’s videos directory
--
-- @since 0.0.1.0
videos = UserDirectory "VIDEOS"
