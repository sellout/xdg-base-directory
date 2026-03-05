{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- The XDG base directory specification defines most directories as living under
-- @$HOME@, but there are a few reasons to expose them in a relative way
--
-- - this doesn’t involve `System.IO.IO` and
--
-- - the XDG categorization of directories is useful on its own, and can be
--   applied in other areas.
module XDG.BaseDirectory.Default.Relative
  ( dataHome,
    configHome,
    stateHome,
    cacheHome,
    binHome,
  )
where

import "base" Data.Bool (Bool (False))
import "base" Data.String (IsString)
import "pathway" Data.Path (Path, Relativity (Rel), Type (Dir))
import "pathway" Data.Path.TH (posix)

-- | Default relative path for user-specific data files: @.local\/share\/@.
--
-- @since 0.0.1.0
dataHome :: (IsString rep) => Path ('Rel 'False) 'Dir rep
dataHome = [posix|.local/share/|]

-- | Default relative path for user-specific configuration files: @.config\/@.
--
-- @since 0.0.1.0
configHome :: (IsString rep) => Path ('Rel 'False) 'Dir rep
configHome = [posix|.config/|]

-- | Default relative path for user-specific state data: @.local\/state\/@.
--
-- @since 0.0.1.0
stateHome :: (IsString rep) => Path ('Rel 'False) 'Dir rep
stateHome = [posix|.local/state/|]

-- | Default relative path for user-specific cache files: @.cache\/@.
--
-- @since 0.0.1.0
cacheHome :: (IsString rep) => Path ('Rel 'False) 'Dir rep
cacheHome = [posix|.cache/|]

-- | Default relative path for user-specific executables: @.local\/bin\/@.
--
-- @since 0.0.1.0
binHome :: (IsString rep) => Path ('Rel 'False) 'Dir rep
binHome = [posix|.local/bin/|]
