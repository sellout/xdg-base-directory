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
import "base" Data.Functor ((<$>))
import "pathway" Data.Path (Path, Relativity (Rel), Type (Dir))
import "pathway" Data.Path.TH (posix)
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System

dataHome,
  configHome,
  stateHome,
  cacheHome,
  binHome ::
    (System.Rep rep) => Path ('Rel 'False) 'Dir rep
dataHome = System.fromStringLiteral <$> [posix|.local/share/|]
configHome = System.fromStringLiteral <$> [posix|.config/|]
stateHome = System.fromStringLiteral <$> [posix|.local/state/|]
cacheHome = System.fromStringLiteral <$> [posix|.cache/|]
binHome = System.fromStringLiteral <$> [posix|.local/bin/|]
