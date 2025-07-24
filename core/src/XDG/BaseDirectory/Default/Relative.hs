{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module XDG.BaseDirectory.Default.Relative
  ( dataHome,
    configHome,
    stateHome,
    cacheHome,
  )
where

import "base" Data.Bool (Bool (False))
import "base" Data.Functor ((<$>))
import "pathway" Data.Path (Path, Relativity (Rel), Type (Dir))
import "pathway" Data.Path.TH (posix)
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System

dataHome, configHome, stateHome, cacheHome :: (System.Rep rep) => Path ('Rel 'False) 'Dir rep
dataHome = System.fromStringLiteral <$> [posix|.local/share/|]
configHome = System.fromStringLiteral <$> [posix|.config/|]
stateHome = System.fromStringLiteral <$> [posix|.local/state/|]
cacheHome = System.fromStringLiteral <$> [posix|.cache/|]
