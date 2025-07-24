{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Safe #-}

module XDG.BaseDirectory.Default
  ( dataHome,
    configHome,
    stateHome,
    dataDirs,
    configDirs,
    cacheHome,

    -- * make vars
    datadir,
    sysconfdir,
  )
where

import "base" Data.Bool (Bool (False))
import "base" Data.Either (Either)
import "base" Data.Functor (fmap, (<$>))
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" System.IO (IO)
import "pathway" Data.Path (Path, Relativity (Abs, Rel), Type (Dir), (</>))
import "pathway" Data.Path.TH (posix)
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import qualified "this" XDG.BaseDirectory.Default.Relative as Relative
import "this" XDG.BaseDirectory.Internal (Error, getHomeDirectory)

pinHome :: (System.Rep rep) => Path ('Rel 'False) typ rep -> IO (Either (Error rep) (Path 'Abs typ rep))
pinHome rel = fmap (</> rel) <$> getHomeDirectory

dataHome, configHome, stateHome, cacheHome :: (System.Rep rep) => IO (Either (Error rep) (Path 'Abs 'Dir rep))

-- |
--
--       If @$XDG_DATA_HOME@ is either not set or empty, a default equal to
--       @$HOME@/.local/share should be used.”
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
dataHome = pinHome Relative.dataHome

-- |
--
--       If @$XDG_CONFIG_HOME@ is either not set or empty, a default equal to @$HOME@/.config should be used.
configHome = pinHome Relative.configHome

stateHome = pinHome Relative.stateHome

cacheHome = pinHome Relative.cacheHome

dataDirs, configDirs :: (System.Rep rep) => NonEmpty (Path 'Abs 'Dir rep)

-- |
--
--        If @$XDG_DATA_DIRS@ is either not set or empty, a value equal to
--        /usr/local/share/:/usr/share/ should be used.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
dataDirs = fmap System.fromStringLiteral <$> [posix|/usr/local/share/|] :| [[posix|/usr/share/|]]

-- |
--
--        If @$XDG_CONFIG_DIRS@ is either not set or empty, a value equal to
--        /etc/xdg should be used.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
configDirs = fmap System.fromStringLiteral <$> [posix|/etc/xdg/|] :| []

datadir, sysconfdir :: (System.Rep rep) => Path 'Abs 'Dir rep
datadir = System.fromStringLiteral <$> [posix|/usr/share/|]
sysconfdir = System.fromStringLiteral <$> [posix|/etc/|]
