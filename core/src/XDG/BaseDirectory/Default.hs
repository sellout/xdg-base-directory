{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- The default XDG base directory values that are used for fallbacks when other
-- values aren’t provided.
module XDG.BaseDirectory.Default
  ( dataHome,
    configHome,
    stateHome,
    dataDirs,
    configDirs,
    cacheHome,
    binHome,

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
import qualified "pathway-system" Filesystem.Path as Dir
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import qualified "this" XDG.BaseDirectory.Default.Relative as Relative
import "this" XDG.BaseDirectory.Internal
  ( BaseDirectory,
    Error,
    getHomeDirectory,
  )

pinHome ::
  Path ('Rel 'False) typ Dir.PathComponent ->
  IO (Either (Error Dir.PathComponent) (Path 'Abs typ Dir.PathComponent))
pinHome rel = fmap (</> rel) <$> getHomeDirectory

dataHome,
  configHome,
  stateHome,
  cacheHome,
  binHome ::
    IO (Either (Error Dir.PathComponent) (BaseDirectory Dir.PathComponent))

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

-- |
--
--       There is a single base directory relative to which user-specific
--       executable files may be written.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
--
--       User-specific executable files may be stored in @$HOME@/.local/bin.
--       Distributions should ensure this directory shows up in the UNIX @$PATH@
--       environment variable, at an appropriate place.
--
--       Since @$HOME@ might be shared between systems of different
--       architectures, installing compiled binaries to @$HOME@/.local/bin could
--       cause problems when used on systems of differing architectures. This is
--       often not a problem, but the fact that @$HOME@ becomes partially
--       architecture-specific if compiled binaries are placed in it should be
--       kept in mind.
--
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
binHome = pinHome Relative.binHome

dataDirs, configDirs :: (System.Rep rep) => NonEmpty (BaseDirectory rep)

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

-- |
--
--     - Such file should be installed to @$datadir@/subdir/filename with
--       @$datadir@ defaulting to /usr/share.
--
--       —[§4](https://specifications.freedesktop.org/basedir-spec/latest/#referencing)
datadir = System.fromStringLiteral <$> [posix|/usr/share/|]

-- |
--
--     - Default configuration files should be installed to
--       @$sysconfdir@/xdg/subdir/filename with @$sysconfdir@ defaulting to
--       /etc.
--
--       —[§4](https://specifications.freedesktop.org/basedir-spec/latest/#referencing)
sysconfdir = System.fromStringLiteral <$> [posix|/etc/|]
