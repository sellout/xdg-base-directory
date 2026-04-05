{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- The default XDG base directory values that are used for fallbacks when other
-- values aren’t provided.
module XDG.BaseDirectory.Default
  ( BaseDirectory,
    dataHome,
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

import safe "base" Control.Applicative (liftA2, pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool (False))
import safe "base" Data.Either (Either)
import safe "base" Data.Functor (fmap)
import safe "base" Data.List.NonEmpty (NonEmpty ((:|)))
import safe "base" Data.String (IsString, String)
import safe "base" Data.Traversable (Traversable, traverse)
import safe "base" System.IO (IO)
import safe "pathway" Data.Path (Filename, Path, Relativity (Abs, Rel), (</>))
import safe "pathway" Data.Path.TH (posix)
import safe qualified "pathway-system" System.Path as Path
import safe qualified "pathway-system" System.Text as Text
import "variant" Data.Variant (V)
import safe qualified "this" XDG.BaseDirectory.Default.Relative as Relative
import safe "this" XDG.BaseDirectory.Internal (BaseDirectory)

pinHome ::
  (Traversable (Filename typ), Path.Rep rep, Text.Rep rep) =>
  Path ('Rel 'False) typ String ->
  IO (Either (V (Path.GetUserDirectoryFailure rep)) (Path 'Abs typ rep))
pinHome =
  liftA2 (\h r -> fmap (</> r) h) Path.getHomeDirectory
    . traverse Text.encodeString

dataHome,
  configHome,
  stateHome,
  cacheHome,
  binHome ::
    (Path.Rep rep, Text.Rep rep) =>
    -- | This should only fail if there is an issue looking up the @$HOME@
    --   directory.
    IO (Either (V (Path.GetUserDirectoryFailure rep)) (BaseDirectory rep))

-- |
--
--       If @$XDG_DATA_HOME@ is either not set or empty, a default equal to
--       @$HOME@/.local/share should be used.”
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
dataHome = pinHome Relative.dataHome

-- |
--
--       If @$XDG_CONFIG_HOME@ is either not set or empty, a default equal to
--       @$HOME@/.config should be used.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
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

dataDirs, configDirs :: (IsString rep) => NonEmpty (BaseDirectory rep)

-- |
--
--        If @$XDG_DATA_DIRS@ is either not set or empty, a value equal to
--        /usr/local/share/:/usr/share/ should be used.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
dataDirs = [posix|/usr/local/share/|] :| [[posix|/usr/share/|]]

-- |
--
--        If @$XDG_CONFIG_DIRS@ is either not set or empty, a value equal to
--        /etc/xdg should be used.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
configDirs = pure [posix|/etc/xdg/|]

datadir, sysconfdir :: (IsString rep) => BaseDirectory rep

-- |
--
--     - Such file should be installed to @$datadir@/subdir/filename with
--       @$datadir@ defaulting to /usr/share.
--
--       —[§4](https://specifications.freedesktop.org/basedir-spec/latest/#referencing)
datadir = [posix|/usr/share/|]

-- |
--
--     - Default configuration files should be installed to
--       @$sysconfdir@/xdg/subdir/filename with @$sysconfdir@ defaulting to
--       /etc.
--
--       —[§4](https://specifications.freedesktop.org/basedir-spec/latest/#referencing)
sysconfdir = [posix|/etc/|]
