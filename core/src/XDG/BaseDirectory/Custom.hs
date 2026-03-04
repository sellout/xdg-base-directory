{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Values retrieved from the variables defined in "XDG.BaseDirectory.Var" or
-- [GNU Make
-- variables](https://www.gnu.org/prep/standards/html_node/Directory-Variables.html)
-- (which [Cabal allows you to
-- override](https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#accessing-data-files-from-package-code)).
module XDG.BaseDirectory.Custom
  ( dataHome,
    configHome,
    stateHome,
    cacheHome,
    dataDirs,
    configDirs,
    runtimeDir,

    -- * make vars
    datadir,
    sysconfdir,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<), (=<<))
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Either (Either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import safe "base" Data.Maybe (maybe)
import safe "base" System.IO (IO)
import safe "these" Data.These (These (This), partitionEithersNE)
import safe qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
import safe qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import qualified "this" Paths_xdg_base_directory as Make
import safe "this" XDG.BaseDirectory.Internal
  ( BaseDirectory,
    Error (NoDirectoriesFound, Var),
    extractAbs,
    weakenEither,
  )
import safe qualified "this" XDG.BaseDirectory.Var as Var

parseDir :: (System.Rep rep) => rep -> Either Error (BaseDirectory rep)
parseDir = extractAbs . Patch.parseDirectory

get ::
  (System.Rep rep) =>
  Var.EnvironmentVariable -> IO (Either Error (BaseDirectory rep))
get = fmap (parseDir <=< first Var) . Var.lookupNonEmptyEnv

getMultiple ::
  (System.Rep rep) =>
  Var.EnvironmentVariable ->
  IO (These (NonEmpty Error) (NonEmpty (BaseDirectory rep)))
getMultiple =
  fmap
    ( maybe (This $ pure NoDirectoriesFound) (partitionEithersNE . fmap parseDir)
        . nonEmpty
        . System.splitSearchPath
        <=< weakenEither . first (pure . Var)
    )
    . Var.lookupNonEmptyEnv

dataHome,
  configHome,
  stateHome,
  cacheHome ::
    (System.Rep rep) => IO (Either Error (BaseDirectory rep))

-- |
--
--       There is a single base directory relative to which user-specific data
--       files should be written. This directory is defined by the environment
--       variable @$XDG_DATA_HOME@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
dataHome = get Var.dataHome

-- |
--
--       There is a single base directory relative to which user-specific
--       configuration files should be written. This directory is defined by the
--       environment variable @$XDG_CONFIG_HOME@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
configHome = get Var.configHome

-- |
--
--       There is a single base directory relative to which user-specific state
--       data should be written. This directory is defined by the environment
--       variable @$XDG_STATE_HOME@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
stateHome = get Var.stateHome

-- |
--
--       There is a single base directory relative to which user-specific
--       non-essential (cached) data should be written. This directory is
--       defined by the environment variable @$XDG_CACHE_HOME@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
cacheHome = get Var.cacheHome

dataDirs,
  configDirs ::
    (System.Rep rep) =>
    IO (These (NonEmpty Error) (NonEmpty (BaseDirectory rep)))

-- |
--
--        There is a set of preference ordered base directories relative to
--       which data files should be searched. This set of directories is defined
--       by the environment variable @$XDG_DATA_DIRS@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
dataDirs = getMultiple Var.dataDirs

-- |
--
--        There is a set of preference ordered base directories relative to
--       which configuration files should be searched. This set of directories
--       is defined by the environment variable @$XDG_CONFIG_DIRS@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
configDirs = getMultiple Var.configDirs

-- |
--
--        There is a single base directory relative to which user-specific
--       runtime files and other file objects should be placed. This directory
--       is defined by the environment variable @$XDG_RUNTIME_DIR@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
runtimeDir :: (System.Rep rep) => IO (Either Error (BaseDirectory rep))
runtimeDir = get Var.runtimeDir

-- |
--
--       Other specifications may reference this specification by specifying the
--       location of a data file as @$XDG_DATA_DIRS@/subdir/filename. This
--       implies that:
--
--     - Such file should be installed to @$datadir@/subdir/filename with
--       @$datadir@ defaulting to /usr/share.
--
--       —[§4](https://specifications.freedesktop.org/basedir-spec/latest/#referencing)
datadir :: (System.Rep rep) => IO (Either Error (BaseDirectory rep))
datadir = fmap parseDir . System.fromStringLiteral =<< Make.getDataDir

-- |
--
--       Specifications may reference this specification by specifying the
--       location of a configuration file as @$XDG_CONFIG_DIRS@/subdir/filename.
--       This implies that:
--
--     - Default configuration files should be installed to
--       @$sysconfdir@/xdg/subdir/filename with @$sysconfdir@ defaulting to
--       /etc.
--
--       —[§4](https://specifications.freedesktop.org/basedir-spec/latest/#referencing)
sysconfdir :: (System.Rep rep) => IO (Either Error (BaseDirectory rep))
sysconfdir = fmap parseDir . System.fromStringLiteral =<< Make.getSysconfDir
