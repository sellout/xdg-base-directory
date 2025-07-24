{-# LANGUAGE Safe #-}

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

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Monad ((<=<))
import "base" Data.Bifunctor (first)
import "base" Data.Either (Either)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import "base" Data.Maybe (maybe)
import "base" System.IO (IO)
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir))
import "these" Data.These (These (This), partitionEithersNE)
import qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import "this" XDG.BaseDirectory.Internal
  ( BaseDirectory,
    Error (NoDirectoriesFound, Var),
    extractAbs,
    weakenEither,
  )
import qualified "this" XDG.BaseDirectory.Var as Var

parseDir :: (System.Rep rep) => rep -> Either (Error rep) (BaseDirectory rep)
parseDir = extractAbs . Patch.parseDirectory

get ::
  (System.Rep rep) =>
  Var.EnvironmentVariable rep ->
  IO (Either (Error rep) (BaseDirectory rep))
get = fmap (parseDir <=< first Var) . Var.lookupNonEmptyEnv

getMultiple ::
  (System.Rep rep) =>
  Var.EnvironmentVariable rep ->
  IO (These (NonEmpty (Error rep)) (NonEmpty (BaseDirectory rep)))
getMultiple =
  fmap (maybe (This $ pure NoDirectoriesFound) (partitionEithersNE . fmap parseDir) . nonEmpty . System.splitSearchPath <=< weakenEither . first (pure . Var))
    . Var.lookupNonEmptyEnv

dataHome,
  configHome,
  stateHome,
  cacheHome ::
    (System.Rep rep) => IO (Either (Error rep) (BaseDirectory rep))

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
    IO (These (NonEmpty (Error rep)) (NonEmpty (BaseDirectory rep)))

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
runtimeDir :: (System.Rep rep) => IO (Either (Error rep) (BaseDirectory rep))
runtimeDir = get Var.runtimeDir

datadir,
  sysconfdir ::
    (System.Rep rep) => IO (Either (Error rep) (Path 'Abs 'Dir rep))
datadir = get Var.datadir
sysconfdir = get Var.sysconfdir
