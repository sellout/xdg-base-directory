{-# LANGUAGE Safe #-}

-- |
--
--        All paths set in these environment variables must be absolute. If an
--        implementation encounters a relative path in any of these variables it
--        should consider the path invalid and ignore it.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
module XDG.BaseDirectory.Var
  ( EnvironmentVariable,
    lookupNonEmptyEnv,

    -- * test
    dataHome,
    configHome,
    stateHome,
    dataDirs,
    configDirs,
    cacheHome,
    runtimeDir,

    -- * make vars
    datadir,
    sysconfdir,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Monad ((<=<))
import "base" Data.Either (Either (Left))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe (Nothing))
import "base" Data.String (String)
import "base" System.IO (IO)
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import "this" XDG.BaseDirectory.Internal (VarError (EmptyVar, MissingVar), note)

newtype EnvironmentVariable rep = EnvVar rep

envVar :: (System.Rep rep) => String -> EnvironmentVariable rep
envVar = EnvVar . System.fromStringLiteral

-- | The spec says “If [a variable] is either not set or empty, a default […]
--   should be used.” This makes sure we handle all environment variables that
--   way.
lookupNonEmptyEnv ::
  (System.Rep rep) => EnvironmentVariable rep -> IO (Either (VarError rep) rep)
lookupNonEmptyEnv (EnvVar varName) =
  ( (\val -> if System.isValid val then Left $ EmptyVar varName else pure val)
      <=< note (MissingVar varName Nothing)
  )
    <$> System.lookupEnv varName

dataHome, configHome, stateHome, dataDirs, configDirs, cacheHome, runtimeDir :: (System.Rep rep) => EnvironmentVariable rep

-- |
--
--       There is a single base directory relative to which user-specific data
--       files should be written. This directory is defined by the environment
--       variable @$XDG_DATA_HOME@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
--
--       @$XDG_DATA_HOME@ defines the base directory relative to which
--       user-specific data files should be stored.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
dataHome = envVar "XDG_DATA_HOME"

-- |
--
--       There is a single base directory relative to which user-specific
--       configuration files should be written. This directory is defined by the
--       environment variable @$XDG_CONFIG_HOME@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
--
--       @$XDG_CONFIG_HOME@ defines the base directory relative to which
--       user-specific configuration files should be stored.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
configHome = envVar "XDG_CONFIG_HOME"

-- |
--
--       There is a single base directory relative to which user-specific state
--       data should be written. This directory is defined by the environment
--       variable @$XDG_STATE_HOME@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
--
--       @$XDG_STATE_HOME@ defines the base directory relative to which
--       user-specific state files should be stored.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
stateHome = envVar "XDG_STATE_HOME"

-- |
--
--        There is a set of preference ordered base directories relative to
--       which data files should be searched. This set of directories is defined
--       by the environment variable @$XDG_DATA_DIRS@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
dataDirs = envVar "XDG_DATA_DIRS"

-- |
--
--        There is a set of preference ordered base directories relative to
--       which configuration files should be searched. This set of directories
--       is defined by the environment variable @$XDG_CONFIG_DIRS@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
configDirs = envVar "XDG_CONFIG_DIRS"

-- |
--
--       There is a single base directory relative to which user-specific
--       non-essential (cached) data should be written. This directory is
--       defined by the environment variable @$XDG_CACHE_HOME@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
--
--       @$XDG_DATA_HOME@ defines the base directory relative to which
--       user-specific data files should be stored.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
cacheHome = envVar "XDG_CACHE_HOME"

-- |
--
--        There is a single base directory relative to which user-specific
--       runtime files and other file objects should be placed. This directory
--       is defined by the environment variable @$XDG_RUNTIME_DIR@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
runtimeDir = envVar "XDG_RUNTIME_DIR"

-- FIXME: These are GNU make variables, the values should be populated at compile time.
--        See https://www.gnu.org/prep/standards/html_node/Directory-Variables.html

datadir, sysconfdir :: (System.Rep rep) => EnvironmentVariable rep
datadir = envVar "datadir"
sysconfdir = envVar "sysconfdir"
