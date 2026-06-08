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
  ( module XDG.BaseDirectory.Internal,
    dataHome,
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

import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<), (=<<))
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" System.IO (IO)
import safe "pathway-compat-base" Common (InternalFailure (ParseFailure))
import safe qualified "pathway-system" System.Path as Path
import safe qualified "pathway-system" System.Text as Text
import qualified "this" Paths_xdg_base_directory as Make
import safe "this" XDG.BaseDirectory.Internal
  ( BaseDirectory,
    Error (Pathway, Var),
    VarError,
    extractAbs,
  )
import safe qualified "this" XDG.BaseDirectory.Var as Var

parseDir :: (Path.Rep rep) => rep -> Either (Error rep) (BaseDirectory rep)
parseDir =
  either (Left . Pathway . ParseFailure) extractAbs
    . Path.parseDirectory

get ::
  (Path.Rep rep, Text.Rep rep) =>
  Var.EnvironmentVariable -> IO (Either (Error rep) (BaseDirectory rep))
get = fmap (parseDir <=< first Var) . Var.lookupNonEmptyEnv

getMultiple ::
  (Path.Rep rep, Text.Rep rep) =>
  Var.EnvironmentVariable ->
  -- | The outer `Left` represents failures due to the variable, and the inner
  --   ones represent failures for each path found in the varaible.
  IO (Either VarError [Either (Error rep) (BaseDirectory rep)])
getMultiple =
  fmap (fmap (either (Left . Pathway) extractAbs) . Path.splitSearchPath <$>)
    . Var.lookupNonEmptyEnv

dataHome,
  configHome,
  stateHome,
  cacheHome,
  runtimeDir ::
    (Path.Rep rep, Text.Rep rep) => IO (Either (Error rep) (BaseDirectory rep))

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

-- |
--
--        There is a single base directory relative to which user-specific
--       runtime files and other file objects should be placed. This directory
--       is defined by the environment variable @$XDG_RUNTIME_DIR@.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
--
--       @$XDG_RUNTIME_DIR@ defines the base directory relative to which
--       user-specific non-essential runtime files and other file objects (such
--       as sockets, named pipes, ...) should be stored. The directory MUST be
--       owned by the user, and they MUST be the only one having read and write
--       access to it. Its Unix access mode MUST be 0700.
--
--       The lifetime of the directory MUST be bound to the user being logged
--       in. It MUST be created when the user first logs in and if the user
--       fully logs out the directory MUST be removed. If the user logs in more
--       than once they should get pointed to the same directory, and it is
--       mandatory that the directory continues to exist from their first login
--       to their last logout on the system, and not removed in between. Files
--       in the directory MUST not survive reboot or a full logout/login cycle.
--
--       The directory MUST be on a local file system and not shared with any
--       other system. The directory MUST by fully-featured by the standards of
--       the operating system. More specifically, on Unix-like operating systems
--       AF_UNIX sockets, symbolic links, hard links, proper permissions, file
--       locking, sparse files, memory mapping, file change notifications, a
--       reliable hard link count must be supported, and no restrictions on the
--       file name character set should be imposed. Files in this directory MAY
--       be subjected to periodic clean-up. To ensure that your files are not
--       removed, they should have their access time timestamp modified at least
--       once every 6 hours of monotonic time or the 'sticky' bit should be set
--       on the file.
--
--       If @$XDG_RUNTIME_DIR@ is not set applications should fall back to a
--       replacement directory with similar capabilities and print a warning
--       message. Applications should use this directory for communication and
--       synchronization purposes and should not place larger files in it, since
--       it might reside in runtime memory and cannot necessarily be swapped out
--       to disk.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
runtimeDir = get Var.runtimeDir

dataDirs,
  configDirs ::
    (Path.Rep rep, Text.Rep rep) =>
    IO (Either VarError [Either (Error rep) (BaseDirectory rep)])

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

datadir,
  sysconfdir ::
    (Path.Rep rep, Text.Rep rep) => IO (Either (Error rep) (BaseDirectory rep))

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
datadir = fmap parseDir . Text.encodeString =<< Make.getDataDir

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
sysconfdir = fmap parseDir . Text.encodeString =<< Make.getSysconfDir
