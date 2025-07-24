{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
--
--       Various specifications specify files and file formats. This
--       specification defines where these files should be looked for by
--       defining one or more base directories relative to which files should be
--       located.
--       —[§1](https://specifications.freedesktop.org/basedir-spec/latest/#introduction)
module XDG.BaseDirectory
  ( dataHome,
    configHome,
    stateHome,
    dataDirs,
    configDirs,
    cacheHome,
    runtimeDir,
    binDir,

    -- * make vars
    datadir,
    sysconfdir,
  )
where

import "base" Control.Applicative (pure, (<*>))
import "base" Control.Category ((.))
import "base" Data.Either (Either, either)
import "base" Data.Function (flip, ($))
import "base" Data.Functor (fmap, (<$>))
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" System.IO (IO)
import "pathway" Data.Path ((</>))
import "pathway" Data.Path.TH (posix)
import "these" Data.These (These (These, This), these)
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import "this" Data.Annotated (Annotated (NotBut, Noted))
import qualified "this" XDG.BaseDirectory.Custom as Custom
import qualified "this" XDG.BaseDirectory.Default as Default
import "this" XDG.BaseDirectory.Internal
  ( BaseDirectory,
    Error,
    getHomeDirectory,
  )

-- | The spec says
--
--       All paths set in these environment variables must be absolute. If an
--       implementation encounters a relative path in any of these variables it
--       should consider the path invalid and ignore it.
--
--   so we don’t bother to return any error here, as we are going to discard it
--   anyway.
getOrDefault ::
  Either e (BaseDirectory rep) ->
  Either e (BaseDirectory rep) ->
  These (NonEmpty e) (BaseDirectory rep)
getOrDefault def =
  either (\e -> either (This . (e :|) . pure) (These $ e :| []) def) pure

-- |
--
--  __TODO__: If we end up ignoring all of the directories in this list, should
--            we use the defaults?
getMultipleOrDefault ::
  NonEmpty (BaseDirectory rep) ->
  These (NonEmpty e) (NonEmpty (BaseDirectory rep)) ->
  Annotated (NonEmpty e) (NonEmpty (BaseDirectory rep))
getMultipleOrDefault def = these (flip Noted def) pure Noted

dataHome,
  configHome,
  stateHome,
  cacheHome ::
    (System.Rep rep) => IO (These (NonEmpty (Error rep)) (BaseDirectory rep))
dataHome = getOrDefault <$> Default.dataHome <*> Custom.dataHome
configHome = getOrDefault <$> Default.configHome <*> Custom.configHome

-- |
--
--       The @$XDG_STATE_HOME@ contains state data that should persist between
--       (application) restarts, but that is not important or portable enough to
--       the user that it should be stored in @$XDG_DATA_HOME@. It may contain:
--
--     - actions history (logs, history, recently used files, …)
--
--     - current state of the application that can be reused on a restart (view,
--       layout, open files, undo history, …)
--
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
stateHome = getOrDefault <$> Default.stateHome <*> Custom.stateHome

cacheHome = getOrDefault <$> Default.cacheHome <*> Custom.cacheHome

dataDirs, configDirs :: (System.Rep rep) => IO (Annotated (NonEmpty (Error rep)) (NonEmpty (BaseDirectory rep)))

-- |
--
--        @$XDG_DATA_DIRS@ defines the preference-ordered set of base
--       directories to search for data files in addition to the
--       @$XDG_DATA_HOME@ base directory. The directories in @$XDG_DATA_DIRS@
--       should be separated with a colon ':'.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
dataDirs = getMultipleOrDefault Default.dataDirs <$> Custom.dataDirs

-- |
--
--        @$XDG_CONFIG_DIRS@ defines the preference-ordered set of base
--       directories to search for configuration files in addition to the
--       @$XDG_CONFIG_HOME@ base directory. The directories in
--       @$XDG_CONFIG_DIRS@ should be separated with a colon ':'.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
configDirs = getMultipleOrDefault Default.configDirs <$> Custom.configDirs

-- |
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
runtimeDir :: (System.Rep rep) => IO (Either (Error rep) (BaseDirectory rep))
runtimeDir = Custom.runtimeDir

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
--        Since @$HOME@ might be shared between systems of different
--       architectures, installing compiled binaries to @$HOME@/.local/bin could
--       cause problems when used on systems of differing architectures. This is
--       often not a problem, but the fact that @$HOME@ becomes partially
--       architecture-specific if compiled binaries are placed in it should be
--       kept in mind.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
binDir :: (System.Rep rep) => IO (Either (Error rep) (BaseDirectory rep))
binDir = fmap (</> fmap System.fromStringLiteral [posix|.local/bin/|]) <$> getHomeDirectory

datadir, sysconfdir :: (System.Rep rep) => IO (Annotated (Error rep) (BaseDirectory rep))

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
datadir = either (flip Noted Default.datadir) NotBut <$> Custom.datadir

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
sysconfdir = either (flip Noted Default.sysconfdir) NotBut <$> Custom.sysconfdir
