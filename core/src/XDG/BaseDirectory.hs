{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--       Various specifications specify files and file formats. This
--       specification defines where these files should be looked for by
--       defining one or more base directories relative to which files should be
--       located.
--       —[§1](https://specifications.freedesktop.org/basedir-spec/latest/#introduction)
module XDG.BaseDirectory
  ( module XDG.BaseDirectory.Internal,
    dataHome,
    configHome,
    stateHome,
    dataDirs,
    configDirs,
    cacheHome,
    -- `runtimeDir` has no default, so we just re-export the term from "Custom".
    Custom.runtimeDir,
    -- `binHome` can’t be customized, so we just re-export the term from
    -- "Default".
    Default.binHome,

    -- * GNU Make installation directories
    datadir,
    sysconfdir,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad (Monad, (=<<))
import safe "base" Data.Bifunctor (bimap)
import safe "base" Data.Either (Either, either, partitionEithers)
import safe "base" Data.Foldable (null)
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.String (String)
import safe "base" Data.Traversable (traverse)
import safe "base" System.IO (IO)
import safe qualified "pathway-system" System.Path as Path
import safe qualified "pathway-system" System.Text as Text
import "variant" Data.Variant (V, toVariant)
import safe "this" Data.Annotated (Annotated (NotBut, Noted))
import safe qualified "this" XDG.BaseDirectory.Custom as Custom
import safe qualified "this" XDG.BaseDirectory.Default as Default
import safe "this" XDG.BaseDirectory.Internal (BaseDirectory, Error, VarError)

-- | The spec says
--
--       All paths set in these environment variables must be absolute. If an
--       implementation encounters a relative path in any of these variables it
--       should consider the path invalid and ignore it.
--
--   so we don’t bother to return any error here, as we are going to discard it
--   anyway.
getOrDefault ::
  (Monad m) =>
  -- | The default value.
  m (Either e (BaseDirectory rep)) ->
  -- | The intended value.
  m (Either e' (BaseDirectory rep)) ->
  -- | If the intended value failed, we annotated the successful result with the
  --   error message. If both failed, we return both the lookup failure & the
  --   default resolution failure.
  m (Either (e', e) (Annotated e' (BaseDirectory rep)))
getOrDefault def =
  (either (\e' -> bimap (e',) (Noted e') <$> def) (pure . pure . NotBut) =<<)

-- |
--
--  __TODO__: If we end up ignoring all of the directories in this list, should
--            we use the defaults?
getMultipleOrDefault ::
  (Monad m) =>
  m (NonEmpty (BaseDirectory rep)) ->
  m (Either VarError [Either (Error rep) (BaseDirectory rep)]) ->
  m (Annotated (V '[VarError, [Error rep]]) (NonEmpty (BaseDirectory rep)))
getMultipleOrDefault def =
  ( either
      (\e -> Noted (toVariant e) <$> def)
      ( ( \(errs, dirs) ->
            maybe
              (Noted (toVariant errs) <$> def)
              (pure . if null errs then NotBut else Noted $ toVariant errs)
              $ nonEmpty dirs
        )
          . partitionEithers
      )
      =<<
  )

dataHome,
  configHome,
  stateHome,
  cacheHome ::
    (Path.Rep rep, Text.Rep rep) =>
    IO
      ( Either
          (Error rep, V (Path.GetUserDirectoryFailure rep))
          (Annotated (Error rep) (BaseDirectory rep))
      )
dataHome = getOrDefault Default.dataHome Custom.dataHome
configHome = getOrDefault Default.configHome Custom.configHome

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
stateHome = getOrDefault Default.stateHome Custom.stateHome

cacheHome = getOrDefault Default.cacheHome Custom.cacheHome

dataDirs,
  configDirs ::
    (Path.Rep rep, Text.Rep rep) =>
    IO (Annotated (V '[VarError, [Error rep]]) (NonEmpty (BaseDirectory rep)))

-- |
--
--        @$XDG_DATA_DIRS@ defines the preference-ordered set of base
--       directories to search for data files in addition to the
--       @$XDG_DATA_HOME@ base directory. The directories in @$XDG_DATA_DIRS@
--       should be separated with a colon ':'.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
dataDirs =
  getMultipleOrDefault
    (traverse (traverse Text.encodeString) Default.dataDirs)
    Custom.dataDirs

-- |
--
--        @$XDG_CONFIG_DIRS@ defines the preference-ordered set of base
--       directories to search for configuration files in addition to the
--       @$XDG_CONFIG_HOME@ base directory. The directories in
--       @$XDG_CONFIG_DIRS@ should be separated with a colon ':'.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
configDirs =
  getMultipleOrDefault
    (traverse (traverse Text.encodeString) Default.configDirs)
    Custom.configDirs

makeDir ::
  (Text.Rep rep) =>
  BaseDirectory String ->
  Either e (BaseDirectory rep) ->
  IO (Annotated e (BaseDirectory rep))
makeDir def =
  either
    (\e -> Noted e <$> traverse Text.encodeString def)
    (pure . NotBut)

datadir,
  sysconfdir ::
    (Path.Rep rep, Text.Rep rep) =>
    IO (Annotated (Error rep) (BaseDirectory rep))

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
datadir = makeDir Default.datadir =<< Custom.datadir

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
sysconfdir = makeDir Default.sysconfdir =<< Custom.sysconfdir
