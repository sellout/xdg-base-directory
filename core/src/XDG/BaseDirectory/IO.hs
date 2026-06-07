{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- The XDG Base Directory specification describes how to access six different
-- categories of file. Here is the API for accessing each of them:
--
-- == data
--
-- - @`withTargetFile` _ `Data`@ – for write-only access to a specific user or
--   system data file
-- - @`withAggregateFiles` `Data`@ – for read-only access to a sequence of data
--   files
--
-- == config
--
-- - @`withTargetFile` _ `Config`@ – for write-only access to a specific user or
--   system configuration file
-- - @`withAggregateFiles` `Config`@ – for read-only access to a sequence of
--   configuration files
--
-- == state
--
-- - @`withUserFile` `State`@ – for write access to a specific state file
-- - @`withUserFileRO` `State`@ – for read-only access to a specific state file
--
-- == executable
--
-- - `withExecutableFile` -- for write-only access to a specific executable file
--
--   Executable files can only be written, and XDG doesn’t allow configuration
--   of the directory used. To read executables, you should check the @PATH@
--   environment variable. This function will warn if you try to write a file
--   and @$HOME/.local/bin/@ isn’t on the @PATH@. Unlike the other types of
--   files, this one doesn’t go into a program-specific subdirectory.
--
-- == cache
--
-- - @`withUserFile` `Cache`@ – for write access to a specific cache file
-- - @`withUserFileRO` `Cache`@ – for read-only access to a specific cache file
--
-- == runtime
--
-- - `withRuntimeFile` – for write access to a specific runtime file
-- - `withRuntimeFileRO` – for read-only access to a specific runtime file
--
--   The runtime directory has special requirements, and these operations check
--   them as much as possible. If the requirements aren’t met when trying to
--   write, it errors, rather than writing to an insecure location. If they
--   aren’t met when trying to read, we return a warning.
module XDG.BaseDirectory.IO
  ( User (..),
    Aggregate (..),
    AggregateDirWarnings,
    IOWriteMode (..),
    InvalidRuntimeDir (..),
    SystemDirWarnings,
    WithFileFailure,
    withUserFile,
    withUserFileRO,
    withUserTargetFile,
    withSystemTargetFile,
    withExecutableFile,
    withAggregateFiles,
    withRuntimeFile,
    withRuntimeFileRO,

    -- * utilities
    consAggregate,
  )
where

import safe "base" Control.Applicative (liftA2, pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad (join, (<=<), (=<<))
import safe "base" Control.Monad.IO.Class (MonadIO, liftIO)
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (Bool (False), bool)
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable, foldr, toList)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor, fmap, (<$>))
import safe "base" Data.Functor.Compose (Compose (Compose), getCompose)
import safe qualified "base" Data.Kind as Kind
import safe "base" Data.List.NonEmpty (NonEmpty)
import safe qualified "base" Data.List.NonEmpty as NonEmpty
import safe "base" Data.Maybe (Maybe (Nothing))
import safe "base" Data.Ord (Ord)
import safe "base" Data.Traversable (Traversable, sequenceA, traverse)
import safe "base" Data.Tuple (curry, uncurry)
import safe "base" Data.Word (Word16)
import safe "base" GHC.Generics (Generic, Generic1)
import safe "base" System.IO (Handle, IO, IOMode)
import safe qualified "base" System.IO as IO
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)
import safe "exceptions" Control.Monad.Catch (MonadMask)
import safe "pathway" Data.Path (Path, Relativity (Abs, Rel), Type (Dir, File), (</>))
import safe qualified "pathway" Data.Path.Directory as Directory
import safe qualified "pathway" Data.Path.File as File
import safe qualified "pathway-system" System.Path as Path
import safe qualified "pathway-system" System.Text as Text
import safe "these" Data.These (These (That, These, This))
import "variant" Data.Variant (V, liftVariant, toVariant, (:<))
import "variant" Data.Variant.Types (Concat)
import safe "yaya" Yaya.Pattern (AndMaybe (Indeed, Only))
import safe "this" Data.Annotated (Annotated (NotBut, Noted))
import safe "this" XDG.BaseDirectory
  ( binHome,
    cacheHome,
    configDirs,
    configHome,
    dataDirs,
    dataHome,
    datadir,
    runtimeDir,
    stateHome,
    sysconfdir,
  )
import safe "this" XDG.BaseDirectory.Internal (BaseDirectory, Error, VarError)

-- $setup
-- >>> :seti -XQuasiQuotes
-- >>> :seti -XTypeApplications
-- >>> import "base" Control.Applicative ((<*>))
-- >>> import "base" Control.Monad (join)
-- >>> import "base" Data.Bool (Bool (True))
-- >>> import "base" Data.Function (const)
-- >>> import "base" Data.String (String)
-- >>> import "base" Data.Tuple (snd)
-- >>> import "base" System.Environment (setEnv)
-- >>> import "pathway" Data.Path (toText)
-- >>> import "pathway" Data.Path.TH (posix)
-- >>> import qualified "pathway" Data.Path.Format as Format
-- >>> import "pathway-compat-temporary" System.IO.Temp.Overlay (createTempDirectory, getCanonicalTemporaryDirectory)
-- >>> import "pathway-system" System.Path (createDirectoryWithParentsIfMissing)
--
-- __TODO__: Extract this to a testing package.
-- >>> tempBase <- getCanonicalTemporaryDirectory
-- >>> tempRoot <- createTempDirectory tempBase "xdg-base-directory-haskell-doctest"
-- >>> tempHome = tempRoot </> [posix|home/example-user/|]
-- >>> createDirectoryWithParentsIfMissing tempHome
-- Right ()
-- >>> setEnv "HOME" $ toText Format.local tempHome

-- | The types of file that live /only/ in the user’s home directory and can be
--   read & written arbitrarily.
data User = Cache | State
  deriving stock (Eq, Generic, Ord, Read, Show)

resolveUser ::
  (Path.Rep rep, Text.Rep rep) =>
  User ->
  IO
    ( Either
        (Error rep, V (Path.GetUserDirectoryFailure rep))
        (Annotated (Error rep) (BaseDirectory rep))
    )
resolveUser = \case
  Cache -> cacheHome
  State -> stateHome

-- | The types of file that allow for layered results.
--
-- - they can be written to in either a `System` or `User` location and
-- - they can be read “in aggregate” – receiving a sequence of handles in order
--   of decreasing importance (later entries may either be ignored or contribute
--   to the final result).
data Aggregate = Config | Data
  deriving stock (Eq, Generic, Ord, Read, Show)

type SystemDirWarnings (rep :: Kind.Type) =
  V '[VarError, [Error rep]] :: Kind.Type

-- | `This` implies a failure to get the user directory, and `That` means
--   there was some failure in getting the system directories (but there’s
--   always at least one system directory, even if we had to reach for the
--   defaults).
type AggregateDirWarnings (rep :: Kind.Type) =
  These
    -- failed to get the user directory
    (Error rep `AndMaybe` V (Path.GetUserDirectoryFailure rep))
    (SystemDirWarnings rep) ::
    Kind.Type

consAggregate ::
  Either
    (Error rep, V (Path.GetUserDirectoryFailure rep))
    (Annotated (Error rep) (BaseDirectory rep)) ->
  Annotated (SystemDirWarnings rep) (NonEmpty (BaseDirectory rep)) ->
  Annotated (AggregateDirWarnings rep) (NonEmpty (BaseDirectory rep))
consAggregate =
  either
    ( \e -> \case
        NotBut dirs -> Noted (This $ uncurry Indeed e) dirs
        Noted e' dirs -> Noted (These (uncurry Indeed e) e') dirs
    )
    ( curry \case
        (NotBut dir, NotBut dirs) -> NotBut $ NonEmpty.cons dir dirs
        (NotBut dir, Noted e' dirs) -> Noted (That e') $ NonEmpty.cons dir dirs
        (Noted e dir, NotBut dirs) ->
          Noted (This $ Only e) $ NonEmpty.cons dir dirs
        (Noted e dir, Noted e' dirs) ->
          Noted (These (Only e) e') $ NonEmpty.cons dir dirs
    )

resolveAggregate ::
  (Path.Rep rep, Text.Rep rep) =>
  Aggregate ->
  IO (Annotated (AggregateDirWarnings rep) (NonEmpty (BaseDirectory rep)))
resolveAggregate =
  uncurry (liftA2 consAggregate) . \case
    Config -> (configHome, configDirs)
    Data -> (dataHome, dataDirs)

resolveUserAggregate ::
  (Path.Rep rep, Text.Rep rep) =>
  Aggregate ->
  IO
    ( Either
        (Error rep, V (Path.GetUserDirectoryFailure rep))
        (Annotated (Error rep) (BaseDirectory rep))
    )
resolveUserAggregate = \case
  Config -> configHome
  Data -> dataHome

resolveSystemAggregate ::
  (Path.Rep rep, Text.Rep rep) =>
  Aggregate ->
  IO (Annotated (Error rep) (BaseDirectory rep))
resolveSystemAggregate = \case
  Config -> sysconfdir
  Data -> datadir

-- | This effectively checks some input against a function, and then returns the
--   input (in the correct context if it worked).
tracing :: (Functor m) => (a -> m ()) -> a -> m a
tracing f x = (\() -> x) <$> f x

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
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
data InvalidRuntimeDir (rep :: Kind.Type)
  = -- |
    --       The directory MUST be owned by the user,
    --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
    OwnedByWrongUser rep
  | -- |
    --
    --  __TODO__: This should maybe behave differently for reads & writes. As it
    --            stands, this type is only a warning for reads, but an error
    --            for writes. However, we might want to error on reads when the
    --            write permissions are incorrect, since that implies an
    --            untrusted source could have modified the data we’re reading.
    --
    --       and they MUST be the only one having read and write access to it.
    --       Its Unix access mode MUST be 0700.
    --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
    IncorrectPermissions Word16
  | -- | This is approximated by checking that the directory already exists.
    --   This could also check that no files contained in the directory predates
    --   the last known reboot.
    --
    --       The lifetime of the directory MUST be bound to the user being
    --       logged in. It MUST be created when the user first logs in and if
    --       the user fully logs out the directory MUST be removed. If the user
    --       logs in more than once they should get pointed to the same
    --       directory, and it is mandatory that the directory continues to
    --       exist from their first login to their last logout on the system,
    --       and not removed in between. Files in the directory MUST not survive
    --       reboot or a full logout/login cycle.
    --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
    InvalidLifetime
  | -- |
    --
    --      The directory MUST be on a local file system and not shared with any
    --       other system.
    --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
    NotLocal
  | -- |
    --
    --      The directory MUST by fully-featured by the standards of the
    --       operating system. More specifically, on Unix-like operating systems
    --       AF_UNIX sockets, symbolic links, hard links, proper permissions,
    --       file locking, sparse files, memory mapping, file change
    --       notifications, a reliable hard link count must be supported, and no
    --       restrictions on the file name character set should be imposed.
    --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
    NotFullyFeatured
  deriving stock (Eq, Ord, Generic, Read, Show)
  deriving stock (Foldable, Functor, Generic1, Traversable)

type role InvalidRuntimeDir representational

-- | This checks the properties that we can, to ensure that it follows them.
--
-- * must be owned by current user
-- * must be the only one with read or write access (on Unix, specifically 0700 mode)
-- * must already exist
-- * must be on local file system
-- * must be “fully-featured”
verifyRuntimeDir ::
  (Path.Operations rep 'Dir) =>
  BaseDirectory rep ->
  IO (Either (InvalidRuntimeDir rep) ())
verifyRuntimeDir dir =
  bool (Left InvalidLifetime) (pure ()) <$> Path.doesExist dir

type WithFileFailure =
  Concat Path.OpenFileFailure Path.MaybeParentCreationFailure :: [Kind.Type]

-- |
--
--       If, when attempting to write a file, the destination directory is
--       non-existent an attempt should be made to create it with permission
--       @0700@. If the destination directory exists already the permissions
--       should not be changed. The application should be prepared to handle the
--       case where the file could not be written, either because the directory
--       was non-existent and could not be created, or for any other reason. In
--       such case it may choose to present an error message to the user.
--
--       —[§4](https://specifications.freedesktop.org/basedir-spec/latest/#referencing)
withFile ::
  (MonadIO m, MonadMask m, Path.Rep rep) =>
  Path ('Rel 'False) 'File rep ->
  IOWriteMode ->
  (Path 'Abs 'File rep -> Handle -> m a) ->
  BaseDirectory rep ->
  m (Either (V WithFileFailure) a)
withFile filename mode action base =
  let filepath = base </> filename
   in either
        (pure . Left . liftVariant)
        ( \() ->
            fmap (first liftVariant)
              . Path.withFile filepath (resolveIOWriteMode mode)
              $ action filepath
        )
        <=< liftIO . Path.createDirectoryWithParentsIfMissing
        $ File.directory filepath

withFileRO ::
  (MonadIO m, MonadMask m, Path.Rep rep) =>
  Path ('Rel 'False) 'File rep ->
  (Path 'Abs 'File rep -> Handle -> m a) ->
  BaseDirectory rep ->
  m (Either (V Path.OpenFileFailure) a)
withFileRO filename action base =
  let filepath = base </> filename
   in Path.withFile filepath IO.ReadMode $ action filepath

-- |
--
-- >>> :{
--   withUserFile @IO @String
--     State
--     [posix|myprogram/archive.db|]
--     ReadWriteMode
--     $ const pure
-- :}
-- Right (These (FileError (ConstructionError (Var (...Var "XDG_STATE_HOME"...)) :| []) {handle: /.../home/example-user/.local/state/myprogram/archive.db})
withUserFile ::
  (MonadIO m, MonadMask m, Path.Rep rep, Text.Rep rep) =>
  User ->
  Path ('Rel 'False) 'File rep ->
  IOWriteMode ->
  (Path 'Abs 'File rep -> Handle -> m a) ->
  m (Either (V ((Error rep, V (Path.GetUserDirectoryFailure rep)) ': WithFileFailure)) (Annotated (Error rep) a))
withUserFile user filename mode action =
  fmap join
    . traverse
      (fmap (first liftVariant . sequenceA) . traverse (withFile filename mode action))
    . first toVariant
    <=< liftIO
    $ resolveUser user

-- |
--
-- >>> withUserFileRO @IO @String State [posix|myprogram/archive.db|] $ const pure
-- This (ConstructionError (Var (...Var "XDG_STATE_HOME"...) :| [IOError .../home/example-user/.local/state/myprogram/archive.db: openFile: does not exist (No such file or directory)])
withUserFileRO ::
  (MonadIO m, MonadMask m, Path.Rep rep, Text.Rep rep) =>
  User ->
  Path ('Rel 'False) 'File rep ->
  (Path 'Abs 'File rep -> Handle -> m a) ->
  m
    ( Either
        (Error rep, V (Path.GetUserDirectoryFailure rep))
        (Annotated (Error rep) (Either (V Path.OpenFileFailure) a))
    )
withUserFileRO user filename action =
  traverse (traverse $ withFileRO filename action)
    <=< liftIO
    $ resolveUser user

-- | Apply an action to the same file in each directory.
foldDirs ::
  (MonadIO m, MonadMask m, Path.Rep rep) =>
  Path ('Rel 'False) 'File rep ->
  ([(Path 'Abs 'File rep, Handle)] -> m a) ->
  [BaseDirectory rep] ->
  -- |
  --
  --  __FIXME__: Rewrite this so we have a @`These` (`NonEmpty` `IOError`)@ at the end.
  m (Annotated (NonEmpty (V Path.OpenFileFailure)) a)
foldDirs filename action dirs =
  foldr
    ( \dir act others ->
        -- FIXME: Ensure this `Noted` isn’t discarding the other annotations.
        either (\e -> (Noted (pure e) =<<) <$> act others) pure
          =<< withFileRO filename (\p -> act . (: others) . (p,)) dir
    )
    (fmap pure . action)
    dirs
    []

-- | Open all of the associated files.
--
--   This can only open files for reading. To write to (some of) these files,
--   use `withTargetFile`.
--
--        A specification that refers to @$XDG_DATA_DIRS@ or
--        @$XDG_CONFIG_DIRS@ should define what the behaviour must be when a
--        file is located under multiple base directories. It could, for
--        example, define that only the file under the most important base
--        directory should be used or, as another example, it could define
--        rules for merging the information from the different files.
--        —[§4](https://specifications.freedesktop.org/basedir-spec/0.8/#referencing)
--
-- >>> :{
--   withAggregateFiles @IO @String
--     Config
--     [posix|myprogram/settings.dhall|]
--     $ pure . fmap snd
-- :}
-- This [ConstructionError (Var (...Var "XDG_CONFIG_HOME"...),ConstructionError (Var (MissingVar "XDG_CONFIG_DIRS" Nothing)),IOError .../home/example-user/.config/myprogram/settings.dhall: openFile: does not exist (No such file or directory),IOError /etc/xdg/myprogram/settings.dhall: openFile: does not exist (No such file or directory)]
--
-- >>> :{
--   withAggregateFiles @IO @String
--     Data
--     [posix|myprogram/resources/splash.png|]
--     $ pure . fmap snd
-- :}
-- This [ConstructionError (Var (...Var "XDG_DATA_HOME"...),ConstructionError (Var (...Var "XDG_DATA_DIRS"...),IOError .../home/example-user/.local/share/myprogram/resources/splash.png: openFile: does not exist (No such file or directory),IOError /usr/local/share/myprogram/resources/splash.png: openFile: does not exist (No such file or directory),IOError /usr/share/myprogram/resources/splash.png: openFile: does not exist (No such file or directory)]
withAggregateFiles ::
  (MonadIO m, MonadMask m, Path.Rep rep, Text.Rep rep) =>
  -- | What kinds of files we are reading.
  Aggregate ->
  -- | Path to the desired file, relative to the project prefixes.
  Path ('Rel 'False) 'File rep ->
  -- | An action that is given the list of handles we are reading from. The
  --   handles are in order of decreasing importance, so the ones after the
  --   first can be dropped, or otherwise have earlier ones layered on them.
  ([(Path 'Abs 'File rep, Handle)] -> m a) ->
  -- |
  --
  --  __FIXME__: Don’t nest `Aggregated`, instead combine the per-file errors in one level.
  m (Annotated (AggregateDirWarnings rep) (Annotated (NonEmpty (V Path.OpenFileFailure)) a))
withAggregateFiles aggregate filename action =
  traverse (foldDirs filename action . toList)
    <=< liftIO
    $ resolveAggregate aggregate

-- | This can only write to the targeted file. To read, use `withAggregateFiles`
--   to access all of the related files.
--
--  __TODO__: This should warn if a `System` dir (`datadir` or `sysconfdir`)
--            isn’t in the the corresponding aggregate list (@XDG_DATA_DIRS@ or
--            @XDG_CONFIG_DIRS@, respectively).
--
-- >>> withUserTargetFile @IO @String Config [posix|myprogram/settings.dhall|] False $ const pure
-- Right (These (FileError (ConstructionError (Var (...Var "XDG_CONFIG_HOME"...)) :| []) {handle: /.../home/example-user/.config/myprogram/settings.dhall})
withUserTargetFile ::
  (MonadIO m, MonadMask m, Path.Rep rep, Text.Rep rep) =>
  Aggregate ->
  Path ('Rel 'False) 'File rep ->
  Bool ->
  (Path 'Abs 'File rep -> Handle -> m a) ->
  m
    ( Either
        ( V
            ( (Error rep, V (Path.GetUserDirectoryFailure rep))
                ': WithFileFailure
            )
        )
        (Annotated (Error rep) a)
    )
withUserTargetFile aggregate filename truncate action =
  fmap join
    . traverse
      ( fmap (first liftVariant . sequenceA)
          . traverse
            ( withFile
                filename
                (if truncate then WriteMode else AppendMode)
                action
            )
      )
    . first toVariant
    <=< liftIO
    $ resolveUserAggregate aggregate

withSystemTargetFile ::
  (MonadIO m, MonadMask m, Path.Rep rep, Text.Rep rep) =>
  Aggregate ->
  Path ('Rel 'False) 'File rep ->
  Bool ->
  (Path 'Abs 'File rep -> Handle -> m a) ->
  m (Annotated (Error rep) (Either (V WithFileFailure) a))
withSystemTargetFile aggregate filename truncate action =
  traverse
    (withFile filename (if truncate then WriteMode else AppendMode) action)
    <=< liftIO
    $ resolveSystemAggregate aggregate

-- | This unifies the complex @XDG_RUNTIME_FILE@ handling, which is then exposed
--   via two different functions.
--
-- * toggle sticky bit when we open/close
withRuntimeFile' ::
  (MonadIO m, Path.Rep rep, Path.Operations rep 'Dir, Text.Rep rep, InvalidRuntimeDir rep :< w) =>
  ( Path ('Rel 'False) 'File rep ->
    (Path 'Abs 'File rep -> Handle -> m a) ->
    BaseDirectory rep ->
    m (Either (V w) a)
  ) ->
  -- | This function sets the sticky bit on any created file while it’s
  --   operating. If this is `True`, it will leave the sticky bit set to avoid
  --   cleanup. If it’s `False`, it will unset the sticky bit even if the file
  --   previously existed with the sticky bit on, and if it’s `Nothing`, it will
  --   restore any previous sticky bit value (but default to `False` for newly
  --   created files).
  --
  --  __TODO__: Could also set a timer to ensure the access timestamp is updated
  --            periodically.
  Maybe Bool ->
  Path ('Rel 'False) 'File rep ->
  -- | Produce a warning to be presented to the user when @XDG_RUNTIME_DIR@
  --   isn’t set. A fallback directory to use if @$XDG_RUNTIME_DIR@ isn’t set. If this is
  --   `Nothing`, then @$XDG_RUNTIME_DIR@ being unset results in an error
  --   instead of a warning
  --
  --       If @$XDG_RUNTIME_DIR@ is not set applications should fall back to a
  --       replacement directory with similar capabilities and print a warning
  --       message.
  --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
  (Error rep -> m (BaseDirectory rep)) ->
  (Path 'Abs 'File rep -> Handle -> m a) ->
  m (Either (V w) a)
withRuntimeFile' withFile' _preserve filename fallback action =
  ( ( fmap join
        . traverse
          ( withFile' filename \handle -> do
              -- prevStickyBit <- casStickyBit handle True
              action handle
              -- maybe
              --   (setStickyBit handle prevStickyBit)
              --   (bool (setStickyBit handle False) $ pure ())
              --   preserve
          )
    )
      <=< getCompose . tracing (Compose . liftIO . fmap (first toVariant) . verifyRuntimeDir)
      <=< either fallback pure
  )
    =<< liftIO runtimeDir

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
--
-- >>> :{
--   withRuntimeFile @IO @String
--     [posix|myprogram/super-secret.age|]
--     ReadWriteMode
--     Nothing
--     (const $ pure [posix|/run/whatever/|])
--     $ const pure
-- :}
-- Right {handle: /.../myprogram/super-secret.age}
withRuntimeFile ::
  (MonadIO m, MonadMask m, Path.Rep rep, Path.Operations rep 'Dir, Text.Rep rep) =>
  Path ('Rel 'False) 'File rep ->
  IOWriteMode ->
  Maybe Bool ->
  (Error rep -> m (BaseDirectory rep)) ->
  (Path 'Abs 'File rep -> Handle -> m a) ->
  m (Either (V (InvalidRuntimeDir rep ': WithFileFailure)) a)
withRuntimeFile filename mode preserve =
  withRuntimeFile'
    (\fn handle -> fmap (first liftVariant) . withFile fn mode handle)
    preserve
    filename

-- |
--
-- >>> :{
--   withRuntimeFileRO @IO @String
--     [posix|myprogram/super-secret.age|]
--     (const $ pure [posix|/run/whatever/|])
--     $ const pure
-- :}
-- Right {handle: /.../myprogram/super-secret.age}
withRuntimeFileRO ::
  (MonadIO m, MonadMask m, Path.Rep rep, Path.Operations rep 'Dir, Text.Rep rep) =>
  Path ('Rel 'False) 'File rep ->
  (Error rep -> m (BaseDirectory rep)) ->
  (Path 'Abs 'File rep -> Handle -> m a) ->
  m (Either (V (InvalidRuntimeDir rep ': WithFileFailure)) a)
withRuntimeFileRO =
  withRuntimeFile'
    (\fn handle -> fmap (first liftVariant) . withFileRO fn handle)
    Nothing

-- | Executables can only be written, not read. To /find/ an executable, you
--   should check the @PATH@ environment variable.
--
--       User-specific executable files may be stored in @$HOME@/.local/bin.
--       Distributions should ensure this directory shows up in the UNIX @$PATH@
--       environment variable, at an appropriate place.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
--
-- >>> withExecutableFile "some-script.sh" True $ const pure
-- NotBut (Right {handle: /.../home/example-user/.local/bin/some-script.sh})
withExecutableFile ::
  (MonadIO m, MonadMask m, Path.Rep rep, Text.Rep rep) =>
  -- | The name of the executable to write.
  rep ->
  -- | Whether the file should be truncated (`True` → `IO.WriteMode`,
  --   `False` → `IO.AppendMode`).
  Bool ->
  (Path 'Abs 'File rep -> Handle -> m a) ->
  -- | Returns @`Noted` ()@ if @$HOME/.local/bin/@ isn’t on @PATH@.
  m
    ( Annotated
        ()
        ( Either
            ( V
                ( Concat
                    WithFileFailure
                    (Path.GetUserDirectoryFailure rep)
                )
            )
            a
        )
    )
withExecutableFile filename truncate action =
  -- TODO: Ensure the file is actually executable
  -- TODO: Warn if `binHome` isn’t on Path
  fmap (pure . join) $
    traverse
      ( fmap (first liftVariant)
          . withFile
            (Directory.selectFile Directory.current filename)
            (if truncate then WriteMode else AppendMode)
            action
      )
      . first liftVariant
      =<< liftIO binHome

-- | `IOMode`, but restricted to modes that involve writing.
data IOWriteMode = WriteMode | AppendMode | ReadWriteMode
  deriving stock (Eq, Generic, Ord, Read, Show)

resolveIOWriteMode :: IOWriteMode -> IOMode
resolveIOWriteMode = \case
  WriteMode -> IO.WriteMode
  AppendMode -> IO.AppendMode
  ReadWriteMode -> IO.ReadWriteMode
