{-# LANGUAGE Safe #-}

-- | The XDG Base Directory specification describes how to access six different
--   categories of file. Here is the API for accessing each of them:
--
-- # data
-- - @`withTargetFile` _ `Data`@ – for write-only access to a specific user or
--   system data file
-- - @`withAggregateFiles` `Data`@ – for read-only access to a sequence of data
--   files
--
-- # config
-- - @`withTargetFile` _ `Config`@ – for write-only access to a specific user or
--   system configuration file
-- - @`withAggregateFiles` `Config`@ – for read-only access to a sequence of
--   configuration files
--
-- # state
-- - @`withUserFile` `State`@ – for write access to a specific state file
-- - @`withUserFileRO` `State`@ – for read-only access to a specific state file
--
-- # executable
-- - `withExecutableFile` -- for write-only access to a specific executable file
--
--   Executable files can only be written, and XDG doesn’t allow configuration
--   of the directory used. To read executables, you should check the @PATH@
--   environment variable. This function will warn if you try to write a file
--   and @$HOME/.local/bin/@ isn’t on the @PATH@. Unlike the other types of
--   files, this one doesn’t go into a program-specific subdirectory.
--
-- # cache
-- - @`withUserFile` `Cache`@ – for write access to a specific cache file
-- - @`withUserFileRO` `Cache`@ – for read-only access to a specific cache file
--
-- # runtime
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
    IOWriteMode (..),
    Target (..),
    InvalidRuntimeDir (..),
    FileError (..),
    WriteError (..),
    withUserFile,
    withUserFileRO,
    withTargetFile,
    withExecutableFile,
    withAggregateFiles,
    withRuntimeFile,
    withRuntimeFileRO,

    -- * utilities
    consAggregate,
    consAggregate',
  )
where

import "base" Control.Applicative (liftA2, pure)
import "base" Control.Category (id, (.))
import "base" Control.Exception (try)
import "base" Control.Monad (join, (<=<), (=<<))
import "base" Data.Bifunctor (first)
import "base" Data.Bool (Bool (False), bool)
import "base" Data.Either (Either (Left, Right), either)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable, foldr, toList)
import "base" Data.Function (flip, ($))
import "base" Data.Functor (Functor, fmap, (<$), (<$>))
import "base" Data.Functor.Compose (Compose (Compose), getCompose)
import "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified "base" Data.List.NonEmpty as NonEmpty
import "base" Data.Maybe (Maybe (Nothing), maybe)
import "base" Data.Monoid (mempty)
import "base" Data.Ord (Ord)
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.Traversable (Traversable, traverse)
import "base" Data.Tuple (curry, uncurry)
import "base" Data.Word (Word16)
import "base" GHC.Generics (Generic, Generic1)
import "base" System.IO (Handle, IO, IOMode)
import qualified "base" System.IO as IO
import "base" System.IO.Error (IOError)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "pathway" Data.Path (Path, Relativity (Rel), Type (File), (</>))
import qualified "pathway" Data.Path.Directory as Directory
import qualified "pathway" Data.Path.File as File
import "these" Data.These (These (That, These, This))
import qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import "this" Data.Annotated (Annotated (NotBut, Noted))
import "this" XDG.BaseDirectory
  ( binDir,
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
import "this" XDG.BaseDirectory.Internal (BaseDirectory, Error, weakenEither)

-- $setup
-- >>> :seti -XQuasiQuotes
-- >>> :seti -XTypeApplications
-- >>> import "base" Control.Applicative ((<*>))
-- >>> import "base" Control.Monad (join)
-- >>> import "base" Data.Bool (Bool (True))
-- >>> import "base" Data.Function (const)
-- >>> import "base" System.Environment (setEnv)
-- >>> import "directory" System.Directory (createDirectoryIfMissing)
-- >>> import "filepath" System.FilePath (FilePath, (</>))
-- >>> import qualified "filepath" System.FilePath as FP
-- >>> import "pathway" Data.Path.TH (posix)
-- >>> import "temporary" System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
--
-- __TODO__: Extract this to a testing package.
-- __TODO__: Replace all this directory/filepath/temporary stuff with Pathway.
-- >>> tempDir <- getCanonicalTemporaryDirectory
-- >>> tempRoot <- createTempDirectory tempDir "xdg-base-directory"
-- >>> tempHome = tempRoot FP.</> "home" FP.</> "example-user"
-- >>> createDirectoryIfMissing True tempHome
-- >>> setEnv "HOME" tempHome

-- | The types of file that live _only_ in the user’s home directory and can be
--   read & written arbitrarily.
data User = Cache | State
  deriving stock (Eq, Generic, Ord, Read, Show)

resolveUser ::
  (System.Rep rep) => User -> IO (These (NonEmpty (Error rep)) (BaseDirectory rep))
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

consAggregate' ::
  Either (Error rep) (BaseDirectory rep) ->
  Annotated (NonEmpty (Error rep)) (NonEmpty (BaseDirectory rep)) ->
  Annotated (NonEmpty (Error rep)) (NonEmpty (BaseDirectory rep))
consAggregate' =
  curry \case
    (Left e, NotBut as) -> Noted (pure e) as
    (Left e, Noted es as) -> Noted (NonEmpty.cons e es) as
    (Right a, NotBut as) -> NotBut $ NonEmpty.cons a as
    (Right a, Noted es as) -> Noted es $ NonEmpty.cons a as

consAggregate ::
  These (NonEmpty (Error rep)) (BaseDirectory rep) ->
  Annotated (NonEmpty (Error rep)) (NonEmpty (BaseDirectory rep)) ->
  Annotated (NonEmpty (Error rep)) (NonEmpty (BaseDirectory rep))
consAggregate =
  curry \case
    (This es, NotBut as) -> Noted es as
    (This es, Noted es' as) -> Noted (es <> es') as
    (That a, NotBut as) -> NotBut $ NonEmpty.cons a as
    (That a, Noted es as) -> Noted es $ NonEmpty.cons a as
    (These es a, NotBut as) -> Noted es $ NonEmpty.cons a as
    (These es a, Noted es' as) -> Noted (es <> es') $ NonEmpty.cons a as

weakenAnnotated :: Annotated a b -> These a b
weakenAnnotated = \case
  NotBut b -> That b
  Noted a b -> These a b

resolveAggregate ::
  (System.Rep rep) =>
  Aggregate ->
  IO (Annotated (NonEmpty (Error rep)) (NonEmpty (BaseDirectory rep)))
resolveAggregate =
  uncurry (liftA2 consAggregate) . \case
    Config -> (configHome, configDirs)
    Data -> (dataHome, dataDirs)

data Target = System | User
  deriving stock (Eq, Generic, Ord, Read, Show)

resolveTarget ::
  (System.Rep rep) =>
  Target ->
  Aggregate ->
  IO (These (NonEmpty (Error rep)) (BaseDirectory rep))
resolveTarget =
  curry \case
    (System, Config) ->
      ( \case
          NotBut b -> pure b
          Noted a b -> These (pure a) b
      )
        <$> sysconfdir
    (System, Data) ->
      ( \case
          NotBut b -> pure b
          Noted a b -> These (pure a) b
      )
        <$> datadir
    (User, Config) -> configHome
    (User, Data) -> dataHome

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
data InvalidRuntimeDir rep
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
  deriving stock
    ( Eq,
      Generic,
      Ord,
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

-- | This checks the properties that we can, to ensure that it follows them.
--
-- * must be owned by current user
-- * must be the only one with read or write access (on Unix, specifically 0700 mode)
-- * must already exist
-- * must be on local file system
-- * must be “fully-featured”
verifyRuntimeDir ::
  (System.Rep rep, Ord rep) =>
  BaseDirectory rep ->
  IO (Either (InvalidRuntimeDir rep) ())
verifyRuntimeDir dir =
  bool (Left InvalidLifetime) (pure ()) <$> Patch.doesDirectoryExist dir

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
  (System.Rep rep, Ord rep) =>
  Path ('Rel 'False) 'File rep ->
  IOWriteMode ->
  (Handle -> IO a) ->
  BaseDirectory rep ->
  IO (Either IOError a)
withFile filename mode action base = do
  let filepath = base </> filename
  Patch.createDirectoryWithParentsIfMissing $ File.directory filepath
  try $ Patch.withFile filepath (resolveIOWriteMode mode) action

withFileRO ::
  (System.Rep rep, Ord rep) =>
  Path ('Rel 'False) 'File rep ->
  (Handle -> IO a) ->
  BaseDirectory rep ->
  IO (Either IOError a)
withFileRO filename =
  ((try .) .) . flip $ flip Patch.withFile IO.ReadMode . (</> filename)

-- |
--
-- >>> withUserFile @FilePath State [posix|myprogram/archive.db|] ReadWriteMode pure
-- These (FileError (ConstructionError (Var (...Var "XDG_STATE_HOME"...)) :| []) {handle: .../home/example-user/.local/state/myprogram/archive.db}
withUserFile ::
  (System.Rep rep, Ord rep) =>
  User ->
  Path ('Rel 'False) 'File rep ->
  IOWriteMode ->
  (Handle -> IO a) ->
  IO (These (NonEmpty (WriteError rep)) a)
withUserFile user filename mode action =
  fmap joinThese
    . traverse
      ( fmap (weakenEither . first (pure . CreationFailure))
          . withFile filename mode action
      )
    . first (FileError . ConstructionError <$>)
    =<< resolveUser user

-- |
--
-- >>> withUserFileRO @FilePath State [posix|myprogram/archive.db|] pure
-- This (ConstructionError (Var (...Var "XDG_STATE_HOME"...) :| [IOError .../home/example-user/.local/state/myprogram/archive.db: withFile: does not exist (No such file or directory)])
withUserFileRO ::
  (System.Rep rep, Ord rep) =>
  User ->
  Path ('Rel 'False) 'File rep ->
  (Handle -> IO a) ->
  IO (These (NonEmpty (FileError rep)) a)
withUserFileRO user filename action =
  fmap joinThese
    . traverse
      ( fmap (weakenEither . first (pure . IOError))
          . withFileRO filename action
      )
    <=< fmap (first $ fmap ConstructionError)
    $ resolveUser user

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
-- >>> withAggregateFiles @FilePath Config [posix|myprogram/settings.dhall|] pure
-- This [ConstructionError (Var (...Var "XDG_CONFIG_HOME"...),ConstructionError (Var (MissingVar "XDG_CONFIG_DIRS" Nothing)),IOError .../home/example-user/.config/myprogram/settings.dhall: withFile: does not exist (No such file or directory),IOError /etc/xdg/myprogram/settings.dhall: withFile: does not exist (No such file or directory)]
--
-- >>> withAggregateFiles @FilePath Data [posix|myprogram/resources/splash.png|] pure
-- This [ConstructionError (Var (...Var "XDG_DATA_HOME"...),ConstructionError (Var (EmptyVar "XDG_DATA_DIRS")),IOError .../home/example-user/.local/share/myprogram/resources/splash.png: withFile: does not exist (No such file or directory),IOError /usr/local/share/myprogram/resources/splash.png: withFile: does not exist (No such file or directory),IOError /usr/share/myprogram/resources/splash.png: withFile: does not exist (No such file or directory)]
withAggregateFiles ::
  (System.Rep rep, Ord rep) =>
  -- | What kinds of files we are reading.
  Aggregate ->
  -- | Path to the desired file, relative to the project prefixes.
  Path ('Rel 'False) 'File rep ->
  -- | An action that is given the list of handles we are reading from. The
  --   handles are in order of decreasing importance, so the ones after the
  --   first can be dropped, or otherwise have earlier ones layered on them.
  (NonEmpty Handle -> IO a) ->
  IO (These [FileError rep] a)
withAggregateFiles aggregate filename action =
  fmap joinThese
    . traverse
      (fmap (first (IOError <$>)) . foldDirs filename action)
    . first (toList . fmap ConstructionError)
    . weakenAnnotated
    =<< resolveAggregate aggregate

-- | This can only write to the targeted file. To read, use `withAggregateFiles`
--   to access all of the related files.
--
-- >>> withTargetFile @FilePath User Config [posix|myprogram/settings.dhall|] False pure
-- These (FileError (ConstructionError (Var (...Var "XDG_CONFIG_HOME"...)) :| []) {handle: .../home/example-user/.config/myprogram/settings.dhall}
withTargetFile ::
  (System.Rep rep, Ord rep) =>
  Target ->
  Aggregate ->
  Path ('Rel 'False) 'File rep ->
  Bool ->
  (Handle -> IO a) ->
  IO (These (NonEmpty (WriteError rep)) a)
withTargetFile target aggregate filename truncate action =
  fmap joinThese
    . traverse
      ( fmap (weakenEither . first (pure . CreationFailure))
          . withFile
            filename
            (if truncate then WriteMode else AppendMode)
            action
      )
    . first (FileError . ConstructionError <$>)
    =<< resolveTarget target aggregate

-- |
--
-- * toggle sticky bit when we open/close
withRuntimeFile' ::
  (System.Rep rep, Ord rep) =>
  ( Path ('Rel 'False) 'File rep ->
    (Handle -> IO a) ->
    BaseDirectory rep ->
    IO (Either IOError a)
  ) ->
  (FileError rep -> w) ->
  -- | This function sets the sticky bit on any created file while it’s
  --   operating. If this is `True`, it will leave the sticky bit set to avoid
  --   cleanup. If it’s `False`, it will unset the sticky bit eveni if the file
  --   previously existed with the sticky bit on, and if it’s `Nothing`, it will
  --   restore any previous sticky bit value (but default to `False` for newly
  --   created files).
  --
  --  __TODO__: Could also set a timer to ensure the access timestamp is updated
  --            periodically.
  Maybe Bool ->
  Path ('Rel 'False) 'File rep ->
  -- | A fallback directory to use if `$XDG_RUNTIME_DIR` isn’t set. If this is
  --   `Nothing`, then `$XDG_RUNTIME_DIR` being unset results in an error
  --   instead of a warning.
  --
  --       If @$XDG_RUNTIME_DIR@ is not set applications should fall back to a
  --       replacement directory with similar capabilities and print a warning
  --       message.
  --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
  Maybe (BaseDirectory rep) ->
  -- | Produce a warning to be presented to the user when `XDG_RUNTIME_DIR`
  --   isn’t set.
  --
  --       If @$XDG_RUNTIME_DIR@ is not set applications should fall back to a
  --       replacement directory with similar capabilities and print a warning
  --       message.
  --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
  (Error rep -> IO ()) ->
  (Handle -> IO a) ->
  IO (These (NonEmpty (Either (InvalidRuntimeDir rep) w)) a)
withRuntimeFile' wf liftErr _preserve filename fallback warning action =
  ( fmap joinThese
      . traverse
        ( fmap (weakenEither . first (pure . pure . liftErr . IOError))
            . wf filename \handle -> do
              -- prevStickyBit <- casStickyBit handle True
              action handle
              -- maybe
              --   (setStickyBit handle prevStickyBit)
              --   (bool (setStickyBit handle False) $ pure ())
              --   preserve
        )
      . joinThese
      <=< traverse
        -- TODO: This should warn if we’re only reading the file (or is that not
        --       good enough?)
        ( fmap (weakenEither . first (pure . Left))
            . getCompose
            . tracing (Compose . verifyRuntimeDir)
        )
      <=< either
        ( \e ->
            maybe
              (This . pure . pure . liftErr $ ConstructionError e)
              (These . pure . pure . liftErr $ ConstructionError e)
              fallback
              <$ warning e
        )
        (pure . pure)
  )
    =<< runtimeDir

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
-- >>> withRuntimeFile @FilePath [posix|myprogram/super-secret.age|] ReadWriteMode Nothing (pure [posix|/run/whatever/|]) (const $ pure ()) pure
-- This (Right (FileError (ConstructionError (Var (...Var "XDG_RUNTIME_DIR"...))) :| [Left InvalidLifetime])
withRuntimeFile ::
  (System.Rep rep, Ord rep) =>
  Path ('Rel 'False) 'File rep ->
  IOWriteMode ->
  Maybe Bool ->
  Maybe (BaseDirectory rep) ->
  (Error rep -> IO ()) ->
  (Handle -> IO a) ->
  IO (These (NonEmpty (Either (InvalidRuntimeDir rep) (WriteError rep))) a)
withRuntimeFile filename mode preserve =
  withRuntimeFile' (flip withFile mode) FileError preserve filename

-- |
--
-- >>> withRuntimeFileRO @FilePath [posix|myprogram/super-secret.age|] (pure [posix|/run/whatever/|]) (const $ pure ()) pure
-- This (Right (ConstructionError (Var (...Var "XDG_RUNTIME_DIR"...)) :| [Left InvalidLifetime])
withRuntimeFileRO ::
  (System.Rep rep, Ord rep) =>
  Path ('Rel 'False) 'File rep ->
  Maybe (BaseDirectory rep) ->
  (Error rep -> IO ()) ->
  (Handle -> IO a) ->
  IO (These (NonEmpty (Either (InvalidRuntimeDir rep) (FileError rep))) a)
withRuntimeFileRO = withRuntimeFile' withFileRO id Nothing

-- | Apply an action to the same file in each directory.
foldDirs ::
  (System.Rep rep, Ord rep) =>
  Path ('Rel 'False) 'File rep ->
  (NonEmpty Handle -> IO a) ->
  NonEmpty (BaseDirectory rep) ->
  -- |
  --
  --  __TODO__: Rewrite this so we have a `These (NonEmpty IOError at the end)`
  IO (These [IOError] a)
foldDirs filename action dirs =
  foldr
    ( \dir act others ->
        either (\ioe -> joinThese . These (pure ioe) <$> act others) pure
          =<< withFileRO filename (act . (: others)) dir
    )
    (maybe (pure $ This mempty) (fmap pure . action) . nonEmpty)
    dirs
    []

-- | Executables can only be written, not read. To _find_ an executable, you
--   should check the @PATH@ environment variable.
--
--       User-specific executable files may be stored in @$HOME@/.local/bin.
--       Distributions should ensure this directory shows up in the UNIX @$PATH@
--       environment variable, at an appropriate place.
--       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
--
-- >>> withExecutableFile "some-script.sh" True pure
-- NotBut (Right {handle: .../home/example-user/.local/bin/some-script.sh})
withExecutableFile ::
  (System.Rep rep, Ord rep) =>
  -- | The name of the executable to write.
  rep ->
  -- | Whether the file should be truncated (`True` → `IO.WriteMode`,
  --   `False` → `IO.AppendMode`).
  Bool ->
  (Handle -> IO a) ->
  -- | Returns @`Noted` ()@ if @$HOME/.local/bin/@ isn’t on @PATH@.
  IO (Annotated () (Either (FileError rep) a))
withExecutableFile filename truncate action =
  -- TODO: Ensure the file is actually executable
  -- TODO: Warn if `binDir` isn’t on Path
  fmap (pure . join) $
    traverse
      ( fmap (first IOError)
          . withFile
            (Directory.selectFile Directory.current filename)
            (if truncate then WriteMode else AppendMode)
            action
      )
      . first ConstructionError
      =<< binDir

data FileError rep
  = ConstructionError (Error rep)
  | IOError IOError
  deriving stock (Eq, Generic, Show, Foldable, Functor, Generic1, Traversable)

data WriteError rep
  = FileError (FileError rep)
  | CreationFailure IOError
  deriving stock (Eq, Generic, Show, Foldable, Functor, Generic1, Traversable)

-- | Why is there no `Monad` for `These`?
joinThese :: (Semigroup a) => These a (These a b) -> These a b
joinThese = \case
  This a -> This a
  That t -> t
  These a (This a') -> This $ a <> a'
  These a (That b) -> These a b
  These a (These a' b) -> These (a <> a') b

-- | `IOMode`, but restricted to modes that involve writing.
data IOWriteMode = WriteMode | AppendMode | ReadWriteMode
  deriving stock (Eq, Generic, Ord, Read, Show)

resolveIOWriteMode :: IOWriteMode -> IOMode
resolveIOWriteMode = \case
  WriteMode -> IO.WriteMode
  AppendMode -> IO.AppendMode
  ReadWriteMode -> IO.ReadWriteMode
