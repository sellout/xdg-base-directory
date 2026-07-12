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
--   and @$HOME\/.local\/bin\/@ isn’t on the @PATH@. Unlike the other types of
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
module XDG.BaseDirectory.Opinionated
  ( User (..),
    Aggregate (..),
    AggregateDirWarnings,
    IOWriteMode (..),
    Operations,
    bleedingOperations,
    subdirOperations,
    withUserFile,
    withUserFileRO,
    withSystemTargetFile,
    withUserTargetFile,
    withExecutableFile,
    withAggregateFiles,
    withRuntimeFile,
    withRuntimeFileRO,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Control.Monad.IO.Class (MonadIO)
import safe "base" Data.Bool (Bool (False))
import safe "base" Data.Either (Either)
import safe "base" Data.Function (flip)
import safe qualified "base" Data.Kind as Kind
import safe "base" Data.List.NonEmpty (NonEmpty)
import safe "base" Data.Maybe (Maybe)
import safe "base" System.IO (Handle)
import safe "exceptions" Control.Monad.Catch (MonadMask)
import safe "pathway" Data.Path (Path, Relativity (Abs, Rel), Type (Dir, File), (</>))
import safe qualified "pathway" Data.Path.Directory as Directory
import safe "pathway-compat-base" System.IO.Pathway (OpenFileFailure)
import safe qualified "pathway-system" System.Path as Path
import safe qualified "pathway-system" System.Text as Text
import "variant" Data.Variant (V)
import qualified "variant" Data.Variant.Types as Variant
import safe "this" Data.Annotated (Annotated)
import safe "this" XDG.BaseDirectory (BaseDirectory, Error)
import safe "this" XDG.BaseDirectory.IO
  ( Aggregate (Config, Data),
    AggregateDirWarnings,
    IOWriteMode (AppendMode, ReadWriteMode, WriteMode),
    InvalidRuntimeDir,
    User (Cache, State),
    WithFileFailure,
  )
import safe qualified "this" XDG.BaseDirectory.IO as XDG

-- $setup
-- >>> :seti -XQuasiQuotes
-- >>> :seti -XTypeApplications
-- >>> import "base" Control.Applicative (pure, (<*>))
-- >>> import "base" Data.Bool (Bool (True))
-- >>> import "base" Data.Function (const, ($))
-- >>> import "base" Data.Functor (fmap, (<$>))
-- >>> import "base" Data.Maybe (Maybe (Nothing))
-- >>> import "base" Data.String (String)
-- >>> import "base" Data.Tuple (snd)
-- >>> import "base" System.Environment (setEnv)
-- >>> import "base" System.IO (IO)
-- >>> import "pathway" Data.Path (toText)
-- >>> import "pathway" Data.Path.TH (posix)
-- >>> import qualified "pathway" Data.Path.Format as Format
-- >>> import "pathway-compat-temporary" System.IO.Temp.Overlay (createTempDirectory, getCanonicalTemporaryDirectory)
-- >>> import "pathway-system" System.Path (createDirectoryWithParentsIfMissing)
--
-- __TODO__: Extract this to a testing package.
-- >>> tempBase <- getCanonicalTemporaryDirectory
-- >>> tempRoot <- createTempDirectory tempBase "xdg-base-directory-haskell-doctest"
-- >>> tempHome = (tempRoot `Directory.descendTo`) "home" `Directory.descendTo` "test-user"
-- >>> createDirectoryWithParentsIfMissing tempHome
-- Right ()
-- >>> setEnv "HOME" $ toText Format.local tempHome
-- >>> setEnv "XDG_CACHE_HOME" . toText Format.local $ tempHome `Directory.descendTo` ".cache"
-- >>> setEnv "XDG_CONFIG_DIRS" . toText Format.local $ (tempRoot `Directory.descendTo` "etc") `Directory.descendTo` "xdg"
-- >>> setEnv "XDG_CONFIG_HOME" . toText Format.local $ tempHome `Directory.descendTo` ".config"
-- >>> setEnv "XDG_DATA_DIRS" . toText Format.local $ (tempRoot `Directory.descendTo` "usr") `Directory.descendTo` "share"
-- >>> setEnv "XDG_DATA_HOME" . toText Format.local $ (tempHome `Directory.descendTo` ".local") `Directory.descendTo` "share"
-- >>> setEnv "XDG_STATE_HOME" . toText Format.local $ (tempHome `Directory.descendTo` ".local") `Directory.descendTo` "state"
-- >>> let runtimeDir = (((tempRoot `Directory.descendTo` "var") `Directory.descendTo` "run")  `Directory.descendTo` "user") `Directory.descendTo` "0"
-- >>> createDirectoryWithParentsIfMissing runtimeDir
-- Right ()
-- >>> setEnv "XDG_RUNTIME_DIR" $ toText Format.local runtimeDir

-- |
--
--   These operations all have a similar parameter structure
--
--       \[Program] [PathContext] RelativeFile [IOMode] [Action]
--
--   To give you an idea where the files referenced live, you can view it as
--   @\/PathContext\/Program\/RelativeFile@. So a call like
--
-- > withTargetDir myprogram User Config [posix|settings.dhall|]
--
--   would be mapped like @\/User\/Config\/myprogram\/settings.dhall@, which
--   becomes a literal path like @$HOME\/.config\/myprogram\/settings.dhall@,
--   assuming the default values.
--
--   The __Program__ is actually a record of these operations, but it fixes the
--   particular subdirectory within each XDG Base Directory that your program
--   will access. `withExecutableFile` doesn’t have a Program parameter, because
--   all executables are placed into the same directory.
--
--   The __PathContext__ says what part of the XDG Base Directory structure
--   we’re trying to access. It varies depending on the call. In some (like
--   `withRuntimeFile`) the PathContext is implied by the function. In others,
--   it might be one or more arguments, like `State` or @`User` `Config`@.
--
--   The __RelativeFile__ is the path to a file within your particular
--   subdirectory that you want to access. This is almost always a relative file
--   path, but for `withExecutableFile`, it’s simply a single path component
--   (because all executables are stored at the top level of its directory).
--
--   The __IOMode__ also varies between calls. It may not exist (if the function
--   can only read), it may be a `Bool` if the function can only write (`True`
--   means to truncate before writing), it may be an `IOWriteMode` which is a
--   subset of `IOMode` that only allows modes that involve writing (this is
--   because we have different types for operations that write (and possibly
--   read) and ones that only read).
--
--   Finally, the __Action__ is a function for manipulating the file handle(s)
--   while they’re open.
data Operations (m :: Kind.Type -> Kind.Type) (rep :: Kind.Type) = Operations
  { -- | Write (or read\/write) a file in either the `Cache` or `State` directory.
    withUserFile ::
      forall a.
      User ->
      Path ('Rel 'False) 'File rep ->
      IOWriteMode ->
      (Path 'Abs 'File rep -> Handle -> m a) ->
      m
        ( Either
            ( V
                ( (Error rep, V (Path.GetUserDirectoryFailure rep))
                    ': WithFileFailure
                )
            )
            (Annotated (Error rep) a)
        ),
    -- | Read a file in either the `Cache` or `State` directory.
    withUserFileRO ::
      forall a.
      User ->
      Path ('Rel 'False) 'File rep ->
      (Path 'Abs 'File rep -> Handle -> m a) ->
      m
        ( Either
            (Error rep, V (Path.GetUserDirectoryFailure rep))
            (Annotated (Error rep) (Either (V OpenFileFailure) a))
        ),
    -- | Open the set of `Config` or `Data` files for reading.
    --   To write to (some of) these files, use `withTargetFile`.
    --
    --        A specification that refers to @$XDG_DATA_DIRS@ or
    --        @$XDG_CONFIG_DIRS@ should define what the behaviour must be when a
    --        file is located under multiple base directories. It could, for
    --        example, define that only the file under the most important base
    --        directory should be used or, as another example, it could define
    --        rules for merging the information from the different files.
    --        —[§4](https://specifications.freedesktop.org/basedir-spec/0.8/#referencing)
    withAggregateFiles ::
      forall a.
      Aggregate ->
      Path ('Rel 'False) 'File rep ->
      ([(Path 'Abs 'File rep, Handle)] -> m a) ->
      m
        ( Annotated
            (AggregateDirWarnings rep)
            (Annotated (NonEmpty (V OpenFileFailure)) a)
        ),
    withSystemTargetFile ::
      forall a.
      Aggregate ->
      Path ('Rel 'False) 'File rep ->
      Bool ->
      (Path 'Abs 'File rep -> Handle -> m a) ->
      m (Annotated (Error rep) (Either (V WithFileFailure) a)),
    -- | This can only write to the targeted file. To read, use
    --   `withAggregateFiles` to access all of the related files.
    --
    --   The available targeted files are
    -- - `User` `Config`
    -- - `System` `Config`
    -- - `User` `Data`
    -- - `System` `Data`
    --
    --  __FIXME__: This should probably use `IOWriteMode`, as we want to be able
    --             to _modify_ existing files in these places, and there’s no
    --             guarantee if we use `withAggregateFiles` that we will match
    --             up with the corresponding file.
    withUserTargetFile ::
      forall a.
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
        ),
    -- | Open a temporary file for writing (or read/write).
    --
    --       If @$XDG_RUNTIME_DIR@ is not set applications should fall back to a
    --       replacement directory with similar capabilities and print a warning
    --       message. Applications should use this directory for communication
    --       and synchronization purposes and should not place larger files in
    --       it, since it might reside in runtime memory and cannot necessarily
    --       be swapped out to disk.
    --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
    withRuntimeFile ::
      forall a.
      Path ('Rel 'False) 'File rep ->
      IOWriteMode ->
      Maybe Bool ->
      (Path 'Abs 'File rep -> Handle -> m a) ->
      m (Either (V (InvalidRuntimeDir rep ': WithFileFailure)) a),
    -- | Open a temporary file read-only.
    withRuntimeFileRO ::
      forall a.
      Path ('Rel 'False) 'File rep ->
      (Path 'Abs 'File rep -> Handle -> m a) ->
      m (Either (V (InvalidRuntimeDir rep ': WithFileFailure)) a),
    withExecutableFile ::
      forall a.
      rep ->
      Bool ->
      (Path 'Abs 'File rep -> Handle -> m a) ->
      m
        ( Annotated
            ()
            ( Either
                ( V
                    ( Variant.Concat
                        WithFileFailure
                        (Path.GetUserDirectoryFailure rep)
                    )
                )
                a
            )
        )
  }

type role Operations representational nominal

-- | This allows writing files directly to the XDG base directories, with no
--   program-specific subdir intervening. It’s not recommended, but may be
--   necessary for accessing existing standardized paths (like XDG User Dirs).
--
-- >>> let xdgOps = bleedingOperations @IO @String . const $ pure [posix|/run/whatever/|]
--
-- >>> withUserFile xdgOps State [posix|myprogram-archive.db|] ReadWriteMode $ const pure
-- Right (NotBut {handle: .../home/test-user/.local/state/myprogram-archive.db})
--
-- >>> withUserFileRO xdgOps State [posix|myprogram-archive.db|] $ const pure
-- Right (NotBut (Right {handle: .../home/test-user/.local/state/myprogram-archive.db}))
--
-- >>> withAggregateFiles xdgOps Config [posix|myprogram-settings.dhall|] $ pure . fmap snd
-- NotBut (Noted (DoesNotExistError .../etc/xdg/myprogram-settings.dhall: openFile: does not exist (No such file or directory) :| [DoesNotExistError .../home/test-user/.config/myprogram-settings.dhall: openFile: does not exist (No such file or directory)]) [])
--
-- >>> withAggregateFiles xdgOps Data [posix|resources/splash.png|] $ pure . fmap snd
-- NotBut (Noted (DoesNotExistError .../usr/share/resources/splash.png: openFile: does not exist (No such file or directory) :| [DoesNotExistError .../home/test-user/.local/share/resources/splash.png: openFile: does not exist (No such file or directory)]) [])
--
-- >>> withUserTargetFile xdgOps Config [posix|settings.dhall|] False $ const pure
-- Right (NotBut {handle: .../home/test-user/.config/settings.dhall})
--
-- >>> withRuntimeFile xdgOps [posix|super-secret.age|] ReadWriteMode Nothing $ const pure
-- Right {handle: .../var/run/user/0/super-secret.age}
bleedingOperations ::
  (MonadIO m, MonadMask m, Path.Operations rep 'Dir, Path.Rep rep, Text.Rep rep) =>
  -- | A fallback directory to use for `withRuntimeFile` and `withRuntimeFileRO`
  --   if @$XDG_RUNTIME_DIR@ isn’t set. If this is `Nothing`, then
  --   @$XDG_RUNTIME_DIR@ being unset results in an error instead of a warning.
  --
  --       If @$XDG_RUNTIME_DIR@ is not set applications should fall back to a
  --       replacement directory with similar capabilities and print a warning
  --       message.
  --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
  --
  --  __NB__: This directory should already be application-specific. The
  --          provided directory component won’t be appended to it.
  (Error rep -> m (BaseDirectory rep)) ->
  Operations m rep
bleedingOperations runtimeFallback =
  Operations
    { withUserFile = XDG.withUserFile,
      withUserFileRO = XDG.withUserFileRO,
      withAggregateFiles = XDG.withAggregateFiles,
      withSystemTargetFile = XDG.withSystemTargetFile,
      withUserTargetFile = XDG.withUserTargetFile,
      withRuntimeFile = \filename mode ->
        flip (XDG.withRuntimeFile filename mode) runtimeFallback,
      withRuntimeFileRO = (`XDG.withRuntimeFileRO` runtimeFallback),
      withExecutableFile = XDG.withExecutableFile
    }

-- | This is the recommended interface with this library. You can call this
--   function once and then use the members throughout your program.
--
--  __NB__: `withExecutableFile` doesn’t use the program subdirectory.
--
-- >>> myprogram = subdirOperations "myprogram" . const $ pure [posix|/run/whatever/|]
--
-- >>> withUserFile myprogram State [posix|archive.db|] ReadWriteMode $ const pure
-- Right (NotBut {handle: .../home/test-user/.local/state/myprogram/archive.db})
--
-- >>> withUserFileRO myprogram State [posix|archive.db|] $ const pure
-- Right (NotBut (Right {handle: .../home/test-user/.local/state/myprogram/archive.db}))
--
--  __NOTE__: Most operations should create any missing directories, but
--            `withRuntimeFile must /not/ create the runtime dir. However, if
--            the runtime dir exists, it /should/ attempt to create the
--            program-specific subdirectory (and any directories in the relative
--            path to the file).
--
-- >>> withRuntimeFile myprogram [posix|some-temp-file.txt|] WriteMode Nothing $ const pure
-- Right {handle: .../var/run/user/0/myprogram/some-temp-file.txt}
--
-- >>> withAggregateFiles myprogram Config [posix|settings.dhall|] $ pure . fmap snd
-- NotBut (Noted (DoesNotExistError .../etc/xdg/myprogram/settings.dhall: openFile: does not exist (No such file or directory) :| [DoesNotExistError .../home/test-user/.config/myprogram/settings.dhall: openFile: does not exist (No such file or directory)]) [])
--
-- >>> withAggregateFiles myprogram Data [posix|resources/splash.png|] $ pure . fmap snd
-- NotBut (Noted (DoesNotExistError .../usr/share/myprogram/resources/splash.png: openFile: does not exist (No such file or directory) :| [DoesNotExistError .../home/test-user/.local/share/myprogram/resources/splash.png: openFile: does not exist (No such file or directory)]) [])
--
-- >>> withUserTargetFile myprogram Config [posix|settings.dhall|] False $ const pure
-- Right (NotBut {handle: .../home/test-user/.config/myprogram/settings.dhall})
--
-- >>> withRuntimeFile myprogram [posix|super-secret.age|] ReadWriteMode Nothing $ const pure
-- Right {handle: .../var/run/user/0/myprogram/super-secret.age}
subdirOperations ::
  forall m rep.
  (MonadIO m, MonadMask m, Path.Operations rep 'Dir, Path.Rep rep, Text.Rep rep) =>
  -- | The program directory component. This is the subdirectory to restrict
  --   everything to within each XDG base directory.
  rep ->
  -- | A fallback directory to use for `withRuntimeFile` and `withRuntimeFileRO`
  --   if @$XDG_RUNTIME_DIR@ isn’t set. If this is `Nothing`, then
  --   @$XDG_RUNTIME_DIR@ being unset results in an error instead of a warning.
  --
  --       If @$XDG_RUNTIME_DIR@ is not set applications should fall back to a
  --       replacement directory with similar capabilities and print a warning
  --       message.
  --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
  --
  --  __NB__: This directory should already be application-specific. The
  --          provided directory component won’t be appended to it.
  (Error rep -> m (BaseDirectory rep)) ->
  Operations m rep
subdirOperations subdir runtimeFallback =
  let injectSubdir ::
        forall k.
        (Path ('Rel 'False) 'File rep -> k) ->
        Path ('Rel 'False) 'File rep ->
        k
      injectSubdir fn = fn . (Directory.descendTo Directory.current subdir </>)
   in Operations
        { withUserFile = injectSubdir . XDG.withUserFile,
          withUserFileRO = injectSubdir . XDG.withUserFileRO,
          withAggregateFiles = injectSubdir . XDG.withAggregateFiles,
          withSystemTargetFile = injectSubdir . XDG.withSystemTargetFile,
          withUserTargetFile = injectSubdir . XDG.withUserTargetFile,
          withRuntimeFile = \filename mode ->
            flip
              (injectSubdir XDG.withRuntimeFile filename mode)
              runtimeFallback,
          withRuntimeFileRO = \filename ->
            injectSubdir XDG.withRuntimeFileRO filename runtimeFallback,
          withExecutableFile = XDG.withExecutableFile
        }
