{-# LANGUAGE Safe #-}

-- | The XDG Base Directory specification describes how to access six different
--   categories of file. Here is the API for accessing each of them:
--
-- # data
-- - withTargetFile – for write-only access to a specific user or system data file
-- - withAggregateFiles – for read-only access to a sequence of data files
--
-- # config
-- - withTargetFile – for write-only access to a specific user or system configuration file
-- - withAggregateFiles – for read-only access to a sequence of configuration files
--
-- # state
-- - withUserFile – for write access to a specific state file
-- - withUserFileRO – for read-only access to a specific state file
--
-- # executable
-- - withExecutableFile -- for write-only access to a specific executable file
--
--   Executable files can only be written, and XDG doesn’t allow configuration
--   of the directory used. To read executables, you should check the @PATH@
--   environment variable. This function will warn if you try to write a file
--   and @$HOME/.local/bin/@ isn’t on the @PATH@. Unlike the other types of
--   files, this one doesn’t go into a program-specific subdirectory.
--
-- # cache
-- - withUserFile – for write access to a specific cache file
-- - withUserFileRO – for read-only access to a specific cache file
--
-- # runtime
-- - withRuntimeFile – for write access to a specific runtime file
-- - withRuntimeFileRO – for read-only access to a specific runtime file
--
--   The runtime directory has special requirements, and these operations check
--   them as much as possible. If the requirements aren’t met when trying to
--   write, it errors, rather than writing to an insecure location. If they
--   aren’t met when trying to read, we return a warning.
module XDG.BaseDirectory.Opinionated
  ( User (..),
    Aggregate (..),
    IOWriteMode (..),
    Target (..),
    FileError (..),
    WriteError (..),
    subdirOperations,
    withUserFile,
    withUserFileRO,
    withTargetFile,
    withExecutableFile,
    withAggregateFiles,
    withRuntimeFile,
    withRuntimeFileRO,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False))
import "base" Data.Either (Either)
import "base" Data.Function (flip)
import "base" Data.List.NonEmpty (NonEmpty)
import "base" Data.Maybe (Maybe)
import "base" Data.Ord (Ord)
import "base" System.IO (Handle, IO)
import "pathway" Data.Path (Path, Relativity (Rel), Type (File), (</>))
import qualified "pathway" Data.Path.Directory as Directory
import "these" Data.These (These)
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System
import "this" XDG.BaseDirectory.IO
  ( Aggregate (Config, Data),
    FileError (ConstructionError, IOError),
    IOWriteMode (AppendMode, ReadWriteMode, WriteMode),
    InvalidRuntimeDir,
    Target (System, User),
    User (Cache, State),
    WriteError (CreationFailure, FileError),
    withExecutableFile,
  )
import qualified "this" XDG.BaseDirectory.IO as XDG
import "this" XDG.BaseDirectory.Internal (BaseDirectory, Error)

-- $setup
-- >>> :seti -XQuasiQuotes
-- >>> :seti -XTypeApplications
-- >>> import "base" Control.Applicative (pure, (<*>))
-- >>> import "base" Data.Bool (Bool (True))
-- >>> import "base" Data.Function (const, ($))
-- >>> import "base" Data.Functor ((<$>))
-- >>> import "base" Data.Maybe (Maybe (Nothing))
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
--
-- This duplicates the doctest for `Operations`, because that doesn’t maintain
-- the scope for its fields.
-- >>> let myprogram = subdirOperations "myprogram" $ pure [posix|/run/whatever/|]

-- |
--
--   These operations all have a similar parameter structure
--
--       [Program] [PathContext] RelativeFile [IOMode] [Action]
--
--  To give you an idea where the files referenced live, you can view it as
--  @/PathContext/Program/RelativeFile@. So a call like @`withTargetDir`
--  myprogram `User` `Config` [posix|settings.dhall|]@ would be mapped like
--  @/User/Config/myprogram/settings.dhall@, which becomes a literal path like
--  @$HOME/.config/myprogram/settings.dhall@, assuming the default values.
--
--  The __Program__ is actually a record of these operations, but it fixes the
--  particular subdirectory within each XDG Base Directory that your program
--  will access. `withExecutableFile` doesn’t have a Program parameter, because
--  all executables are placed into the same directory.
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
--
-- >>> let myprogram = subdirOperations "myprogram" $ pure [posix|/run/whatever/|]
data Operations rep = Operations
  { -- |
    --
    -- >>> withUserFile myprogram State [posix|archive.db|] ReadWriteMode pure
    -- These (FileError (ConstructionError (Var (...Var "XDG_STATE_HOME"...)) :| []) {handle: .../home/example-user/.local/state/myprogram/archive.db}
    withUserFile ::
      forall a.
      User ->
      Path ('Rel 'False) 'File rep ->
      IOWriteMode ->
      (Handle -> IO a) ->
      IO (These (NonEmpty (WriteError rep)) a),
    -- |
    --
    -- >>> withUserFileRO myprogram State [posix|archive.db|] pure
    -- This (ConstructionError (Var (...Var "XDG_STATE_HOME"...) :| [IOError .../home/example-user/.local/state/myprogram/archive.db: withFile: does not exist (No such file or directory)])
    withUserFileRO ::
      forall a.
      User ->
      Path ('Rel 'False) 'File rep ->
      (Handle -> IO a) ->
      IO (These (NonEmpty (FileError rep)) a),
    -- | Open all of the associated files. This can only open files for reading.
    --   To write to (some of) these files, use `withTargetFile`.
    --
    --        A specification that refers to `$XDG_DATA_DIRS` or
    --        `$XDG_CONFIG_DIRS` should define what the behaviour must be when a
    --        file is located under multiple base directories. It could, for
    --        example, define that only the file under the most important base
    --        directory should be used or, as another example, it could define
    --        rules for merging the information from the different files.
    --        —[§4](https://specifications.freedesktop.org/basedir-spec/0.8/#referencing)
    --
    -- >>> withAggregateFiles myprogram Config [posix|settings.dhall|] pure
    -- This [ConstructionError (Var (...Var "XDG_CONFIG_HOME"...),ConstructionError (Var (MissingVar "XDG_CONFIG_DIRS" Nothing)),IOError .../home/example-user/.config/myprogram/settings.dhall: withFile: does not exist (No such file or directory),IOError /etc/xdg/myprogram/settings.dhall: withFile: does not exist (No such file or directory)]
    --
    -- >>> withAggregateFiles myprogram Data [posix|resources/splash.png|] pure
    -- This [ConstructionError (Var (...Var "XDG_DATA_HOME"...),ConstructionError (Var (EmptyVar "XDG_DATA_DIRS")),IOError .../home/example-user/.local/share/myprogram/resources/splash.png: withFile: does not exist (No such file or directory),IOError /usr/local/share/myprogram/resources/splash.png: withFile: does not exist (No such file or directory),IOError /usr/share/myprogram/resources/splash.png: withFile: does not exist (No such file or directory)]
    withAggregateFiles ::
      forall a.
      Aggregate ->
      Path ('Rel 'False) 'File rep ->
      (NonEmpty Handle -> IO a) ->
      IO (These [FileError rep] a),
    -- | This can only write to the targeted file. To read, use
    --   `withAggregateFiles` to access all of the related files.
    --
    -- >>> withTargetFile myprogram User Config [posix|settings.dhall|] False pure
    -- These (FileError (ConstructionError (Var (...Var "XDG_CONFIG_HOME"...)) :| []) {handle: .../home/example-user/.config/myprogram/settings.dhall}
    withTargetFile ::
      forall a.
      Target ->
      Aggregate ->
      Path ('Rel 'False) 'File rep ->
      Bool ->
      (Handle -> IO a) ->
      IO (These (NonEmpty (WriteError rep)) a),
    -- |
    --
    --       If @$XDG_RUNTIME_DIR@ is not set applications should fall back to a
    --       replacement directory with similar capabilities and print a warning
    --       message. Applications should use this directory for communication
    --       and synchronization purposes and should not place larger files in
    --       it, since it might reside in runtime memory and cannot necessarily
    --       be swapped out to disk.
    --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
    --
    -- >>> withRuntimeFile myprogram [posix|super-secret.age|] ReadWriteMode Nothing (const $ pure ()) pure
    -- This (Right (FileError (ConstructionError (Var (...Var "XDG_RUNTIME_DIR"...))) :| [Left InvalidLifetime])
    withRuntimeFile ::
      forall a.
      Path ('Rel 'False) 'File rep ->
      IOWriteMode ->
      Maybe Bool ->
      (Error rep -> IO ()) ->
      (Handle -> IO a) ->
      IO (These (NonEmpty (Either (InvalidRuntimeDir rep) (WriteError rep))) a),
    withRuntimeFileRO ::
      forall a.
      Path ('Rel 'False) 'File rep ->
      (Error rep -> IO ()) ->
      (Handle -> IO a) ->
      IO (These (NonEmpty (Either (InvalidRuntimeDir rep) (FileError rep))) a)
  }

-- | This is the recommended interface with this library. You can call this
--   function once and then use the members throughout your program.
--
-- >>> myprogram = subdirOperations "myprogram" $ pure [posix|/run/whatever/|]
subdirOperations ::
  forall rep.
  (System.Rep rep, Ord rep) =>
  -- | The program directory component.
  rep ->
  -- | A fallback directory to use for `withRuntimeFile` and `withRuntimeFileRO`
  --   if `$XDG_RUNTIME_DIR` isn’t set. If this is `Nothing`, then
  --   `$XDG_RUNTIME_DIR` being unset results in an error instead of a warning.
  --
  --       If @$XDG_RUNTIME_DIR@ is not set applications should fall back to a
  --       replacement directory with similar capabilities and print a warning
  --       message.
  --       —[§3](https://specifications.freedesktop.org/basedir-spec/latest/#variables)
  Maybe (BaseDirectory rep) ->
  Operations rep
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
          withTargetFile = \target -> injectSubdir . XDG.withTargetFile target,
          withRuntimeFile = \filename mode ->
            flip
              (injectSubdir XDG.withRuntimeFile filename mode)
              runtimeFallback,
          withRuntimeFileRO = \filename ->
            injectSubdir XDG.withRuntimeFileRO filename runtimeFallback
        }
