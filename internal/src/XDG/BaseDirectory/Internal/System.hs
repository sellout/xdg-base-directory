{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

module XDG.BaseDirectory.Internal.System
  ( Rep (..),
  )
where

import safe "base" Control.Category (id)
import safe "base" Data.Bool (Bool)
import safe qualified "base" Data.Char as Base
import safe "base" Data.Maybe (Maybe)
import safe "base" Data.String (String)
import safe qualified "base" System.Environment as F.Env
import safe "base" System.IO (Handle, IO, IOMode)
import safe qualified "base" System.IO as F.IO
import qualified "directory" System.Directory as F.Dir
import safe qualified "filepath" System.FilePath as F.Path

-- TODO: `OsString` was introduced in filepath-1.4.100, but there was no
--       `unsafeEncodeUtf` until 1.5.2, so this (for now) uses the later
--       constraint. We can make some changes here to work back to 1.4.100,
--       though.
#if MIN_VERSION_filepath(1, 4, 100)
import safe "base" Control.Category ((.))
import "base" System.IO.Unsafe (unsafePerformIO)
import qualified "directory" System.Directory.Internal as O.In
import qualified "directory" System.Directory.OsPath as O.Dir
import qualified "file-io" System.File.OsPath as O.IO
import "filepath" System.OsPath (OsString, OsChar)
import qualified "filepath" System.OsPath as O.Path
#endif

-- | This wraps the underlying operations we use, so we can parameterize over
--   string types.
class Rep a where
  type Char a
  createDirectoryIfMissing :: Bool -> a -> IO ()
  doesDirectoryExist :: a -> IO Bool
  fromStringLiteral :: String -> a
  getHomeDirectory :: IO a
  isValid :: a -> Bool
  joinPath :: [a] -> a
  lookupEnv :: a -> IO (Maybe a)
  pack :: [Char a] -> a
  pathSeparator :: proxy a -> Char a
  splitDirectories :: a -> [a]
  splitDrive :: a -> (a, a)
  splitFileName :: a -> (a, a)
  splitSearchPath :: a -> [a]
  withFile :: a -> IOMode -> (Handle -> IO r) -> IO r
  (</>) :: a -> a -> a

instance Rep String where
  type Char String = Base.Char
  createDirectoryIfMissing = F.Dir.createDirectoryIfMissing
  doesDirectoryExist = F.Dir.doesDirectoryExist
  fromStringLiteral = id
  getHomeDirectory = F.Dir.getHomeDirectory
  isValid = F.Path.isValid
  lookupEnv = F.Env.lookupEnv
  joinPath = F.Path.joinPath
  pack = id
  pathSeparator _ = F.Path.pathSeparator
  splitDirectories = F.Path.splitDirectories
  splitDrive = F.Path.splitDrive
  splitFileName = F.Path.splitFileName
  splitSearchPath = F.Path.splitSearchPath
  withFile = F.IO.withFile
  (</>) = (F.Path.</>)

#if MIN_VERSION_filepath(1, 4, 100)
instance Rep OsString where
  type Char OsString = OsChar
  createDirectoryIfMissing = O.Dir.createDirectoryIfMissing
  doesDirectoryExist = O.Dir.doesDirectoryExist
  fromStringLiteral = unsafePerformIO . O.Path.encodeUtf
  getHomeDirectory = O.Dir.getHomeDirectory
  isValid = O.Path.isValid
  joinPath = O.Path.joinPath
  lookupEnv = O.In.lookupEnvOs
  pack = O.Path.pack
  pathSeparator _ = O.Path.pathSeparator
  splitDirectories = O.Path.splitDirectories
  splitDrive = O.Path.splitDrive
  splitFileName = O.Path.splitFileName
  splitSearchPath = O.Path.splitSearchPath
  withFile = O.IO.withFile
  (</>) = (O.Path.</>)
#endif
