{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Tests for the @xdg-user-dirs-update@ executable.
module Main (main) where

import "base" Control.Applicative (pure)
import "base" Control.Monad (unless)
import "base" Data.Bool (Bool (False, True), otherwise, (&&))
import "base" Data.Eq ((==))
import "base" Data.Function (($))
import "base" Data.Int (Int)
import "base" Data.List (isInfixOf)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" System.Environment (getExecutablePath)
import "base" System.Exit (ExitCode (..), exitFailure)
import "base" System.IO (IO)
import qualified "base" System.IO as IO
import "directory" System.Directory
  ( createDirectoryIfMissing,
    getHomeDirectory,
    getTemporaryDirectory,
    removeDirectoryRecursive,
  )
import "filepath" System.FilePath (takeDirectory, (</>))
import "process" System.Process (readProcessWithExitCode)
import "base" Prelude (seq, (+), (-))

-- | Find the xdg-user-dirs-update executable relative to this test executable.
-- Both are built in the same dist-newstyle structure.
getXdgUserDirsUpdatePath :: IO String
getXdgUserDirsUpdatePath = do
  testExe <- getExecutablePath
  -- testExe is like .../build/xdg-user-dirs-update-test/xdg-user-dirs-update-test
  -- We need        .../build/xdg-user-dirs-update/xdg-user-dirs-update
  let buildDir = takeDirectory (takeDirectory testExe)
  pure $ buildDir </> "xdg-user-dirs-update" </> "xdg-user-dirs-update"

-- | Run xdg-user-dirs-update with given arguments.
runXdgUserDirsUpdate :: String -> [String] -> IO (String, String)
runXdgUserDirsUpdate exe args = do
  (_, out, err) <- readProcessWithExitCode exe args ""
  pure (out, err)

-- | Run xdg-user-dirs-update with given arguments and return exit code.
runXdgUserDirsUpdateWithExit :: String -> [String] -> IO (ExitCode, String, String)
runXdgUserDirsUpdateWithExit exe args = readProcessWithExitCode exe args ""

-- | Assert a condition, failing with message if false.
assert :: String -> Bool -> IO ()
assert msg condition = unless condition do
  IO.hPutStrLn IO.stderr $ "FAIL: " <> msg
  exitFailure

-- | Report test success.
pass :: String -> IO ()
pass msg = IO.putStrLn $ "PASS: " <> msg

-- | Read file contents strictly.
readFileStrict :: String -> IO String
readFileStrict path = do
  contents <- IO.readFile path
  -- Force evaluation
  seq (length contents) (pure contents)
  where
    length :: String -> Int
    length [] = 0
    length (_ : xs) = 1 + length xs

-- | Count occurrences of a substring in a string.
countOccurrences :: String -> String -> Int
countOccurrences needle = go 0
  where
    needleLen = length needle
    go count [] = count
    go count str@(_ : rest)
      | needle `isPrefixOf` str = go (count + 1) (drop needleLen str)
      | otherwise = go count rest
    length :: String -> Int
    length [] = 0
    length (_ : xs) = 1 + length xs
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys
    drop :: Int -> String -> String
    drop 0 xs = xs
    drop _ [] = []
    drop n (_ : xs) = drop (n - 1) xs

-- | The test suite’s entry point.
--
-- @since 0.0.1.0
main :: IO ()
main = do
  IO.putStrLn "=== Testing xdg-user-dirs-update ==="
  exe <- getXdgUserDirsUpdatePath
  IO.putStrLn $ "Using executable: " <> exe

  -- Create temp directory for tests
  tmpBase <- getTemporaryDirectory
  let tempDir = tmpBase </> "xdg-user-dirs-test"
  createDirectoryIfMissing True tempDir

  -- Test --help
  IO.putStrLn "Test: --help"
  (out1, _) <- runXdgUserDirsUpdate exe ["--help"]
  assert "Help message missing 'Usage'" $ "Usage" `isInfixOf` out1
  assert "Help message missing '--set'" $ "--set" `isInfixOf` out1
  pass "Shows help with --set option"

  -- Test --dummy-output
  IO.putStrLn "Test: --dummy-output"
  let config1 = tempDir </> "config1.dirs"
  _ <- runXdgUserDirsUpdate exe ["--dummy-output", config1]
  contents1 <- readFileStrict config1
  assert ("Config missing XDG_DESKTOP_DIR got: " <> contents1) $ "XDG_DESKTOP_DIR" `isInfixOf` contents1
  pass "Config file created with XDG_DESKTOP_DIR"

  -- Test --set with relative path (should fail)
  IO.putStrLn "Test: --set with relative path should fail"
  let config2 = tempDir </> "config2.dirs"
  (exitCode2, _, err2) <- runXdgUserDirsUpdateWithExit exe ["--set", "DESKTOP", "MyNewDesktop", "--dummy-output", config2]
  assert "Relative path should cause exit failure" $ exitCode2 == ExitFailure 1
  assert ("Error message should mention absolute path: " <> err2) $ "absolute" `isInfixOf` err2
  pass "Relative path rejected with error"

  -- Test --set with absolute path
  IO.putStrLn "Test: --set with absolute path"
  let config3 = tempDir </> "config3.dirs"
  _ <- runXdgUserDirsUpdate exe ["--set", "DOWNLOAD", "/tmp/my-downloads", "--dummy-output", config3]
  contents3 <- readFileStrict config3
  assert "DOWNLOAD not set to /tmp/my-downloads" $
    "XDG_DOWNLOAD_DIR=\"/tmp/my-downloads\"" `isInfixOf` contents3
  pass "DOWNLOAD set to /tmp/my-downloads"

  -- Test --set with home directory path (should convert to $HOME)
  IO.putStrLn "Test: --set with home directory path converts to $HOME"
  homeDir <- getHomeDirectory
  let config4 = tempDir </> "config4.dirs"
  _ <- runXdgUserDirsUpdate exe ["--set", "VIDEOS", homeDir </> "MyVideos", "--dummy-output", config4]
  contents4 <- readFileStrict config4
  assert "VIDEOS not converted to $HOME/MyVideos" $
    "XDG_VIDEOS_DIR=\"$HOME/MyVideos\"" `isInfixOf` contents4
  pass "Home directory path converted to $HOME/MyVideos"

  -- FIXME: Need to carefully test without `--dummy-output` by redirecting
  --        `$HOME.
  -- -- Test --set without --dummy-output shows warning
  -- IO.putStrLn "Test: --set without --dummy-output shows warning"
  -- let config4 = tempDir </> "config4.dirs"
  -- result <- runXdgUserDirsUpdate exe ["--set", "DESKTOP", "foo"]
  -- if [[ "$result" == *"Warning"* ]] && [[ "$result" == *"--dummy-output"* ]]; then
  --     echo "  PASS: Shows warning about missing --dummy-output"
  -- else
  --     echo "  FAIL: No warning shown"
  --     exit 1
  -- fi

  -- Test that config preserves existing entries
  IO.putStrLn "Test: --set preserves existing entries"
  let config5 = tempDir </> "config5.dirs"
  _ <- runXdgUserDirsUpdate exe ["--set", "NEWDIR", homeDir </> "TestDir", "--dummy-output", config5]
  contents5 <- readFileStrict config5
  let desktopCount = countOccurrences "XDG_DESKTOP_DIR" contents5
      newdirCount = countOccurrences "XDG_NEWDIR_DIR" contents5
  assert "Entry counts wrong" $ desktopCount == 1 && newdirCount == 1
  pass "Both existing and new entries present"

  -- Cleanup
  removeDirectoryRecursive tempDir

  IO.putStrLn ""
  IO.putStrLn "=== All xdg-user-dirs-update tests passed! ==="
