{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Tests for the @xdg-user-dir@ executable.
module Main (main) where

import "base" Control.Applicative (pure)
import "base" Control.Monad (unless)
import "base" Data.Bool (Bool, not, (||))
import "base" Data.Foldable (null)
import "base" Data.Function (($))
import "base" Data.List (isInfixOf)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" System.Environment (getExecutablePath)
import "base" System.Exit (exitFailure)
import "base" System.IO (IO)
import qualified "base" System.IO as IO
import "filepath" System.FilePath (takeDirectory, (</>))
import "process" System.Process (readProcessWithExitCode)

-- | Find the xdg-user-dir executable relative to this test executable.
-- Both are built in the same dist-newstyle structure.
getXdgUserDirPath :: IO String
getXdgUserDirPath = do
  testExe <- getExecutablePath
  -- testExe is like .../build/xdg-user-dir-test/xdg-user-dir-test
  -- We need        .../build/xdg-user-dir/xdg-user-dir
  let buildDir = takeDirectory (takeDirectory testExe)
  pure $ buildDir </> "xdg-user-dir" </> "xdg-user-dir"

-- | Run xdg-user-dir with given arguments.
runXdgUserDir :: String -> [String] -> IO (String, String)
runXdgUserDir exe args = do
  (_, out, err) <- readProcessWithExitCode exe args ""
  pure (out, err)

-- | Assert a condition, failing with message if false.
assert :: String -> Bool -> IO ()
assert msg condition = unless condition do
  IO.hPutStrLn IO.stderr $ "FAIL: " <> msg
  exitFailure

-- | Report test success.
pass :: String -> IO ()
pass msg = IO.putStrLn $ "PASS: " <> msg

-- | The test suite’s entry point.
--
-- @since 0.0.1.0
main :: IO ()
main = do
  IO.putStrLn "=== Testing xdg-user-dir ==="
  exe <- getXdgUserDirPath
  IO.putStrLn $ "Using executable: " <> exe

  -- Test basic directory lookup (DESKTOP)
  IO.putStrLn "Test: xdg-user-dir DESKTOP"
  (out1, err1) <- runXdgUserDir exe ["DESKTOP"]
  assert ("Expected path containing 'Desktop' or HOME, but got " <> out1 <> " with " <> err1) $
    "Desktop" `isInfixOf` out1
  pass $ "Got " <> out1

  -- Test DOCUMENTS directory
  IO.putStrLn "Test: xdg-user-dir DOCUMENTS"
  (out2, _) <- runXdgUserDir exe ["DOCUMENTS"]
  assert "Got empty result for DOCUMENTS" $ not (null out2)
  pass $ "Got " <> out2

  -- Test usage message (no args)
  IO.putStrLn "Test: xdg-user-dir (no args)"
  (out3, err3) <- runXdgUserDir exe []
  -- Usage may go to stdout or stderr depending on implementation
  assert "No usage message" $ "Usage" `isInfixOf` out3 || "Usage" `isInfixOf` err3
  pass "Shows usage"

  IO.putStrLn ""
  IO.putStrLn "=== All xdg-user-dir tests passed! ==="
