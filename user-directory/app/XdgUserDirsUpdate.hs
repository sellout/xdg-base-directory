{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- A command-line utility that behaves similarly to the C reference
-- implementation of @xdg-user-dirs-update@.
--
-- Usage: @xdg-user-dirs-update [OPTIONS]@
--
-- Options:
--   --help               Display help and exit
--   --force              Force update even if directories already exist
--   --dummy-output PATH  Write results to PATH instead of actual config file
--   --set NAME PATH      Set a specific directory
module Main (main) where

import "base" Data.Bool (Bool (False, True))
import "base" Data.Either (Either (Left, Right))
import "base" Data.Foldable (mapM_)
import "base" Data.Function (($))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Monoid (mempty)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import qualified "base" System.Environment as Env
import qualified "base" System.Exit as Exit
import qualified "base" System.IO as IO
import qualified "containers" Data.Map.Strict as Map
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir))
import "pathway" Data.Path.Format (Format (Format))
import qualified "pathway" Data.Path.Format as Format
import "xdg-base-directory-internal" Data.Path.Patch (serialize)
import "xdg-user-directory" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
  )
import "xdg-user-directory" XDG.UserDirectory.Update
  ( UpdateError,
    ensureAllUserDirectories,
  )

-- | Command-line options.
data Options = Options
  { optHelp :: Bool,
    optForce :: Bool,
    optDummyOutput :: Maybe String,
    optSet :: Maybe (String, String)
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optHelp = False,
      optForce = False,
      optDummyOutput = Nothing,
      optSet = Nothing
    }

-- | Parse command-line arguments.
parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = Right opts
    go opts ("--help" : rest) = go opts {optHelp = True} rest
    go opts ("--force" : rest) = go opts {optForce = True} rest
    go opts ("--dummy-output" : path : rest) =
      go opts {optDummyOutput = Just path} rest
    go _ ("--dummy-output" : []) =
      Left "--dummy-output requires a PATH argument"
    go opts ("--set" : name : path : rest) =
      go opts {optSet = Just (name, path)} rest
    go _ ("--set" : _) =
      Left "--set requires NAME and PATH arguments"
    go _ (arg : _) =
      Left $ "Unknown option: " <> arg

-- | Display help message.
showHelp :: IO.IO ()
showHelp = do
  IO.putStrLn "Usage: xdg-user-dirs-update [OPTIONS]"
  IO.putStrLn ""
  IO.putStrLn "Update XDG user directories configuration."
  IO.putStrLn ""
  IO.putStrLn "Options:"
  IO.putStrLn "  --help               Display this help and exit"
  IO.putStrLn "  --force              Force update even if directories already exist"
  IO.putStrLn "  --dummy-output PATH  Write results to PATH instead of config file"
  IO.putStrLn "  --set NAME PATH      Set a specific directory"
  IO.putStrLn ""
  IO.putStrLn "Directory names: DESKTOP, DOWNLOAD, TEMPLATES, PUBLICSHARE,"
  IO.putStrLn "                 DOCUMENTS, MUSIC, PICTURES, VIDEOS"

-- | Local format for serializing paths.
localFormat :: Format String
localFormat =
  Format
    { Format.root = "/",
      Format.current = "",
      Format.parent = "..",
      Format.separator = "/",
      Format.substitutions = mempty
    }

-- | Show update result for a directory.
showResult ::
  UserDirectory ->
  Either UpdateError (Path 'Abs 'Dir String) ->
  IO.IO ()
showResult dir result = case result of
  Left _ -> IO.hPutStrLn IO.stderr $ "  " <> showDir dir <> ": error"
  Right path -> IO.putStrLn $ "  " <> showDir dir <> ": " <> serialize localFormat path
  where
    showDir (UserDirectory name) = name

main :: IO.IO ()
main = do
  args <- Env.getArgs
  case parseArgs args of
    Left err -> do
      IO.hPutStrLn IO.stderr $ "Error: " <> err
      IO.hPutStrLn IO.stderr "Try 'xdg-user-dirs-update --help' for more information."
      Exit.exitFailure
    Right opts
      | optHelp opts -> showHelp
      | Just (name, path) <- optSet opts -> do
          -- --set is not yet implemented in the library
          IO.hPutStrLn IO.stderr $
            "Setting " <> name <> " to " <> path <> " (not yet implemented)"
      | True -> do
          IO.putStrLn "Ensuring user directories exist..."
          results <- ensureAllUserDirectories
          mapM_ showResultPair $ Map.toList results
          IO.putStrLn "Done."
  where
    showResultPair (dir, result) = showResult dir result
