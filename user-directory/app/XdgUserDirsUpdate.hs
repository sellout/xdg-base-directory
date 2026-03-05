{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Safe #-}

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

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Monad ((=<<))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Either (Either (Left, Right), either)
import "base" Data.Foldable (fold)
import "base" Data.Function (const, ($))
import "base" Data.Functor (fmap, (<$>))
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" Data.Traversable (traverse)
import "base" Data.Tuple (uncurry)
import qualified "base" System.Environment as Env
import qualified "base" System.Exit as Exit
import "base" System.IO (IO)
import qualified "base" System.IO as IO
import qualified "containers" Data.Map.Strict as Map
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir))
import qualified "pathway" Data.Path.Format as Format
import "xdg-base-directory-internal" Data.Path.Patch (serialize)
import "xdg-user-directory" XDG.UserDirectory.Config
  ( loadConfig,
    writeConfigTo,
  )
import "xdg-user-directory" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
  )
import "xdg-user-directory" XDG.UserDirectory.Update
  ( UpdateError,
    ensureAllUserDirectories,
  )

-- | Command-line options.
data Options = Options
  { help :: Bool,
    force :: Bool,
    dummyOutput :: Maybe String,
    set :: Maybe (String, String)
  }

defaultOptions :: Options
defaultOptions =
  Options
    { help = False,
      force = False,
      dummyOutput = Nothing,
      set = Nothing
    }

-- | Parse command-line arguments.
parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = Right opts
    go opts ("--help" : rest) = go opts {help = True} rest
    go opts ("--force" : rest) = go opts {force = True} rest
    go opts ("--dummy-output" : path : rest) =
      go opts {dummyOutput = Just path} rest
    go _ ("--dummy-output" : []) =
      Left "--dummy-output requires a PATH argument"
    go opts ("--set" : name : path : rest) =
      go opts {set = Just (name, path)} rest
    go _ ("--set" : _) =
      Left "--set requires NAME and PATH arguments"
    go _ (arg : _) =
      Left $ "Invalid argument " <> arg

-- | Display help message.
showHelp :: IO ()
showHelp = do
  progName <- Env.getProgName
  fold
    <$> traverse
      IO.putStrLn
      [ "Usage: " <> progName <> " [OPTIONS]",
        "",
        "Update XDG user directories configuration.",
        "",
        "Options:",
        "  --help               Display this help and exit",
        "  --force              Force update even if directories already exist",
        "  --dummy-output PATH  Write results to PATH instead of config file",
        "  --set NAME PATH      Set a specific directory"
      ]

-- | Show update result for a directory.
showResult ::
  UserDirectory ->
  Either UpdateError (Path 'Abs 'Dir String) ->
  IO ()
showResult (UserDirectory dir) =
  either
    (const . IO.hPutStrLn IO.stderr $ "  " <> dir <> ": error")
    (IO.putStrLn . (("  " <> dir <> ": ") <>) . serialize Format.local)

main :: IO ()
main =
  either
    ( \err -> do
        progName <- Env.getProgName
        fold
          <$> traverse
            (IO.hPutStrLn IO.stderr)
            [ "Error: " <> err,
              "Try ‘" <> progName <> " --help’ for more information."
            ]
        Exit.exitFailure
    )
    ( \opts ->
        if
          | help opts -> showHelp
          | Just (name, path) <- set opts -> do
              -- --set is not yet implemented in the library
              IO.hPutStrLn IO.stderr $
                "Setting " <> name <> " to " <> path <> " (not yet implemented)"
          | True ->
              either
                ( const do
                    IO.hPutStrLn IO.stderr "Warning: Could not load config file"
                    -- Continue with directory creation anyway
                    ensure $ pure ()
                )
                ( \config ->
                    ensure
                      -- Write config to dummy output if specified
                      . maybe
                        (pure ())
                        ( \path -> do
                            IO.putStrLn $ "Writing config to: " <> path
                            writeConfigTo path config
                        )
                      $ dummyOutput opts
                )
                =<< loadConfig
    )
    . parseArgs
    =<< Env.getArgs
  where
    ensure :: IO () -> IO ()
    ensure action = do
      IO.putStrLn "Ensuring user directories exist..."
      fmap fold . traverse (uncurry showResult) . Map.toList
        =<< ensureAllUserDirectories
      action
      IO.putStrLn "Done."
