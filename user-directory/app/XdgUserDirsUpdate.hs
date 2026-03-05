{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Trustworthy #-}

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

import "base" Control.Applicative (Applicative, liftA2, pure)
import "base" Control.Category ((.))
import "base" Control.Monad ((=<<))
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Either (Either (Left, Right), either, fromRight)
import "base" Data.Foldable (Foldable, foldr)
import "base" Data.Function (const, ($))
import "base" Data.List (drop, isPrefixOf, length)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" Data.Tuple (uncurry)
import qualified "base" System.Environment as Env
import qualified "base" System.Exit as Exit
import "base" System.IO (IO)
import qualified "base" System.IO as IO
import qualified "containers" Data.Map.Strict as Map
import "directory" System.Directory (getHomeDirectory)
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir))
import qualified "pathway" Data.Path.Format as Format
import "xdg-base-directory-internal" Data.Path.Patch (serialize)
import "xdg-user-directory" XDG.UserDirectory.Config
  ( loadConfig,
    writeConfigTo,
  )
import "xdg-user-directory" XDG.UserDirectory.Parser
  ( parsePathToDirectoryValue,
    setDirectory,
  )
import "xdg-user-directory" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
  )
import "xdg-user-directory" XDG.UserDirectory.Update
  ( UpdateError,
    ensureAllUserDirectories,
  )

-- | Like `foldMap`, but for `traverse`.
--
--  __NB__: This is a safer alternative to `traverse_`. `traverse_` can throw
--          away arbitrary values, but it’s often used to discard values like @t
--          ()@. This can still be used in the @t ()@ case, but won’t work to
--          discard non-unit structures.
foldTraverse ::
  (Applicative f, Foldable t, Monoid b) => (a -> f b) -> t a -> f b
foldTraverse f = foldr (liftA2 (<>) . f) $ pure mempty

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
    go _ ("--dummy-output" : _) =
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
  foldTraverse
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

-- | The program’s entry point.
--
-- @since 0.0.1.0
main :: IO ()
main =
  either
    ( \err -> do
        progName <- Env.getProgName
        foldTraverse
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
              -- Get home directory and normalize path
              homeDir <- getHomeDirectory
              let normalizedPath =
                    if ("/" `isPrefixOf` path) && (homeDir `isPrefixOf` path)
                      then "$HOME" <> drop (length homeDir) path
                      else path
              case parsePathToDirectoryValue normalizedPath of
                Nothing -> do
                  IO.hPutStrLn IO.stderr "Error: PATH must be absolute"
                  Exit.exitFailure
                Just dirValue -> do
                  -- Load existing config or start with empty
                  configResult <- loadConfig
                  let baseConfig = fromRight Map.empty configResult
                      newConfig = setDirectory (UserDirectory name) dirValue baseConfig
                  -- Write to dummy output or warn if no output specified
                  maybe
                    ( foldTraverse
                        (IO.hPutStrLn IO.stderr)
                        [ "Warning: --set without --dummy-output would modify the real config.",
                          "Use --dummy-output to specify where to write the config."
                        ]
                    )
                    ( \outPath -> do
                        IO.putStrLn $ "Setting " <> name <> " to " <> path
                        writeConfigTo outPath newConfig
                        IO.putStrLn $ "Config written to: " <> outPath
                    )
                    $ dummyOutput opts
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
                      . foldTraverse
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
      foldTraverse (uncurry showResult) . Map.toList
        =<< ensureAllUserDirectories
      action
      IO.putStrLn "Done."
