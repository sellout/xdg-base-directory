{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

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
import "base" Control.Monad ((<=<), (=<<))
import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "base" Data.Bifunctor (first)
import "base" Data.Bool (Bool (False, True))
import "base" Data.Either (Either (Left, Right), either, fromRight)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable, foldr)
import "base" Data.Function (const, ($))
import "base" Data.Functor ((<$>))
import "base" Data.List.NonEmpty (NonEmpty)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" Data.Tuple (uncurry)
import "base" Data.Void (Void)
import "base" GHC.Generics (Generic)
import qualified "base" System.Environment as Env
import qualified "base" System.Exit as Exit
import "base" System.IO (IO)
import qualified "base" System.IO as IO
import "base" Text.Show (Show, show)
import qualified "containers" Data.Map.Strict as Map
import qualified "megaparsec" Text.Megaparsec as MP
import "pathway" Data.Path (Path, Type (Dir, File), (</>))
import qualified "pathway" Data.Path as Path
import "pathway" Data.Path.Directory ((</?>))
import qualified "pathway" Data.Path.Format as Format
import qualified "pathway" Data.Path.Parser as Parser
import "pathway" Data.Path.Relativity (Relativity (Abs, Any, Rel))
import qualified "pathway-system" Filesystem.Path as FS
import qualified "strict" Data.Strict.Maybe as Strict
import "these" Data.These (These, these)
import "transformers" Control.Monad.Trans.Except
  ( ExceptT (ExceptT),
    runExceptT,
    throwE,
    withExceptT,
  )
-- FIXME: Shouldn’t need to import from ".Internal".
import qualified "xdg-base-directory" XDG.BaseDirectory.Internal as BaseDir
import "xdg-base-directory-internal" Data.Path.Patch (serialize)
import qualified "xdg-user-directory" XDG.UserDirectory.Config as Config
import "xdg-user-directory" XDG.UserDirectory.Parser
  ( pathToDirectoryValue,
    setDirectory,
  )
import "xdg-user-directory" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
    UserDirsConfig,
    formatUserDirectory,
  )
import "xdg-user-directory" XDG.UserDirectory.Update
  ( UpdateError,
    ensureAllUserDirectories,
  )
import "base" Prelude (error)

-- | Like `foldMap`, but for `traverse`.
--
--  __NB__: This is a safer alternative to `traverse_`. `traverse_` can throw
--          away arbitrary values, but it’s often used to discard values like @t
--          ()@. This can still be used in the @t ()@ case, but won’t work to
--          discard non-unit structures.
foldTraverse ::
  (Applicative f, Foldable t, Monoid b) => (a -> f b) -> t a -> f b
foldTraverse f = foldr (liftA2 (<>) . f) $ pure mempty

-- |
--
--   When a reparented path tries to pass @/@ when resolving its @../@, this
--   returns the reparented path in `Left`.
resolveAnyFile ::
  Path 'Abs 'Dir rep ->
  Path 'Any 'File rep ->
  Either (Path ('Rel 'True) 'File rep) (Path 'Abs 'File rep)
resolveAnyFile base =
  ( \case
      Path.AbsFile abs -> pure abs
      Path.RelFile rel -> pure $ base </> rel
      -- FIXME: Pathway should be using lazy Maybe for `</?>`
      Path.ReparentedFile rep -> Strict.maybe (Left rep) pure $ base </?> rep
      _ -> error "Directories should be unreachable, because we just “forgot” that it’s a file."
  )
    . Path.anchor
    . Path.forgetType

-- | Command-line options.
data Options = Options
  { help :: Bool,
    force :: Bool,
    -- |
    --
    --  __FIXME__: This is a boundary case that Pathway should make very easy –
    --             user provides an arbitrary path, of which some subset (in
    --             this case, anything without a trailing @/@) is valid. And
    --             then we separately resolve it to an absolute path (generally
    --             relative to `FS.getCurrentDirectory`) if necessary. For
    --             example, the user shouldn’t have to `Path.forgetType` in
    --             order to concatenate the absolute path.
    dummyOutput :: Maybe (Path 'Any 'File String),
    set :: Maybe (UserDirectory, Path 'Abs 'Dir String)
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
    go opts ("--dummy-output" : file : rest) =
      go
        opts
          { dummyOutput =
              either
                (const Nothing)
                ( ( \case
                      Path.AbsFile abs -> pure $ Path.forgetRelativity abs
                      Path.RelFile abs -> pure $ Path.forgetRelativity abs
                      Path.ReparentedFile abs -> pure $ Path.forgetRelativity abs
                      _ -> Nothing
                  )
                    . Path.anchor
                )
                $ MP.parse (Parser.path @_ @Void Format.local) "" file
          }
        rest
    go _ ("--dummy-output" : _) =
      Left "--dummy-output requires a PATH argument"
    go opts ("--set" : name : absDir : rest) =
      go
        opts
          { set =
              either
                (const Nothing)
                ( ( \case
                      Path.AbsDir abs -> pure (UserDirectory name, abs)
                      _ -> Nothing
                  )
                    . Path.anchor
                    . Path.forgetType
                )
                $ MP.parse (Parser.directory @_ @Void Format.local) "" absDir
          }
        rest
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

data Error
  = ConfigFailure (Config.Error String)
  | -- | When trying to resolve a relative output path, but we couldn’t
    --   determine what @./@ refers to.
    FailedToResolveCurrentDirectory (FS.InternalFailure FS.PathRep Void)
  | -- | Occurs when the user provides an output path with @../@ that causes it
    --   to try to go beyond @/@.
    OutputPathPassedRoot
      -- | What the program thinks the “current directory” is
      (Path 'Abs 'Dir String)
      -- | The path the user provided, which we expect to be able to append to
      --   the current directory.
      (Path ('Rel 'True) 'File String)
  | -- | When we couldn’t find any reasonable value or fallback for
    --   @$XDG_CONFIG_HOME@.
    FailedToResolveXdgConfigHome (NonEmpty BaseDir.Error)
  | FailedToResolveHomeDirectory BaseDir.Error
  deriving stock (Eq, Generic, Show)

resolveDummyOutput ::
  Path 'Any 'File String -> ExceptT Error IO (Path 'Abs 'File String)
resolveDummyOutput outPath =
  ( \curDir ->
      either (throwE . OutputPathPassedRoot curDir) pure $
        resolveAnyFile curDir outPath
  )
    =<< withExceptT FailedToResolveCurrentDirectory FS.getCurrentDirectory

-- | Determines the output file to use for the update.
output :: Options -> ExceptT Error IO (Path 'Abs 'File String)
output =
  maybe
    ( ExceptT $
        first FailedToResolveXdgConfigHome . theseToEither
          <$> Config.defaultFile
    )
    resolveDummyOutput
    . dummyOutput
  where
    theseToEither :: These a b -> Either a b
    theseToEither = these Left pure $ const pure

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
        either
          ( \err -> do
              IO.hPutStrLn IO.stderr $ show err
              Exit.exitFailure
          )
          pure
          <=< runExceptT
          $ if
            | help opts -> liftIO showHelp
            | Just (name, path) <- set opts ->
                ( \dirValue -> do
                    -- Load existing config or start with empty
                    newConfig <- liftIO $ setDirectory name dirValue . fromRight Map.empty <$> Config.load
                    out <- output opts
                    liftIO . IO.putStrLn $
                      "Setting ‘"
                        <> formatUserDirectory name
                        <> "’ to "
                        <> Path.toText Format.local path
                    liftIO $ Config.writeTo out newConfig
                    liftIO . IO.putStrLn $ "Config written to: " <> Path.toText Format.local out
                )
                  <=< withExceptT FailedToResolveHomeDirectory . ExceptT
                  $ pathToDirectoryValue path
            | True ->
                ( \config ->
                    ensure config
                      $ foldTraverse
                        ( \path -> do
                            out <- resolveDummyOutput path
                            liftIO . IO.putStrLn $
                              "Writing config to: "
                                <> Path.toText Format.local out
                            liftIO $ Config.writeTo out config
                        )
                      $ dummyOutput opts
                )
                  <=< withExceptT ConfigFailure
                  $ ExceptT Config.load
    )
    . parseArgs
    =<< Env.getArgs
  where
    ensure :: (MonadIO m) => UserDirsConfig String -> m () -> m ()
    ensure config action = do
      liftIO $ IO.putStrLn "Ensuring user directories exist..."
      liftIO $
        foldTraverse (uncurry showResult) . Map.toList
          =<< ensureAllUserDirectories config
      action
      liftIO $ IO.putStrLn "Done."
