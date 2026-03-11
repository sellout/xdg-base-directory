{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- A command-line utility that behaves identically to the C reference
-- implementation of @xdg-user-dir@.
--
-- Usage: @xdg-user-dir NAME@
--
-- Prints the resolved path for the given user directory type (DESKTOP,
-- DOWNLOAD, TEMPLATES, PUBLICSHARE, DOCUMENTS, MUSIC, PICTURES, VIDEOS).
module Main (main) where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Monad ((<=<), (=<<))
import "base" Data.Either (either)
import "base" Data.Function (($))
import "base" Data.Maybe (maybe)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import qualified "base" System.Environment as Env
import "base" System.Exit (exitFailure)
import "base" System.IO (IO)
import qualified "base" System.IO as IO
import "base" Text.Show (show)
import qualified "megaparsec" Text.Megaparsec as MP
import qualified "pathway" Data.Path as Path
import qualified "pathway" Data.Path.Format as Format
import qualified "pathway-system" Filesystem.Path as FS
import qualified "xdg-base-directory" XDG.BaseDirectory.Internal as BaseDir
import "xdg-base-directory-internal" Data.Path.Patch (serialize)
import "xdg-user-directory" XDG.UserDirectory (getUserDirectory)
import qualified "xdg-user-directory" XDG.UserDirectory.Config as Config
import "xdg-user-directory" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
  )

formatConfigError :: Config.Error -> String
formatConfigError = \case
  Config.ConfigHomeError _errs -> "couldn’t resolve $XDG_CONFIG_HOME"
  Config.ParseFailed _err -> "couldn’t parse the config file"
  Config.FileNotFound -> "couldn’t find the xdg-user-dirs config file"

formatBaseDirError :: BaseDir.Error -> String
formatBaseDirError = \case
  BaseDir.Var error -> case error of
    BaseDir.MissingVar var merr ->
      "$" <> var <> "is missing" <> maybe "" ((": " <>) . show) merr
    BaseDir.EmptyVar var -> "$" <> var <> " is empty"
  BaseDir.NoDirectoriesFound -> "couldn’t find the any matching directories"
  BaseDir.RelativeDirectory ->
    "expected an absolute directory, but it was relative"
  BaseDir.Pathway error -> case error of
    FS.ParseFailure failure -> MP.errorBundlePretty failure
    FS.IncorrectResultType _eRel _eTyp _aRel _aTyp path ->
      Path.toText Format.local path <> " isn’t the type of path we expected"

-- | The program’s entry point.
--
-- @since 0.0.1.0
main :: IO ()
main =
  ( \case
      [dir] ->
        either
          ( \err -> do
              IO.hPutStrLn IO.stderr $ "ERROR: " <> formatConfigError err
              exitFailure
          )
          pure
          <=< Config.withConfig
          $ either
            ( \err -> do
                IO.hPutStrLn IO.stderr $ "ERROR: " <> formatBaseDirError err
                exitFailure
            )
            (IO.putStrLn . serialize Format.local)
            <=< getUserDirectory (UserDirectory dir)
      _ -> do
        progName <- Env.getProgName
        IO.hPutStrLn IO.stderr $ "Usage " <> progName <> " <dir-type>"
        exitFailure
  )
    =<< Env.getArgs
