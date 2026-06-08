{-# LANGUAGE Unsafe #-}

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

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<), (=<<))
import safe "base" Data.Either (either)
import safe "base" Data.Function (($))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe qualified "base" System.Environment as Env
import safe "base" System.Exit (exitFailure)
import safe "base" System.IO (IO)
import safe qualified "base" System.IO as IO
import safe "base" Text.Show (show)
import safe "comonad" Control.Comonad (extract)
import safe qualified "megaparsec" Text.Megaparsec as MP
import safe qualified "pathway" Data.Path as Path
import safe qualified "pathway" Data.Path.Format as Format
import safe qualified "pathway-compat-base" System.IO.Error.Pathway as Error
import safe qualified "pathway-compat-directory" System.Directory.Error as DirError
import safe qualified "pathway-system" System.Path as FS
import "variant" Data.Variant (V)
import "variant" Data.Variant.ContFlow ((>:>))
import safe "xdg-user-directory" XDG.UserDirectory (getUserDirectory)
import safe qualified "xdg-user-directory" XDG.UserDirectory.Config as Config
import safe "xdg-user-directory" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
  )

-- |
--
--  __TODO__: Put up a comparison of different variant-handling libraries
--          - oops
--          - polysemy – mostly meant as an effect system
--          - variant (née haskus-utils-variant)
formatBaseDirError :: V (FS.GetUserDirectoryFailure String) -> String
formatBaseDirError =
  ( >:>
      ( \(Error.DoesNotExistError ioe) -> "The requested base directory doesn’t exist: " <> show ioe,
        \(Error.UnsupportedOperation ioe) -> "The requested op isn’t possible:" <> show ioe,
        \case
          DirError.ParseFailure failure -> MP.errorBundlePretty failure
          DirError.IncorrectResultType _eRel _eTyp _aRel _aTyp path ->
            Path.toText Format.local path <> " isn’t the type of path we expected"
      )
  )

-- | The program’s entry point.
--
-- @since 0.0.1.0
main :: IO ()
main =
  ( \case
      [dir] ->
        either
          ( \err -> do
              IO.hPutStrLn IO.stderr $ "ERROR: " <> MP.errorBundlePretty err
              exitFailure
          )
          pure
          -- TODO: Don’t discard the warnings. Output them if there’s a debug
          --       flag or something.
          . extract
          . extract
          <=< Config.withConfig
          $ either
            ( \err -> do
                IO.hPutStrLn IO.stderr $ "ERROR: " <> formatBaseDirError err
                exitFailure
            )
            (IO.putStrLn . Path.toText Format.local)
            <=< getUserDirectory (UserDirectory dir)
      _ -> do
        progName <- Env.getProgName
        IO.hPutStrLn IO.stderr $ "Usage " <> progName <> " <dir-type>"
        exitFailure
  )
    =<< Env.getArgs
