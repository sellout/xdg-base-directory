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

import "base" Control.Category ((.))
import "base" Control.Monad ((<=<), (=<<))
import "base" Data.Either (either)
import "base" Data.Function (($))
import "base" Data.Semigroup ((<>))
import qualified "base" System.Environment as Env
import "base" System.Exit (exitFailure)
import "base" System.IO (IO)
import qualified "base" System.IO as IO
import qualified "pathway" Data.Path.Format as Format
import "xdg-base-directory-internal" Data.Path.Patch (serialize)
import "xdg-user-directory" XDG.UserDirectory (getUserDirectory)
import "xdg-user-directory" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
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
              IO.print err
              exitFailure
          )
          (IO.putStrLn . serialize Format.local)
          <=< getUserDirectory
          $ UserDirectory dir
      _ -> do
        progName <- Env.getProgName
        IO.hPutStrLn IO.stderr $ "Usage " <> progName <> " <dir-type>"
        exitFailure
  )
    =<< Env.getArgs
