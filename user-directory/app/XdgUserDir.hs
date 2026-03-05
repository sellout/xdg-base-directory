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

import "base" Data.Either (Either (Left, Right))
import "base" Data.Function (($))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Monoid (mempty)
import "base" Data.String (String)
import qualified "base" System.Environment as Env
import qualified "base" System.IO as IO
import "pathway" Data.Path.Format (Format (Format))
import qualified "pathway" Data.Path.Format as Format
import "xdg-base-directory-internal" Data.Path.Patch (serialize)
import "xdg-user-directory" XDG.UserDirectory (getUserDirectory)
import "xdg-user-directory" XDG.UserDirectory.Type
  ( UserDirectory (UserDirectory),
    desktop,
    documents,
    download,
    music,
    pictures,
    publicShare,
    templates,
    videos,
  )

-- | Parse a directory name argument to a UserDirectory.
parseDirectoryName :: String -> Maybe UserDirectory
parseDirectoryName name = case name of
  "DESKTOP" -> Just desktop
  "DOWNLOAD" -> Just download
  "TEMPLATES" -> Just templates
  "PUBLICSHARE" -> Just publicShare
  "DOCUMENTS" -> Just documents
  "MUSIC" -> Just music
  "PICTURES" -> Just pictures
  "VIDEOS" -> Just videos
  -- Allow custom directory types
  _ -> Just $ UserDirectory name

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

main :: IO.IO ()
main = do
  args <- Env.getArgs
  case args of
    [name] -> case parseDirectoryName name of
      Nothing -> do
        IO.hPutStrLn IO.stderr "Invalid directory name"
      Just dir -> do
        result <- getUserDirectory dir
        case result of
          Left _ -> do
            -- Fallback already handled by getUserDirectory, this shouldn't happen
            IO.putStrLn ""
          Right path -> IO.putStrLn $ serialize localFormat path
    _ -> do
      IO.hPutStrLn IO.stderr "Usage: xdg-user-dir NAME"
      IO.hPutStrLn IO.stderr ""
      IO.hPutStrLn IO.stderr "Valid names: DESKTOP, DOWNLOAD, TEMPLATES, PUBLICSHARE,"
      IO.hPutStrLn IO.stderr "             DOCUMENTS, MUSIC, PICTURES, VIDEOS"
