{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2009-2025 Simon Hengel
-- License: MIT
module Main (main) where

import safe "base" Data.Function (($))
import safe "base" Data.Semigroup ((<>))
import safe "base" System.IO (IO)
import "doctest" Test.DocTest (doctest)
import "this" Build_doctests (flags, module_sources, pkgs)

-- | The test suite’s entry point.
--
-- @since 0.0.1.0
main :: IO ()
main = doctest $ flags <> pkgs <> module_sources
