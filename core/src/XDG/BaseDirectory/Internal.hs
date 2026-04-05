{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module XDG.BaseDirectory.Internal
  ( VarError (..),
    Error (..),
    BaseDirectory,
    extractAbs,
    note,
  )
where

import "base" Control.Applicative (pure)
import "base" Data.Either (Either (Left))
import "base" Data.Eq (Eq)
import "base" Data.Function (const, ($))
import qualified "base" Data.Kind as Kind
import "base" Data.Maybe (Maybe, maybe)
import "base" Data.String (String)
import "base" Data.Void (Void)
import "base" GHC.Generics (Generic)
import "base" System.IO.Error (IOError)
import "base" Text.Show (Show)
import "pathway" Data.Path (Path, Type (Dir))
import "pathway" Data.Path.Relativity (Relativity (Abs, Any))
import "pathway-compat-base" Common (InternalFailure)
import "pathway-internal" Data.Path.Internal (parents)
import qualified "yaya" Yaya.Pattern as Strict

-- |
--
--  __NB__: This is lacking `Ord` and `Read` instances because `IOError` is
--          missing them.
data VarError
  = MissingVar String (Maybe IOError)
  | EmptyVar String
  deriving stock (Eq, Generic, Show)

-- |
--
--  __NB__: This is lacking `Ord` and `Read` instances because
--          `Dir.InternalFailure` is missing them.
--
--  __TODO__: Add `Eq` and `Show` instances.
data Error (rep :: Kind.Type)
  = Var VarError
  | NoDirectoriesFound -- only for directory lists
  | RelativeDirectory
  | Pathway (InternalFailure rep Void)
  deriving stock (Generic)

type role Error nominal

-- |
--
--       All paths set in these environment variables must be absolute.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
type BaseDirectory = Path 'Abs 'Dir :: Kind.Type -> Kind.Type

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) pure

extractAbs :: Path 'Any typ rep -> Either (Error rep) (Path 'Abs typ rep)
extractAbs path =
  Strict.maybe (pure path {parents = ()}) (const $ Left RelativeDirectory) $
    parents path
