{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

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
import "base" Data.Kind qualified as Kind
import "base" Data.Maybe (Maybe, maybe)
import "base" Data.String (String)
import "base" Data.Void (Void)
import "base" GHC.Generics (Generic)
import "base" System.IO.Error (IOError)
import "base" Text.Show (Show)
import "megaparsec" Text.Megaparsec.Stream qualified as MP
import "pathway" Data.Path (Path, Type (Dir))
import "pathway" Data.Path.Relativity (Relativity (Abs, Any))
import "pathway-compat-base" Common (InternalFailure)
import "pathway-internal" Data.Path.Internal (parents)
import "yaya" Yaya.Pattern qualified as Strict

-- | Errors that can occur when reading an environment variable.
--
--  __NB__: This is lacking `Ord` and `Read` instances because `IOError` is
--          missing them.
--
-- @since 99999
type VarError :: Kind.Type
data VarError
  = MissingVar String (Maybe IOError)
  | EmptyVar String
  deriving stock (Eq, Generic, Show)

-- | Errors that can occur when resolving a base directory.
--
--  __NB__: This is lacking `Ord` and `Read` instances because
--          `Dir.InternalFailure` is missing them.
--
-- @since 99999
type Error :: Kind.Type -> Kind.Type
data Error rep
  = Var VarError
  | NoDirectoriesFound -- only for directory lists
  | RelativeDirectory
  | Pathway (InternalFailure rep Void)
  deriving stock (Generic)

type role Error nominal

deriving stock instance (Eq rep, Eq (MP.Token rep)) => Eq (Error rep)

deriving stock instance (Show rep, Show (MP.Token rep)) => Show (Error rep)

-- | An absolute directory path, as required by the XDG base directory spec.
--
--       All paths set in these environment variables must be absolute.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
--
-- @since 99999
type BaseDirectory :: Kind.Type -> Kind.Type
type BaseDirectory = Path 'Abs 'Dir

-- | Convert a 'Maybe' to an 'Either' using the provided error for 'Nothing'.
--
-- @since 99999
note :: e -> Maybe a -> Either e a
note e = maybe (Left e) pure

-- | Extract an absolute path, returning an error if the path is relative.
--
-- @since 99999
extractAbs :: Path 'Any typ rep -> Either (Error rep) (Path 'Abs typ rep)
extractAbs path =
  Strict.maybe (pure path {parents = ()}) (const $ Left RelativeDirectory) $
    parents path
