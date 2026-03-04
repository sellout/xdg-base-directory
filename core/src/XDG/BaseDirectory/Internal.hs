{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module XDG.BaseDirectory.Internal
  ( VarError (..),
    Error (..),
    BaseDirectory,
    extractAbs,
    getHomeDirectory,
    note,
    weakenEither,
  )
where

import "base" Control.Applicative (empty, pure)
import "base" Control.Category ((.))
import "base" Control.Exception (tryJust)
import "base" Control.Monad ((=<<))
import "base" Data.Either (Either (Left), either)
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import qualified "base" Data.Kind as Kind
import "base" Data.Maybe (Maybe, maybe)
import "base" Data.String (String)
import "base" Data.Void (Void)
import "base" GHC.Generics (Generic)
import "base" System.IO (IO)
import "base" System.IO.Error (IOError, isDoesNotExistError)
import "base" Text.Show (Show)
import "pathway" Data.Path (Path, Type (Dir))
import "pathway" Data.Path.Relativity (Relativity (Abs, Any))
import qualified "pathway-system" Filesystem.Path as Dir
import "these" Data.These (These (That, This))
import qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System

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
data Error
  = Var VarError
  | NoDirectoriesFound -- only for directory lists
  | RelativeDirectory
  | Pathway (Dir.InternalFailure Dir.PathRep Void)
  deriving stock (Eq, Generic, Show)

-- |
--
--       All paths set in these environment variables must be absolute.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
type BaseDirectory = Path 'Abs 'Dir :: Kind.Type -> Kind.Type

getAbs :: Patch.AnchoredType typ rep -> Maybe (Path 'Abs typ rep)
getAbs = \case
  Patch.Abs abs -> pure abs
  Patch.Rel _ -> empty
  Patch.Reparented _ -> empty

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) pure

weakenEither :: Either a b -> These a b
weakenEither = either This That

extractAbs :: Path 'Any typ rep -> Either Error (Path 'Abs typ rep)
extractAbs = note RelativeDirectory . getAbs . Patch.anchorType

-- |
--
--  __FIXME__: This should be coming from a Pathway lib.
getHomeDirectory :: (System.Rep rep) => IO (Either Error (Path 'Abs 'Dir rep))
getHomeDirectory =
  (extractAbs . Patch.parseDirectory =<<)
    <$> tryJust
      ( \e ->
          if isDoesNotExistError e
            then pure . Var . MissingVar "HOME" $ pure e
            else empty
      )
      System.getHomeDirectory
