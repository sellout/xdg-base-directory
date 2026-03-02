{-# LANGUAGE Safe #-}

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

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Exception (tryJust)
import "base" Control.Monad ((=<<))
import "base" Data.Bifunctor (first)
import "base" Data.Either (Either (Left), either)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor, fmap)
import "base" Data.Maybe (Maybe (Nothing), maybe)
import "base" Data.Traversable (Traversable)
import "base" Data.Void (Void)
import "base" GHC.Generics (Generic, Generic1)
import "base" System.IO (IO)
import "base" System.IO.Error (IOError, isDoesNotExistError)
import "base" Text.Show (Show)
import "pathway" Data.Path (Path, Type (Dir))
import "pathway" Data.Path.Relativity (Relativity (Abs, Any))
import qualified "pathway-system" Filesystem.Path as Dir
import "these" Data.These (These (That, This))
import "transformers" Control.Monad.Trans.Except (runExceptT)
import qualified "xdg-base-directory-internal" Data.Path.Patch as Patch
import qualified "xdg-base-directory-internal" XDG.BaseDirectory.Internal.System as System

data VarError rep = MissingVar rep (Maybe IOError) | EmptyVar rep
  deriving stock (Eq, Generic, Show, Foldable, Functor, Generic1, Traversable)

data Error rep
  = Var (VarError rep)
  | NoDirectoriesFound -- only for directory lists
  | RelativeDirectory
  | Pathway (Dir.InternalFailure Dir.PathRep Void)
  deriving stock (Eq, Generic, Show, Foldable, Functor, Generic1, Traversable)

-- |
--
--       All paths set in these environment variables must be absolute.
--       —[§2](https://specifications.freedesktop.org/basedir-spec/latest/#basics)
type BaseDirectory = Path 'Abs 'Dir

getAbs :: Patch.AnchoredType typ rep -> Maybe (Path 'Abs typ rep)
getAbs = \case
  Patch.Abs abs -> pure abs
  Patch.Rel _ -> Nothing
  Patch.Reparented _ -> Nothing

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) pure

weakenEither :: Either a b -> These a b
weakenEither = either This That

extractAbs :: Path 'Any typ rep -> Either (Error rep) (Path 'Abs typ rep)
extractAbs = note RelativeDirectory . getAbs . Patch.anchorType

-- |
--
--  __FIXME__: This should be coming from a Pathway lib.
getHomeDirectory ::
  IO (Either (Error Dir.PathComponent) (Path 'Abs 'Dir Dir.PathComponent))
getHomeDirectory =
  fmap (first Pathway =<<)
    . tryJust
      ( \e ->
          if isDoesNotExistError e
            then
              pure . Var . MissingVar (System.fromStringLiteral "HOME") $
                pure e
            else Nothing
      )
    $ runExceptT Dir.getHomeDirectory
