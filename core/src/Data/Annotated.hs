{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fplugin-opt NoRecursion:ignore-methods:sconcat,stimes #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- A good data type for warnings.
module Data.Annotated
  ( Annotated (..),
    annotated,
    combineAnnotated,
  )
where

import "base" Control.Applicative (Applicative, liftA2, pure)
import "base" Control.Category (id, (.))
import "base" Control.Monad (Monad, (>>=))
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable)
import "base" Data.Function (const, ($))
import "base" Data.Functor (Functor)
import qualified "base" Data.Kind as Kind
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Ord (Ord)
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.Traversable (Traversable)
import "base" Data.Tuple (curry)
import "base" GHC.Generics (Generic, Generic1)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "comonad" Control.Comonad (Comonad, duplicate, extract)

-- | This is isomorphic to `(Maybe a, b)` and is like a reversed `AndMaybe`,
--   which gives it different semantics. It is useful when a value may be
--   produced with warnings, with `NotBut` representing a clean result and
--   `Noted` being annotated with warnings.
--
-- @since 0.0.1.0
data Annotated (a :: Kind.Type) (b :: Kind.Type) = NotBut b | Noted a b
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving stock (Foldable, Functor, Generic1, Traversable)

type role Annotated representational representational

-- | Case analysis for 'Annotated'. The first function handles the unannotated
--   case ('NotBut'), the second handles the annotated case ('Noted').
--
-- @since 0.0.1.0
annotated :: (b -> c) -> (a -> b -> c) -> Annotated a b -> c
annotated f g = \case
  NotBut b -> f b
  Noted a b -> g a b

-- | A more general version of `<>` for `Annotated`.
--
-- @since 0.0.1.0
combineAnnotated ::
  (a -> a -> a) ->
  (b -> c -> d) ->
  Annotated a b ->
  Annotated a c ->
  Annotated a d
combineAnnotated f s = curry \case
  (NotBut b, NotBut b') -> NotBut $ s b b'
  (NotBut b, Noted a' b') -> Noted a' $ s b b'
  (Noted a b, NotBut b') -> Noted a $ s b b'
  (Noted a b, Noted a' b') -> Noted (f a a') $ s b b'

-- | There are various `Applicatives` possible based on how the @a@ is combined.
--   This one chooses `<>`, but you can use `combineAnnotated` to define your
--   own combining operation.
instance (Semigroup a) => Applicative (Annotated a) where
  pure = NotBut
  liftA2 = combineAnnotated (<>)

instance (Semigroup a, Semigroup b) => Semigroup (Annotated a b) where
  (<>) = liftA2 (<>)

instance (Semigroup a, Monoid b) => Monoid (Annotated a b) where
  mempty = NotBut mempty

instance (Semigroup a) => Monad (Annotated a) where
  ann >>= f = annotated f (\a -> annotated (Noted a) (Noted . (a <>)) . f) ann

instance Comonad (Annotated a) where
  extract = annotated id $ const id
  duplicate = annotated (NotBut . NotBut) $ \a -> Noted a . Noted a
