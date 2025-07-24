{-# LANGUAGE Safe #-}

module Data.Annotated
  ( Annotated (..),
  )
where

import "base" Control.Applicative (Applicative, liftA2, pure)
import "base" Control.Monad (Monad, (>>=))
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor)
import "base" Data.Ord (Ord)
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.Traversable (Traversable)
import "base" Data.Tuple (curry)
import "base" GHC.Generics (Generic, Generic1)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "comonad" Control.Comonad (Comonad, duplicate, extract)

data Annotated a b = NotBut b | Noted a b
  deriving stock
    ( Eq,
      Generic,
      Ord,
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

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

instance (Semigroup a) => Monad (Annotated a) where
  ann >>= f = case ann of
    NotBut b -> f b
    Noted a b -> case f b of
      NotBut b' -> Noted a b'
      Noted a' b' -> Noted (a <> a') b'

instance Comonad (Annotated a) where
  extract = \case
    NotBut b -> b
    Noted _ b -> b
  duplicate = \case
    NotBut b -> NotBut (NotBut b)
    Noted a b -> Noted a (Noted a b)
