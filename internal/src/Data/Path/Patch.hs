{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Data.Path.Patch
  ( AnchoredType (..),
    anchorType,
    parseDirectory,
    parseStrict,
    serialize,
    serializeAny,
    withFile,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Monad.IO.Class (MonadIO)
import "base" Data.Bool (Bool (False, True))
import "base" Data.Foldable (foldMap, foldl, toList)
import "base" Data.Function (flip, ($))
import qualified "base" Data.Kind as Kind
import "base" Data.List (reverse)
import "base" Data.Monoid (mempty)
import "base" Data.Ord (Ord)
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.Semigroup ((<>))
import "base" Numeric.Natural (Natural)
import "base" System.IO (Handle, IOMode)
import "exceptions" Control.Monad.Catch (MonadMask)
import "pathway" Data.Path (Type (Dir, File))
import qualified "pathway" Data.Path as Path
import qualified "pathway" Data.Path.Directory as Directory
import "pathway" Data.Path.Format (Format (Format))
import qualified "pathway" Data.Path.Format as Format
import qualified "pathway" Data.Path.Relativity as Rel
import qualified "pathway" Data.Path.Type as Type
import "pathway-internal" Data.Path.Internal
  ( List (List),
    Path (Path),
    directories,
    filename,
    parents,
  )
import "strict" Data.Strict.Maybe (maybe)
import "yaya" Yaya.Fold (cata, cata2, embed)
import "yaya" Yaya.Fold.Common (takeAvailable)
import qualified "this" XDG.BaseDirectory.Internal.System as System

type role AnchoredType nominal nominal

data AnchoredType (typ :: Type) (rep :: Kind.Type)
  = Abs (Path 'Rel.Abs typ rep)
  | Rel (Path ('Rel.Rel 'False) typ rep)
  | Reparented (Path ('Rel.Rel 'True) typ rep)

anchorType :: Path 'Rel.Any typ rep -> AnchoredType typ rep
anchorType path =
  maybe
    ( Abs
        Path
          { parents = (),
            directories = directories path,
            filename = filename path
          }
    )
    ( \case
        0 ->
          Rel
            Path
              { parents = Proxy,
                directories = directories path,
                filename = filename path
              }
        parnts ->
          Reparented
            Path
              { parents = parnts,
                directories = directories path,
                filename = filename path
              }
    )
    $ parents path

-- |
--
--  __TODO__: Move this to pathway-system as an alternative to @MP.parse directory Format.local@.
--
--  __FIXME__: This currently does nothing about escape chars.
parseStrict :: (System.Rep rep) => rep -> Path 'Rel.Any 'Type.Any rep
parseStrict path =
  let (dir, file) = System.splitFileName path
   in ( if System.isValid file
          then Path.forgetType . flip Directory.selectFile file
          else Path.forgetType
      )
        $ parseDirectory dir

-- |
--
--  __TODO__: Move this to pathway-system as an alternative to @MP.parse directory Format.local@.
--
--  __FIXME__: This currently does nothing about escape chars.
parseDirectory :: (System.Rep rep) => rep -> Path 'Rel.Any 'Dir rep
parseDirectory path =
  let (drive, dir) = System.splitDrive path
   in Directory.descendThrough
        ( if System.isValid drive
            then Path.forgetRelativity Directory.root
            else Path.forgetRelativity Directory.current
        )
        . List
        -- NB: The list of dirs is potentially infinite, so here we truncate it
        --     to no more than 100 elements. It’s easy to bump if this is ever a
        --     problem.
        . cata2 (embed . takeAvailable) (100 :: Natural)
        $ System.splitDirectories dir

serializeAny :: (System.Rep a) => Format a -> Path.AnyPath a -> a
serializeAny format path =
  let prefix =
        maybe
          (pure . System.pack . pure $ System.pathSeparator format)
          (cata $ foldMap (Format.parent format :))
          $ parents path
   in foldl
        (System.</>)
        (System.joinPath $ prefix <> reverse (toList $ directories path))
        $ filename path

serialize ::
  (Path.Pathy rel typ, System.Rep a) => Format a -> Path rel typ a -> a
serialize format = serializeAny format . Path.unanchor

localFormat :: forall rep. (System.Rep rep, Ord rep) => Format rep
localFormat =
  let proxy = Proxy :: Proxy rep
   in Format
        { Format.root = System.pack . pure $ System.pathSeparator proxy,
          Format.current = System.fromStringLiteral "",
          Format.parent = System.fromStringLiteral "..",
          Format.separator = System.pack . pure $ System.pathSeparator proxy,
          Format.substitutions = mempty
        }

-- |
--
--  __TODO__: Move this upstream.
withFile ::
  forall m rep a.
  (MonadIO m, MonadMask m, System.Rep rep, Ord rep) =>
  Path 'Rel.Abs 'File rep ->
  IOMode ->
  (Handle -> m a) ->
  m a
withFile = System.withFile . serialize localFormat
