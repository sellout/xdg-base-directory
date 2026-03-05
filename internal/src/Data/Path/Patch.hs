{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
    createDirectoryWithParentsIfMissing,
    doesDirectoryExist,
  )
where

import "base" Control.Applicative (empty, pure)
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
import "base" System.IO (Handle, IO, IOMode)
import qualified "base" System.IO.Error as IO
import "exceptions" Control.Monad.Catch (MonadMask, tryJust)
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
import qualified "pathway-system" Filesystem.Path as Dir
import "strict" Data.Strict.Maybe (maybe)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import "yaya" Yaya.Fold (cata, cata2, embed)
import "yaya" Yaya.Fold.Common (takeAvailable)
import qualified "this" XDG.BaseDirectory.Internal.System as System
import "base" Prelude (error)

type role AnchoredType nominal nominal

-- | A path with its relativity anchored to a specific type.
--
-- @since 0.0.1.0
data AnchoredType (typ :: Type) (rep :: Kind.Type)
  = Abs (Path 'Rel.Abs typ rep)
  | Rel (Path ('Rel.Rel 'False) typ rep)
  | Reparented (Path ('Rel.Rel 'True) typ rep)

-- | Determine the relativity of an 'Any' path.
--
-- @since 0.0.1.0
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

-- | Parse a path string strictly.
--
--  __TODO__: Move this to pathway-system as an alternative to @MP.parse directory Format.local@.
--
--  __FIXME__: This currently does nothing about escape chars.
--
-- @since 0.0.1.0
parseStrict :: (System.Rep rep) => rep -> Path 'Rel.Any 'Type.Any rep
parseStrict path =
  let (dir, file) = System.splitFileName path
   in ( if System.isValid file
          then Path.forgetType . flip Directory.selectFile file
          else Path.forgetType
      )
        $ parseDirectory dir

-- | Parse a directory path string.
--
--  __TODO__: Move this to pathway-system as an alternative to @MP.parse directory Format.local@.
--
--  __FIXME__: This currently does nothing about escape chars.
--
-- @since 0.0.1.0
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

-- | Serialize any path to a string representation.
--
-- @since 0.0.1.0
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

-- | Serialize a path to a string representation.
--
-- @since 0.0.1.0
serialize ::
  (Path.Pathy rel typ, System.Rep a) => Format a -> Path rel typ a -> a
serialize format = serializeAny format . Path.unanchor

localFormat :: forall rep. (System.Rep rep, Ord rep) => Format rep
localFormat =
  let proxy = Proxy :: Proxy rep
   in Format
        { Format.root = System.pack . pure $ System.pathSeparator proxy,
          Format.current = mempty,
          -- TODO: Figure out how to implement this without needing IO. I need
          --       to be able to create an `OsPath` literal for `".."`.
          Format.parent =
            error $
              "Internal xdg-base-directory error: This library doesn’t "
                <> "allow reparented paths, but one was encountered.",
          Format.separator = System.pack . pure $ System.pathSeparator proxy,
          Format.substitutions = mempty
        }

-- | Open a file for the given mode, performing an action with the handle.
--
--  __TODO__: Move this upstream.
--
-- @since 0.0.1.0
withFile ::
  forall m rep a.
  (MonadIO m, MonadMask m, System.Rep rep, Ord rep) =>
  Path 'Rel.Abs 'File rep ->
  IOMode ->
  (Handle -> m a) ->
  m a
withFile = System.withFile . serialize localFormat

-- | Create a directory and all missing parent directories.
--
-- @since 0.0.1.0
createDirectoryWithParentsIfMissing ::
  (System.Rep rep, Ord rep) =>
  Path 'Path.Abs 'Dir rep ->
  ExceptT Dir.MaybeParentCreationFailure IO ()
createDirectoryWithParentsIfMissing =
  ExceptT
    . tryJust
      ( \e ->
          if
            | IO.isFullError e -> pure Dir.FullError
            | True -> empty
      )
    . System.createDirectoryIfMissing True
    . serialize localFormat

-- | Check whether a directory exists.
--
-- @since 0.0.1.0
doesDirectoryExist ::
  (System.Rep rep, Ord rep) => Path 'Path.Abs 'Dir rep -> IO Bool
doesDirectoryExist =
  System.doesDirectoryExist . serialize localFormat
