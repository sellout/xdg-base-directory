{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module XDG.UserDirectory.IO
  ( Error (..),
    withUserFile,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<))
import safe "base" Control.Monad.IO.Class (MonadIO, liftIO)
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (Bool (False))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.List.NonEmpty (NonEmpty)
import safe "base" Data.String (String)
import safe "base" Data.Traversable (traverse)
import safe qualified "base" System.IO as IO
import safe "exceptions" Control.Monad.Catch (MonadMask)
import safe "pathway" Data.Path (Path, Relativity (Rel), Type (File), (</>))
import safe "pathway-compat-base" System.IO.Pathway (OpenFileFailure)
import safe qualified "pathway-system" System.Path as SysPath
import "variant" Data.Variant (V)
import safe "xdg-base-directory" Data.Annotated (Annotated)
import safe "this" XDG.UserDirectory (UserDirectory, getUserDirectory)
import safe "this" XDG.UserDirectory.Config (AggregateDirWarnings, withConfig)
import safe qualified "this" XDG.UserDirectory.Parser as Parser

-- | A failure when retrieving a user directory.
--
-- @since 0.0.1
data Error
  = -- | Something was wrong with @$HOME@.
    --
    -- @since 0.0.1
    HomeDirFailure (V (SysPath.GetUserDirectoryFailure String))
  | -- | Something went wrong reading user-dirs.dirs.
    --
    -- @since 0.0.1
    UserDirConfigFailure Parser.Error

assocEither :: Either a (Either b c) -> Either (Either a b) c
assocEither = either (Left . Left) $ first pure

-- | Perform an action on some file in a user directory.
--
--   This can be useful in some cases (like, putting downloaded files into
--   `XDG.UserDirectory.Common.downloads`, but often you’ll want to use the
--   operations form "XDG.UserDirectory" directly, because you just want to
--   choose a /default/ location to set up a file browser or something, and let
--   the user choose where it’ll actually be saved).
--
-- @since 0.0.1
withUserFile ::
  (MonadIO m, MonadMask m) =>
  UserDirectory ->
  Path ('Rel 'False) 'File String ->
  IO.IOMode ->
  (IO.Handle -> m a) ->
  m
    ( Annotated
        (AggregateDirWarnings String)
        ( Annotated
            (NonEmpty (V OpenFileFailure))
            (Either Error (Either (V OpenFileFailure) a))
        )
    )
withUserFile userDir file mode action =
  fmap (fmap (first (either UserDirConfigFailure HomeDirFailure) . assocEither <$>))
    . withConfig
    $ traverse (\dir -> SysPath.withFile (dir </> file) mode action)
      <=< liftIO . getUserDirectory userDir
