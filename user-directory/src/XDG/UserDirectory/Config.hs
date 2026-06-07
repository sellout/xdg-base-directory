{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Configuration loading from @$XDG_CONFIG_HOME/user-dirs.dirs@.
module XDG.UserDirectory.Config
  ( BaseOps.AggregateDirWarnings,
    defaultFile,
    withConfigFrom,
    withConfig,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<))
import safe "base" Control.Monad.IO.Class (MonadIO, liftIO)
import safe "base" Data.Bool (Bool (False))
import safe "base" Data.Either (Either)
import safe "base" Data.Function (const, ($))
import safe "base" Data.List.NonEmpty (NonEmpty)
import safe "base" Data.String (IsString, String)
import safe "base" Data.Traversable (traverse)
import safe qualified "base" System.IO as IO
import safe "exceptions" Control.Monad.Catch (MonadMask)
import safe qualified "megaparsec" Text.Megaparsec as MP
import safe "pathway" Data.Path (Path, Relativity (Rel), Type (Dir, File))
import safe qualified "pathway" Data.Path as Path
import safe qualified "pathway" Data.Path.Format as Format
import safe "pathway" Data.Path.TH (posix)
import safe qualified "pathway-system" System.Path as SysPath
import safe qualified "pathway-system" System.Text as SysText
import "variant" Data.Variant (V)
import safe "xdg-base-directory" Data.Annotated (Annotated)
import safe qualified "xdg-base-directory" XDG.BaseDirectory.Opinionated as BaseOps
import safe qualified "this" XDG.UserDirectory.Parser as Parser
import safe "base" Prelude (error)

xdg ::
  (MonadIO m, MonadMask m, SysPath.Rep rep, SysText.Rep rep, SysPath.Operations rep 'Dir) =>
  BaseOps.Operations m rep
xdg = BaseOps.bleedingOperations . const $ pure [posix|/|]

-- | The location of the XDG user dirs config file.
--
-- @since 0.0.1.0
defaultFile :: (IsString rep) => Path ('Rel 'False) 'File rep
defaultFile = [posix|user-dirs.dirs|]

withParsedFile ::
  (MonadIO m, MonadMask m) =>
  MP.Parsec (Parser.InvalidDir String) String a ->
  Path ('Rel 'False) 'File String ->
  (a -> m b) ->
  m
    ( Annotated
        (BaseOps.AggregateDirWarnings String)
        ( Annotated
            (NonEmpty (V SysPath.OpenFileFailure))
            (Either Parser.Error b)
        )
    )
withParsedFile parser file action =
  BaseOps.withAggregateFiles xdg BaseOps.Config file $ \case
    (filepath, handle) : _ ->
      -- TODO: This should print the absolute path, not the relative one, so we
      --       need to include those in the action argument.
      traverse action . MP.parse parser (Path.toText Format.local filepath)
        <=< liftIO
        $ IO.hGetContents handle
    [] -> error "no files"

-- | Perform some operation on the `Parser.UserDirsConfig` extracted from the
--   provided file.
--
-- @since 0.0.1.0
withConfigFrom ::
  (MonadIO m, MonadMask m) =>
  Path ('Rel 'False) 'File String ->
  (Parser.UserDirsConfig String -> m a) ->
  m
    ( Annotated
        (BaseOps.AggregateDirWarnings String)
        ( Annotated
            (NonEmpty (V SysPath.OpenFileFailure))
            (Either Parser.Error a)
        )
    )
withConfigFrom = withParsedFile Parser.userDirs

-- | Perform some operation on the `Parser.UserDirsConfig` extracted from
--   `defaultFile`.
--
-- @since 0.0.1.0
withConfig ::
  (MonadIO m, MonadMask m) =>
  (Parser.UserDirsConfig String -> m a) ->
  m
    ( Annotated
        (BaseOps.AggregateDirWarnings String)
        ( Annotated
            (NonEmpty (V SysPath.OpenFileFailure))
            (Either Parser.Error a)
        )
    )
withConfig = withConfigFrom defaultFile
