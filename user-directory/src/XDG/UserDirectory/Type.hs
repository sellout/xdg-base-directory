{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- The well-known user directory types from the xdg-user-dirs specification.
-- See <https://www.freedesktop.org/wiki/Software/xdg-user-dirs/>.
module XDG.UserDirectory.Type
  ( UserDirectory (..),
    DirectoryValue (..),
    UserDirsConfig,
    envVarName,
  )
where

import "base" Data.Bool (Bool (False))
import "base" Data.Eq (Eq)
import qualified "base" Data.Kind as Kind
import "base" Data.Ord (Ord)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" GHC.Generics (Generic)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "containers" Data.Map (Map)
import "pathway" Data.Path (Path, Relativity (Abs, Rel), Type (Dir))

-- | A user directory type from the xdg-user-dirs specification.
--
--   The spec defines a set of well-known directories, but the format is
--   extensible. Use the provided constants ('desktop', 'download', etc.) for
--   standard directories, or construct a 'UserDirectory' directly for custom
--   ones.
--
--   The string should be the middle part of the variable name, e.g.,
--   @\"DESKTOP\"@ for @XDG_DESKTOP_DIR@.
--
-- @since 0.0.1.0
newtype UserDirectory = UserDirectory {formatUserDirectory :: String}
  deriving stock (Eq, Generic, Ord, Read, Show)

-- | Get the environment variable name for a user directory type.
--
--   This returns the variable name as it appears in @user-dirs.dirs@, e.g.,
--   @\"XDG_DESKTOP_DIR\"@ for 'desktop'.
--
-- @since 0.0.1.0
envVarName :: UserDirectory -> String
envVarName (UserDirectory name) = "XDG_" <> name <> "_DIR"

-- | A directory value from the config file.
--
--   We preserve paths relative to `$HOME` for two reasons
--
-- 1. it avoids new failure cases when parsing and
-- 2. it allows us to distinguish them more easily.
--
-- @since 0.0.1.0
data DirectoryValue (rep :: Kind.Type)
  = -- | A path relative to @$HOME@
    HomeRelative (Path ('Rel 'False) 'Dir rep)
  | -- | An absolute path
    Absolute (Path 'Abs 'Dir rep)
  deriving stock (Eq, Generic, Ord, Show)

type role DirectoryValue nominal

-- | Parsed user directories configuration.
--
-- @since 0.0.1.0
type UserDirsConfig (rep :: Kind.Type) =
  Map UserDirectory (DirectoryValue rep) :: Kind.Type
