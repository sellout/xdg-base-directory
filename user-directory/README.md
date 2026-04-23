# xdg-user-directory

A Haskell implementation of the XDG user directories specification (xdg-user-dirs).

This library provides access to well-known user directories (Desktop, Documents, Downloads, etc.) via the `user-dirs.dirs` configuration file.

## Usage

```haskell
import XDG.UserDirectory

main :: IO ()
main = do
  desktop <- getUserDirectory Desktop
  case desktop of
    Left err -> putStrLn $ "Error: " ++ show err
    Right path -> putStrLn $ "Desktop: " ++ show path
```

## See also

- [xdg-base-directory](../core/) - XDG Base Directory specification implementation
- [freedesktop.org xdg-user-dirs](https://www.freedesktop.org/wiki/Software/xdg-user-dirs/)
