# XDG Base Directory

[![Hackage Version](https://img.shields.io/hackage/v/xdg-base-directory)](https://hackage.haskell.org/package/xdg-base-directory)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:xdg-base-directory.svg)](https://repology.org/project/haskell:xdg-base-directory/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:xdg-base-directory.svg)](https://repology.org/project/haskell:xdg-base-directory/versions)

A faithful implementation of [the XDG Base Directory spec](https://specifications.freedesktop.org/basedir-spec/latest/)

This tries to capture the spec as precisely as possible, and gives the user a safe API that’s hard to misuse.

## usage

**NB**: This library exposes [Pathway](https://github.com/sellout/pathway) types for typed file paths. You’ll have to make some use of it directly to work with this library.

- [comparisons to similar projects](#comparisons)

The recommended entry point to this library is [`XDG.BaseDirectory.Opinionated`](./src/XDG/BaseDirectory/Opinionated.hs). It implements [§4](https://specifications.freedesktop.org/basedir-spec/0.8/#referencing) of the spec, which is generally overlooked by libraries implementing the spec.

If that’s too inflexible for you, 1. open an issue (or PR) and 2. use one of the other modules.

- [`XDG.BaseDirectory.IO`](./src/XDG/BaseDirectory/IO.hs) has a similar interface, but doesn’t enforce the program subdirectory.
- [`XDG.BaseDirectory`](./src/XDG/BaseDirectory.hs) exposes the base directories directly, allowing you to do pretty much anything.

The companion XDG user directories spec (the one behind `user-dirs.dirs`) is a separate spec, and lives in a separate package: [xdg-user-directory](https://github.com/sellout/xdg-user-directory), which depends on this one.

### lower-level

For particular use cases, you might need deeper access to things.

For example, if you’re integrating this with a library that handles things differently on different platforms (like macOS and Windows), [`XDG.BaseDirectory.Custom`](./src/XDG.BaseDirectory/Custom.hs) provides only the values set in the environment, allowing you to use different defaults on different platforms.

A number of modules expose roughly the same ten identifiers:

|              | `BaseDirectory` | `.Var` | `.Default` | `.Default.Relative` | `.Custom` |
| ------------ | --------------- | ------ | ---------- | ------------------- | --------- |
| `configHome` | ✔               | ✔      | ✔          | ✔                   | ✔         |
| `stateHome`  | ✔               | ✔      | ✔          | ✔                   | ✔         |
| `cacheHome`  | ✔               | ✔      | ✔          | ✔                   | ✔         |
| `dataDirs`   | ✔               | ✔      | ✔          |                     | ✔         |
| `configDirs` | ✔               | ✔      | ✔          |                     | ✔         |
| `runtimeDir` | ✔               | ✔      |            |                     | ✔         |
| `binHome`    | ✔               |        | ✔          | ✔                   |           |
| `datadir`    | ✔               |        | ✔          |                     | ✔         |
| `sysconfdir` | ✔               |        | ✔          |                     | ✔         |

The first six of these correspond to the `XDG_*` variables defined by the XDG base directory specification. The other three are slightly different: `binHome` refers specifically to ‘.local/bin/’, which is defined in the spec, but doesn’t have a variable associated with it; and `datadir` & `sysconfdir` are GNU Make variables that are determined at _compile_ time (they’re referenced in the spec, but aren’t defined by it).

The modules that expose these are:

- `XDG.BaseDirectory` – the fully resolved values – using those from `XDG.BaseDirectory.Custom` and falling back to `XDG.BaseDirectory.Default` when there’s no variable set. This is similar to the API offered by most such libraries.
- `XDG.BaseDirectory.Var` – the environment variables themselves (so only exposes the first seven identifiers)
- `XDG.BaseDirectory.Default` – the default values from the spec, which are defined for everything except `runtimeDir`
- `XDG.BaseDirectory.Default.Relative` – relative versions (that is, without `$HOME`) of all the `*Home` identifiers
- `XDG.BaseDirectory.Custom` – values read from the variables either in `XDG.BaseDirectory.Var` or the GNU Make variables (this doesn’t expose `binHome`, because that isn’t customizable)

### deviations from the spec

This library tries extremely hard to follow the spec. One place that it differs is that it uses `getHomeDirectory` rather than `looupEnv "HOME"` to get the home directory for default values. This means that an unset `HOME` will result in us looking up the home directory from /etc/passwd, but should also help make this library function more effectively across platforms.

The [`XDG.BaseDirectory.Opinionated`](./src/XDG/BaseDirectory/Opinionated.hs) goes a bit beyond the spec, by enforcing a program subdirectory. This is usually what you want to do, but if you have already been putting files directly into a base directory, then you might want to use [`XDG.BaseDirectory.IO`](./src/XDG/BaseDirectory/IO.hs), which doesn’t enforce the intervening subdirectory.

### error reporting

This library uses a few different types of error:

- `Either e a` – You’re used to this one – `Left e` is an error, `Right a` is a successful result.
- `These e a` – This is similar to `Either` – `This e` is like `Left e`, `That a` is like `Right a`, but there is also a third case: `These e a`, which has both error and success values. You can think of `These` as carrying a warning.
- `Annotated e a` – This now removes the error cases from before – `NotBut a` is like `Right a` or `That a`, and `Noted e a` is like `These e a`, but there isn’t anything like `Left e` or `This e`. This type can carry warnings, but not errors.

Why do we care about warnings? XDG Base Directory lists a number of cases where some kind of failure (for example, a relative path in one of the environment variables, a directory in `XDG_DATA_DIRS` that doesn’t contain the file we’re looking for, etc.) are intended to be passed over … as long as there are still other options. We preserve the errors in that case as warnings, which can be helpful in debugging. For example, why a file wasn’t found where you were expecting.

|             | failure | success  | warning |
| ----------- | ------- | -------- | ------- |
| `Either`    | `Left`  | `Right`  |         |
| `These`     | `This`  | `That`   | `These` |
| `Annotated` |         | `NotBut` | `Noted` |

Many of the errors or warnings are structured (generally `NonEmpty e`), so we don’t just track the first thing that broke, so you can all the things that affected your results.

We could track errors and warnings separately, especially when they have different types. Right now we conflate them, and when you get a `NonEmpty` failure, you can assume that all except for the last one are warnings.

### specification issues

Perhaps partially because of its popularity, the XDG Base Directory spec has taken a lot of flak for various shortcomings. Here are some that I’ve run into and how this library resolves them (until something more official comes along).

#### What files go where?

The spec is fairly sparse with regard to explaining what belongs in each location. Other, unofficial, sources try to give more guidance.

### licensing

This package is licensed under [The GNU AGPL 3.0 only, with some alternatives](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20xdg-base-directory).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

### versioning

This project largely follows a strict variation of the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP). See [the specifics](https://sellout.github.io/flaky/haskell/strict-PVP.html).

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)

## comparisons

Other projects similar to this one, and how they differ.

### [directory](https://hackage.haskell.org/package/directory)

- Included with GHC.
- Provides both `FilePath` and `OsPath`-based interfaces
- Doesn’t support `$HOME/.local/bin` or `XDG_RUNTIME_DIR`.
- Exposes the variable contents directly (as does this library), but does nothing to support the specification’s guidelines for accessing files. For example, `getXdgDirectoryList` doesn’t prepend the corresponding `XDG_*_HOME` value to the list.
- No support for warnings.

### [XDG-BaseDir](https://hackage.haskell.org/package/xdg-basedir)

- Implements corresponding functionality for Windows, unrelated to the XDG Base Directory spec. I think this is better extracted to a separate library.
- Exposes the variable contents directly (as does this library), but does nothing to support the specification’s guidelines for accessing files.
- Doesn’t support `XDG_STATE_HOME`, `$HOME/.local/bin`, or `XDG_RUNTIME_DIR`.
- Uses `System.FilePath.FilePath` directly, with no support for typed paths.
- No support for warnings.

### [xdg-basedir-compliant](https://hackage.haskell.org/package/xdg-basedir-compliant)

- Uses the [Path](https://hackage.haskell.org/package/path) library internally, but the public API uses `System.FilePath.FilePath`.
- Exposes `read*` and `write*` operations, but doesn’t support writing to system data & config files.
- Module names don’t indicate the Base Directory spec at all, only XDG.
- Uses exceptions rather than tracking errors.
- No support for warnings.

### in other languages

#### Rust’s [`xdg`](https://crates.io/crates/xdg)

#### Python’s [`platformdirs`](https://platformdirs.readthedocs.io/en/latest/)

#### Python’s [config-path](https://pypi.org/project/config-path/)

- Only handles config files, not other file locations.

#### Python’s deprecated [`appdirs`](https://pypi.org/project/appdirs/)
