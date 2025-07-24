# XDG Base Directory

[![Packaging status](https://repology.org/badge/tiny-repos/haskell:xdg-base-directory.svg)](https://repology.org/project/haskell:xdg-base-directory/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:xdg-base-directory.svg)](https://repology.org/project/haskell:xdg-base-directory/versions)

A faithful implementation of [the XDG Base Directory spec](https://specifications.freedesktop.org/basedir-spec/latest/)

This tries to capture the spec as precisely as possible, and gives the user a safe and simple API.

## usage

**NB**: This library exposes [Pathway](https://github.com/sellout/pathway) types for typed filepaths. You’ll have to make some use of it directly to work with this library.

The recommended entry point to this library is [`XDG.BaseDirectory.Opinionated`](./src/XDG/BaseDirectory/Opinionated.hs). It implements [§4](https://specifications.freedesktop.org/basedir-spec/0.8/#referencing) of the spec, which is generally overlooked by libraries implementing the spec.

If that is too inflexible for you, 1. open an issue (or PR) and 2. use one of the other modules.

- [`XDG.BaseDirectory.IO`](./src/XDG/BaseDirectory/IO.hs) has a similar interface, but doesn’t enforce the program subdirectory.
- [`XDG.BaseDirectory`](./src/XDG/BaseDirectory.hs) exposes the base directories directly, allowing you to do pretty much anything.

If you are integrating this with a library that handles things differently on different platforms (e.g., macOS and Windows), [`XDG.BaseDirectory.Custom`](./src/XDG.BaseDirectory/Custom.hs) gets provides only the values set in the environment, allowing you to use different defaults on different platforms.

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

## specification issues

Perhaps partially because of its popularity, the XDG Base Directory spec has taken a lot of flak for various shortcomings. Here are some that I’ve run into and how this library resolves them (until something more official comes along).

### What files go where?

The spec is fairly sparse with regard to explaining what belongs in each location. There are other, unofficial, sources that try to give more guidance.

## versioning

This project largely follows the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP), but is more strict in some ways.

The version always has four components, `A.B.C.D`. The first three correspond to those required by PVP, while the fourth matches the “patch” component from [Semantic Versioning](https://semver.org/).

Here is a breakdown of some of the constraints:

### sensitivity to additions to the API

PVP recommends that clients follow [these import guidelines](https://wiki.haskell.org/Import_modules_properly) in order that they may be considered insensitive to additions to the API. However, this isn’t sufficient. We expect clients to follow these additional recommendations for API insensitivity

If you don’t follow these recommendations (in addition to the ones made by PVP), you should ensure your dependencies don’t allow a range of `C` values. That is, your dependencies should look like

```cabal
yaya >=1.2.3 && <1.2.4
```

rather than

```cabal
yaya >=1.2.3 && <1.3
```

#### use package-qualified imports everywhere

If your imports are [package-qualified](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/package_qualified_imports.html?highlight=packageimports#extension-PackageImports), then a dependency adding new modules can’t cause a conflict with modules you already import.

#### avoid orphans

Because of the transitivity of instances, orphans make you sensitive to your dependencies’ instances. If you have an orphan instance, you are sensitive to the APIs of the packages that define the class and the types of the instance.

One way to minimize this sensitivity is to have a separate package (or packages) dedicated to any orphans you have. Those packages can be sensitive to their dependencies’ APIs, while the primary package remains insensitive, relying on the tighter ranges of the orphan packages to constrain the solver.

### transitively breaking changes (increments `A`)

#### removing a type class instance

Type class instances are imported transitively, and thus changing them can impact packages that only have your package as a transitive dependency.

#### widening a dependency range with new major versions

This is a consequence of instances being transitively imported. A new major version of a dependency can remove instances, and that can break downstream clients that unwittingly depended on those instances.

A library _may_ declare that it always bumps the `A` component when it removes an instance (as this policy dictates). In that case, only `A` widenings need to induce `A` bumps. `B` widenings can be `D` bumps like other widenings, Alternatively, one may compare the APIs when widening a dependency range, and if no instances have been removed, make it a `D` bump.

### breaking changes (increments `B`)

#### restricting an existing dependency’s version range in any way

Consumers have to contend not only with our version bounds, but also with those of other libraries. It’s possible that some dependency overlapped in a very narrow way, and even just restricting a particular patch version of a dependency could make it impossible to find a dependency solution.

#### restricting the license in any way

Making a license more restrictive may prevent clients from being able to continue using the package.

#### adding a dependency

A new dependency may make it impossible to find a solution in the face of other packages dependency ranges.

### non-breaking changes (increments `C`)

#### adding a module

This is also what PVP recommends. However, unlike in PVP, this is because we recommend that package-qualified imports be used on all imports.

### other changes (increments `D`)

#### widening a dependency range for non-major versions

This is fairly uncommon, in the face of `^>=`-style ranges, but it can happen in a few situations.

#### deprecation

**NB**: This case is _weaker_ than PVP, which indicates that packages should bump their major version when adding `deprecation` pragmas.

We disagree with this because packages shouldn’t be _publishing_ with `-Werror`. The intent of deprecation is to indicate that some API _will_ change. To make that signal a major change itself defeats the purpose. You want people to start seeing that warning as soon as possible. The major change occurs when you actually remove the old API.

Yes, in development, `-Werror` is often (and should be) used. However, that just helps developers be aware of deprecations more immediately. They can always add `-Wwarn=deprecation` in some scope if they need to avoid updating it for the time being.

## licensing

This package is licensed under [The GNU AGPL 3.0 or later](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20xdg-base-directory).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

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

#### Rust’s [xdg](https://crates.io/crates/xdg)

#### Python’s [platformdirs](https://platformdirs.readthedocs.io/en/latest/)

#### Python’s [config-path](https://pypi.org/project/config-path/)

- Only handles config files, not other file locations.

#### Python’s deprecated [appdirs](https://pypi.org/project/appdirs/)
