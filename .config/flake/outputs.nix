### This is a complicated flake. Here’s the rundown:
###
### overlays.default – includes all of the packages from cabal.project
### packages = {
###   default = points to `packages.${defaultGhcVersion}`
###   <ghcVersion>-<cabal-package> = an individual package compiled for one
###                                  GHC version
###   <ghcVersion>-all = all of the packages in cabal.project compiled for one
###                      GHC version
### };
### devShells = {
###   default = points to `devShells.${defaultGhcVersion}`
###   <ghcVersion> = a shell providing all of the dependencies for all
###                  packages in cabal.project compiled for one GHC version
### };
### checks.format = verify that code matches Ormolu expectations
{
  flake-utils,
  flaky,
  flaky-haskell,
  nixpkgs,
  pathway,
  self,
  systems,
}: let
  pname = "xdg-base-directory";

  supportedSystems = import systems;

  cabalProject = ../../cabal.project;

  ## Nixpkgs pins no-recursion 0.1.2.3, which predates the `ignore-methods`
  ## plugin option that `Data.Annotated` relies on to silence the recursive
  ## default definitions of `sconcat` and `stimes`.
  ##
  ## TODO: Push this to Flaky, which is where every project gets its linting
  ##       plugins from.
  lintPlugins = _hfinal: hprev: {
    no-recursion = hprev.callHackageDirect {
      pkg = "no-recursion";
      ver = "0.4.0.1";
      sha256 = "sha256-sYHocSgSyzFVlRhBFRuuuzjckOpq+qHhTD8XNEHwxhA=";
    } {};
  };

  ## NB: This inlines `flaky-haskell.lib.cabalProject2nix` so that it can pass
  ##     `--flag=lint` to cabal2nix. Setting the flag only in `configureFlags`
  ##     isn’t enough: the linting plugins are conditional on it, so cabal2nix
  ##     doesn’t record them as dependencies unless it sees the flag itself,
  ##     and the build then fails to find them.
  ##
  ## TODO: Have `cabalProject2nix` take cabal2nix options upstream, then drop
  ##       this.
  cabalPackages = pkgs: hpkgs:
    builtins.mapAttrs
    (name: path:
      pkgs.shellchecked
      ((hpkgs.callCabal2nixWithOptions name
        (builtins.dirOf cabalProject + "/${path}")
        "--flag=lint"
        {})
        .overrideAttrs (old: {
        configureFlags = old.configureFlags ++ ["--ghc-options=-Werror"];
      })))
    (flaky-haskell.lib.parseCabalProject cabalProject);
in
  {
    schemas = {
      inherit
        (flaky.schemas)
        overlays
        homeConfigurations
        packages
        devShells
        projectConfigurations
        checks
        formatter
        ;
    };

    # see these issues and discussions:
    # - NixOS/nixpkgs#16394
    # - NixOS/nixpkgs#25887
    # - NixOS/nixpkgs#26561
    # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
    overlays = {
      default = final:
        nixpkgs.lib.composeManyExtensions [
          flaky.overlays.default
          (flaky-haskell.lib.overlayHaskellPackages
            (map self.lib.nixifyGhcVersion
              (self.lib.supportedGhcVersions final.system))
            (final: prev:
              nixpkgs.lib.composeManyExtensions [
                (flaky.overlays.haskellDependencies final prev)
                (self.overlays.haskell final prev)
                (self.overlays.haskellDependencies final prev)
              ]))
        ]
        final;

      haskell = flaky-haskell.lib.haskellOverlay cabalPackages;

      ## NB: Dependencies that are overridden because they are broken in
      ##     Nixpkgs should be pushed upstream to Flaky. This is for
      ##     dependencies that we override for reasons local to the project.
      haskellDependencies = final: prev:
        nixpkgs.lib.composeManyExtensions [
          (pathway.overlays.haskell final prev)
          lintPlugins
        ];
    };

    homeConfigurations =
      builtins.listToAttrs
      (builtins.map
        (flaky.lib.homeConfigurations.example self [
          ({pkgs, ...}: {
            home.packages = [
              (pkgs.haskellPackages.ghcWithPackages (hpkgs: [hpkgs.${pname}]))
            ];
          })
        ])
        supportedSystems);

    lib = {
      nixifyGhcVersion = version:
        "ghc" + nixpkgs.lib.replaceStrings ["."] [""] version;

      ## TODO: Extract this automatically from `pkgs.haskellPackages`.
      defaultGhcVersion = "9.10.3";

      ## Test the oldest revision possible for each minor release. If it’s not
      ## available in nixpkgs, test the oldest available, then try an older
      ## one via GitHub workflow. Additionally, check any revisions that have
      ## explicit conditionalization. And check whatever version `pkgs.ghc`
      ## maps to in the nixpkgs we depend on.
      testedGhcVersions = system:
        [
          self.lib.defaultGhcVersion
          "9.4.8"
          "9.6.7"
          "9.8.4"
        ]
        ++ nixpkgs.lib.optionals (system != "i686-linux") [
          "9.12.3" # GHC fails to build on i686-linux
        ];

      ## The versions that are older than those supported by Nix that we
      ## prefer to test against.
      nonNixTestedGhcVersions = [
        # Yaya requires at least GHC 8.6.1.
        "8.6.1"
        "8.8.1"
        "8.10.1"
        "9.0.1"
        "9.2.1"
        "9.4.1"
        "9.6.1"
        ## since `cabal-plan-bounds` doesn’t work under Nix
        "9.8.1"
        "9.10.1"
        "9.12.1"
        "9.14.1"
      ];

      ## However, provide packages in the default overlay for _every_
      ## supported version.
      supportedGhcVersions = self.lib.testedGhcVersions;
    };
  }
  // flake-utils.lib.eachSystem supportedSystems
  (system: let
    pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
      ## NB: This uses `self.overlays.default` because packages need to be
      ##     able to find other packages in this flake as dependencies.
      self.overlays.default
    ];
  in {
    packages =
      {
        default =
          self.packages.${system}."${self.lib.nixifyGhcVersion self.lib.defaultGhcVersion}_all";
      }
      // flaky-haskell.lib.mkPackages
      pkgs
      (map self.lib.nixifyGhcVersion (self.lib.supportedGhcVersions system))
      cabalPackages;

    devShells =
      {
        default =
          self.devShells.${system}.${self.lib.nixifyGhcVersion self.lib.defaultGhcVersion};
      }
      // self.projectConfigurations.${system}.devShells
      // flaky-haskell.lib.mkDevShells
      pkgs
      (map self.lib.nixifyGhcVersion (self.lib.supportedGhcVersions system))
      cabalPackages
      (hpkgs:
        [self.projectConfigurations.${system}.packages.path]
        ## NB: Haskell Language Server no longer supports GHC <9.6.
        ++ nixpkgs.lib.optional
        (nixpkgs.lib.versionAtLeast hpkgs.ghc.version "9.6"
          ## TODO: With Nixpkgs 25.11, HLS complains about conflicting package
          ##       versions in these combinations, so skip it.
          && !(pkgs.stdenv.hostPlatform.isLinux
            && nixpkgs.lib.versionAtLeast hpkgs.ghc.version "9.8"
            && nixpkgs.lib.versionOlder hpkgs.ghc.version "9.10"))
        hpkgs.haskell-language-server);

    projectConfigurations =
      flaky.lib.projectConfigurations.haskell {inherit pkgs self;};

    checks = self.projectConfigurations.${system}.checks;
    formatter = self.projectConfigurations.${system}.formatter;
  })
