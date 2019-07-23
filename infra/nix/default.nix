{}:
with rec
{ sources = import ./sources.nix;
  pkgs_ = import sources.nixpkgs {};
  mkPackage = pkgs: hsuper: name: path:
    with
    { addStaticLinkerFlagsWithPkgconfig =
        haskellDrv:
        pkgConfigNixPackages:
        pkgconfigFlagsString:
        pkgs.haskell.lib.overrideCabal
          (pkgs.haskell.lib.appendConfigureFlag haskellDrv
            [ "--ld-option=-Wl,--start-group" ]
          )
          (old:
            { preConfigure =
                builtins.concatStringsSep "\n"
                  [ (old.preConfigure or "")
                    ''
                      set -e
                      configureFlags+=$(for flag in $(pkg-config --static ${pkgconfigFlagsString}); do echo -n " --ld-option=$flag"; done)
                    ''
                  ];
              libraryPkgconfigDepends =
                (old.libraryPkgconfigDepends or []) ++
                pkgConfigNixPackages;
            }
          );
    };
    { ${name} =
        with { drv = hsuper.callCabal2nix name (pkgs.lib.cleanSource path) {}; };
        with pkgs.haskell.lib;
        disableLibraryProfiling (
        disableExecutableProfiling (
          with { openssl_static = pkgs.openssl.override { static = true; }; };
          addStaticLinkerFlagsWithPkgconfig drv [ openssl_static ]
            "--libs openssl"
        ));
    };

  pkgs = import sources.nixpkgs
    { overlays =
        [ (_: pkgs: pkgs.lib.recursiveUpdate pkgs
            { haskell = pkgs.lib.recursiveUpdate pkgs.haskell
              { packages = pkgs.lib.recursiveUpdate pkgs.haskell.packages
                { ghc864 = pkgs.haskell.packages.ghc864.override
                  (old:
                    { overrides =
                        pkgs.lib.composeExtensions
                        (old.overrides or (_: _: {}))
                        (hself: hsuper:
                          mkPackage pkgs hsuper "deckdeckgo-handler"
                            ../handler //
                          mkPackage pkgs hsuper "firebase-login"
                            ../firebase-login //
                          mkPackage pkgs hsuper "unsplash-proxy"
                            ../unsplash-proxy //
                          mkPackage pkgs hsuper "wai-lambda"
                            wai-lambda.wai-lambda-source //
                          mkPackage pkgs hsuper "google-key-updater"
                            ../google-key-updater
                        );
                    }
                  );
                };
              };

              # for cabal2nix
              nix = pkgs_.nix;
              git = pkgs_.git;
              subversion = pkgs_.subversion;
            }
          )

          (_: pkgs:
            {
              # Fails during tests
              go_1_12 = pkgs.go_1_12.overrideAttrs (_:
                { doCheck = false; }
                );

              # for napalm
              nodejs = pkgs.nodejs-12_x;
            }
          )
        ];
    };

  wai-lambda = pkgs.callPackage "${sources.wai-lambda}/nix/packages.nix" {};

  survey = import "${sources.static-haskell-nix}/survey"
    { normalPkgs = pkgs; };

  haskellPackages =
    survey.pkgsWithStaticHaskellBinaries.haskellPackages;
};

pkgs //
{ inherit haskellPackages sources wai-lambda; } //
{ terraform = pkgs.terraform_0_12 ; }
