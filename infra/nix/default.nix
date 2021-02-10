{}:
let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {
    overlays = [
      (
        self: super:
          super.lib.optionalAttrs super.stdenv.hostPlatform.isMusl
            {


              postgresql = (super.postgresql.overrideAttrs (old: { dontDisableStatic = true; })).override {
                # We need libpq, which does not need systemd,
                # and systemd doesn't currently build with musl.
                enableSystemd = false;
              };
              lzma = super.lzma.overrideAttrs (old: { dontDisableStatic = true; });

              openssl = super.openssl.override { static = true; };
            }
      )
    ];
  };
  staticPkgs = pkgs.pkgsMusl;
  compiler = "ghc865";

  addStaticLinkerFlagsWithPkgconfig =
    haskellDrv:
    pkgConfigNixPackages:
    pkgconfigFlagsString:
      pkgs.haskell.lib.overrideCabal
        (
          pkgs.haskell.lib.appendConfigureFlag haskellDrv
            [ "--ld-option=-Wl,--start-group" ]
        )
        (
          old:
            {
              preConfigure =
                builtins.concatStringsSep "\n"
                  [
                    (old.preConfigure or "")
                    ''
                      set -e
                      configureFlags+=$(for flag in $(pkg-config --static ${pkgconfigFlagsString}); do echo -n " --ld-option=$flag"; done)
                    ''
                  ];
              libraryPkgconfigDepends =
                (old.libraryPkgconfigDepends or []) ++ pkgConfigNixPackages;
            }
        );


  opensslStatic = staticPkgs.openssl.dev;

  foo = self: drv:
    addStaticLinkerFlagsWithPkgconfig drv [ opensslStatic staticPkgs.postgresql ]
      "--libs openssl --libs libpq";

  final = staticPkgs;

  staticHaskellPackages = with pkgs.haskell.lib; staticPkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      google-key-updater = foo self (self.callPackage ../google-key-updater {});
      deckdeckgo-handler = foo self (self.callPackage ../handler {});
      unsplash-proxy = foo self (self.callPackage ../unsplash-proxy {});
      firebase-login = foo self (self.callPackage ../firebase-login {});

      postgresql-libpq = foo self super.postgresql-libpq;
      postgresql-binary = foo self super.postgresql-binary;
      hasql = foo self super.hasql;
    };
  };

in

pkgs // { inherit staticHaskellPackages sources; } // { terraform = pkgs.terraform_0_12; }
