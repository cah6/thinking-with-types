let 
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-ghc843.json; };
  myPackages = (import ./release.nix { withHoogle = true; } );
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};

  projectDrvEnv = myPackages.project1.env.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [ 
      pinnedPkgs.haskellPackages.cabal-install
      all-hies.versions.ghc843
      ];
    shellHook = ''
      export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
    '';
  });
in 
  projectDrvEnv