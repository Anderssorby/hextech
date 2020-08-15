let
  pkgs = import <nixpkgs> { };

in
  { backend  = pkgs.haskellPackages.callPackage ./backend/default.nix { };
    graphics = pkgs.haskellPackages.callPackage ./graphics/default.nix { };
  }
