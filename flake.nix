{

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      supportedSystems = [
        # "aarch64-linux"
        # "aarch64-darwin"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs.lib) concatStringsSep;
        includes = [
          "${pkgs.SDL2.dev}/include"
          "${pkgs.SDL2.dev}/include/SDL2"
          "${pkgs.SDL2_ttf}/include"
          #"${pkgs.SDL2_net.dev}/include"
          "${pkgs.SDL2_gfx}/include"
          "${pkgs.SDL2_mixer}/include"
          "${pkgs.SDL2_image}/include"
        ];
        INCLUDE_PATH = concatStringsSep ":" includes;
        withGdb = bin: pkgs.writeShellScriptBin "${bin.name}-with-gdb" "${pkgs.gdb}/bin/gdb ${bin}/bin/${bin.name}";
      in
      {
        packages = {
        };


        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              SDL2
              SDL2_image
              pkg-config
              elan
            ];
            C_INCLUDE_PATH = INCLUDE_PATH;
          };
        };
      });
}
