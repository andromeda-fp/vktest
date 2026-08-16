let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShellNoCC {
  packages = [
    pkgs.cabal-install
    pkgs.ghc
    pkgs.vulkan-headers
    pkgs.vulkan-loader
    pkgs.pkg-config
  ];
  LIBRARY_PATH = "${pkgs.vulkan-loader}/lib";
}
