let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  packages = [
    pkgs.cabal-install
    pkgs.ghc
    pkgs.llvm
    pkgs.libclang
    pkgs.pkg-config
    pkgs.vulkan-headers
    pkgs.vulkan-loader
  ];
  buildInputs = [
    pkgs.libllvm
    pkgs.vulkan-headers
    pkgs.vulkan-loader
  ];
  shellHook = ''
    BINDGEN_EXTRA_CLANG_ARGS="$(<${pkgs.clang}/nix-support/cc-cflags) $(<${pkgs.clang}/nix-support/libc-cflags) $NIX_CFLAGS_COMPILE"
    export BINDGEN_EXTRA_CLANG_ARGS
    BINDGEN_BUILTIN_INCLUDE_DIR=disable
    export BINDGEN_BUILTIN_INCLUDE_DIR
  '';
}
