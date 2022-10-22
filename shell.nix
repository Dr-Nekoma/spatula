{ pkgs ? import <nixpkgs> {} }:
let
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    stack
    haskell.compiler.ghc8107
    haskell.packages.ghc8107.cabal-install
    haskell.packages.ghc8107.haskell-language-server
    haskell.packages.ghc8107.hlint
  ];
  shellHook = ''
  '';
}
