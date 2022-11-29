{ pkgs ? import <nixpkgs> {} }:
let
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    stack
    haskell.compiler.ghc902
    haskell.packages.ghc902.cabal-install
    haskell.packages.ghc902.haskell-language-server
    haskell.packages.ghc902.hlint
  ];
  shellHook = ''
  '';
}
