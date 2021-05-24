{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  myHaskellPackages = ps:
    with ps; [
      servant-server
      base64-bytestring
      hex-text
    ];
  myGhc = haskellPackages.ghcWithHoogle myHaskellPackages;
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ myGhc (haskellPackages.haskell-language-server) ];
}
