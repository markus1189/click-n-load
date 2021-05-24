{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  myHaskellPackages = ps:
    with ps; [
      base64-bytestring
      either
      hex-text
      language-javascript
      lens
      servant-server
    ];
  myGhc = haskellPackages.ghcWithHoogle myHaskellPackages;
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ myGhc (haskellPackages.haskell-language-server) ];
}
