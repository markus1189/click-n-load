{
  description = "Flake for dev shell";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        myHaskellPackages = ps:
          with ps; [
            base64-bytestring
            either
            hex-text
            language-javascript
            lens
            servant-server
          ];
        myGhc = pkgs.haskellPackages.ghcWithHoogle myHaskellPackages;
        clickNLoad = pkgs.writeScriptBin "click-n-load" ''
          ${myGhc}/bin/runhaskell ${./clicknload-decryptor.hs}
        '';
      in {
        defaultPackage = clickNLoad;

        devShell = with pkgs;
          pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              myGhc
              (haskellPackages.haskell-language-server)
              clickNLoad
            ];
          };
      });
}
