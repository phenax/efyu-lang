{ nixpkgs ? import <nixpkgs> { }
, haskellPackages ? nixpkgs.haskellPackages
, nodePackages ? nixpkgs.nodePackages
, compiler ? "default"
, doBenchmark ? false
}:

let
  inherit (nixpkgs) pkgs;
  systemPackages = [
    haskellPackages.haskell-language-server
    haskellPackages.cabal-install
    haskellPackages.hpack
    nodePackages.nodemon
    pkgs.zlib
  ];

  commonHsPackages = with haskellPackages; [ ];
in
with haskellPackages; mkDerivation {
  pname = "efyu-lang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = commonHsPackages;
  executableHaskellDepends = commonHsPackages;
  executableSystemDepends = systemPackages;
  testHaskellDepends = commonHsPackages; # ++ [ hspec ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
