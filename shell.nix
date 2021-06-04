let nixpkgs_source = (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz);
in {pkgs ? import nixpkgs_source {
    inherit system;
  }, system ? builtins.currentSystem}:

let

   # nixpkgs_source = /local_dir; # for local directory
   # nixpkgs_source = nixpkgs.fetchFromGitHub { # for safety of checking the hash
   #    owner = "jyp";
   #    repo = "nixpkgs";
   #    rev = "e32d48eb4d2a4c2d668cc8b1ca77bb03c2510b74";
   #    sha256 = "02dwn1k1yxc8d2fviynalb43csggd0v3lmsaaammswkqcyb7m836";
   #  };
   # nixpkgs_source = ~/repo/nixpkgs;

  hp = pkgs.haskellPackages.override{
      overrides = self: super: {
        pretty-compact = self.callPackage ./pretty-compact.nix {};
        # typedflow = self.callPackage ./typedflow.nix {};
        
        gasp = self.callPackage ({ mkDerivation, base, binary, containers, fetchgit, mtl, QuickCheck , stdenv
                 }:
                 mkDerivation {
                   pname = "gasp";
                   version = "1.3.0.0";
                   src = fetchgit {
                     url = "https://github.com/jyp/gasp.git";
                     sha256 = "0fd18x83sjxnqkbikb93rdl2vffmxh3835isiy1b7ilikbdpkmx5";
                     rev = "c70466868c8436a759f0603815a22d33a4fe38cf";
                     fetchSubmodules = true;
                   };
                   libraryHaskellDepends = [ base binary containers mtl QuickCheck ];
                   description = "A framework of algebraic classes";
                   license = stdenv.lib.licenses.bsd3;
                 }) {};
                       };};
  ghc = hp.ghcWithPackages (ps: with ps; ([ cabal-install statistics QuickCheck gasp ]));

in pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ ghc ];
  shellHook = ''
    export LANG=en_US.UTF-8
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}

