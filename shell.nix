{ nixpkgs ? import <nixpkgs> { }
, ghc     ? nixpkgs.ghc
}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "tower-mynewt";
  inherit ghc;
}
