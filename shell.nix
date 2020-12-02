let
  pkgs = import <nixpkgs> {};
  projectDrv = (import ./release.nix { } ).project1.env;
in
  projectDrv

