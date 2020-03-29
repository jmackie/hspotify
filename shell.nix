{ compiler ? "ghc883" }:
let
  # nix-prefetch-git https://github.com/NixOS/nixpkgs
  nixpkgs-source = builtins.fromJSON ''
    {
      "url": "https://github.com/NixOS/nixpkgs",
      "rev": "728188794e6ce021948697c967318934907d67fa",
      "date": "2020-03-27T19:18:05+01:00",
      "sha256": "1w3sxx15njjxdiw9dk75lm3cb1lyka9ib1gicpf4chxvbrxrr6nv",
      "fetchSubmodules": false
    }
  '';

  pkgs = import (builtins.fetchTarball {
    name = "nixpkgs-${nixpkgs-source.rev}";
    url =
      "https://github.com/nixos/nixpkgs/archive/${nixpkgs-source.rev}.tar.gz";
    inherit (nixpkgs-source) sha256;
  }) {
    config.allowBroken = true; # for refined
  };

  haskellPackages = pkgs.haskell.packages.${compiler};

  hspotify = haskellPackages.callCabal2nix "hspotify" ./. { };
in haskellPackages.shellFor {
  packages = _: [ hspotify ];
  buildInputs =
    [ pkgs.gnumake pkgs.cabal-install pkgs.ghcid pkgs.hlint pkgs.ormolu ];
}
