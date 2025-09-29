{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  }) {}
}:

pkgs.mkShell {
  nativeBuildInputs = with pkgs.ocamlPackages; [
    dune_3
    findlib
    ocaml
    lablgtk
  ];
  buildInputs = with pkgs.ocamlPackages; [
    graphics
  ];
}
