{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Dependencies
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-language-server
  ];
}
