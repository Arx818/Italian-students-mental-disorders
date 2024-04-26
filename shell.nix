{ pkgs ? import <nixos-unstable> {} }:
let
  python-with-pkgs = pkgs.python3.withPackages (p: with p; [
    pandas
    matplotlib
  ]);
  ## R deps for RStudio
  # R-with-pkgs = pkgs.rstudioWrapper.override{ packages = with pkgs.rPackages; [
  #   ggplot2
  #   ggExtra
  #   gridExtra
  #   reshape2
  #   MuMIn
  #   afex
  #   emmeans
  #   effects
  #   lme4
  #   lemon
  # ];};
in
pkgs.mkShell {
  buildInputs = [
    python-with-pkgs
#    R-with-pkgs
#    pkgs.rstudio
    pkgs.libreoffice
  ];
}
