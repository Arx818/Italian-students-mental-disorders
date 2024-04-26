{ python39Packages, stdenv, emacs, texlive }:

let
  tex = (texlive.combine {
    inherit (texlive) scheme-basic;
  });
in
stdenv.mkDerivation rec {
  name = "visual";
  src = ./.;
  
  buildInputs = [
    python39Packages.numpy
    python39Packages.pandas
    python39Packages.matplotlib
    emacs
    tex
  ];
  
  buildPhase = ''
    cd src/visual
    python .
    cd ../../
    emacs -Q --batch "(require 'ox-latex)" notebook.org -f org-latex-export-to-pdf
  '';
  installPhase = "cp -r notebook.pdf $out";
}
