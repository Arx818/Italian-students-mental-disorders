# Data Visualization on mental disorders among italian students

<img src="https://www.un.org/sites/un2.un.org/files/2020/04/covid19_response_icon.svg" width="200" heigth="200">

## Description

This project aims to visualize mental disorders situations
before/during/after Covid-19

## Usage

Simply run 

	nix-build -E 'with import <nixpkgs> {}; callPackage ./default.nix {}'

This will create a `result` directory with the pdf produced.

## Development

Use shell.nix for get all dependencies needed for run scripts

	nix-shell

This will drop you in a sort-of python virtualenv, but for both R and
Python. It also includes rstudio for easily run the R script against
2the RDA.

## License

This project is licensed under the GPL3 License - see the
[LICENSE](LICENSE.md) file for details
