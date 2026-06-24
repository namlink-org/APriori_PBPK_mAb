#!/usr/bin/env bash
set -euo pipefail

export PATH="/x86_64-w64-mingw32.static.posix/bin:/usr/bin:/c/Program Files/R/R-4.5.1/bin/x64:${PATH:-}"
export BINPREF="/x86_64-w64-mingw32.static.posix/bin/"
export COMPILER_PATH="/x86_64-w64-mingw32.static.posix/bin/"

cd "$(dirname "$0")"

Rscript -e "rfile <- tempfile(fileext = '.R'); knitr::purl('TopDownTMDD_Monkey.qmd', output = rfile, quiet = TRUE); source(rfile, echo = FALSE)"
