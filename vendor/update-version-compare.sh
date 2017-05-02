#!/bin/bash

set -e -o pipefail

cd ../src
rm -f version_compare.{ml,mli}

for ext in ml mli; do
    curl -o version_compare.$ext \
         https://raw.githubusercontent.com/ocaml/opam/master/src/core/opamVersionCompare.$ext
done
