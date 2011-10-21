#!/bin/bash
set -e
cd `dirname $0`
ghc --make site
./site rebuild
rsync -av --no-t --no-p --checksum _site/ kowey@code.haskell.org:/srv/projects/GenI
