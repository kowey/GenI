#!/bin/bash
# Set up documentation infrastructure

set -ev
PAGES_REPO=git@github.com:kowey/GenI
WIKI_REPO=git@github.com:kowey/GenI.wiki

if [ ! -e wiki ]; then
    git clone $WIKI_REPO wiki
fi

mkdir -p _site
cd _site
git init
git fetch $PAGES_REPO gh-pages
git checkout FETCH_HEAD
git branch | grep gh-pages
if [ $? -ne 0 ]; then
    git checkout -b gh-pages
    git remote add github $PAGES_REPO
fi
git checkout gh-pages
