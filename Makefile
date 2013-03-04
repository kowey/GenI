# --------------------------------------------------------------------
# source stuff
# --------------------------------------------------------------------

SOURCE_FILES=$(shell find src geni-gui -name '*.*hs' -o -name '*.lhs')

SCRIPT_FILES = bin/tryXtimes\
	       etc/stupidmorph.pl\
	       etc/tommorph.pl\
	       etc/perf-latest.sh\
	       etc/perf-save.sh\

# Phony targets do not keep track of file modification times
.PHONY: docs doc maindoc html release\

# --------------------------------------------------------------------
# main targets
# --------------------------------------------------------------------

all: build

build: dist/build/geni/geni

dist/build/geni/geni: $(SOURCE_FILES)
	cabal build

doc:  init maindoc

init: permissions

permissions: $(config_file)
	chmod u+x $(SCRIPT_FILES)

tags: $(SOURCE_FILES)
	hasktags -c $(SOURCE_FILES)
	LC_ALL=C sort $@ > $@.tmp
	mv $@.tmp $@

# --------------------------------------------------------------------
# testing
# --------------------------------------------------------------------

test: unit regression

regression: etc/SumHUnit
	@chmod u+x etc/regression
	etc/regression

unit: dist/build/geni/geni
	dist/build/geni/geni --unit

etc/SumHUnit : etc/SumHUnit.hs
	ghc --make -o $@ $<

# --------------------------------------------------------------------
# documentation
# --------------------------------------------------------------------

publish:
	cabal haddock
	rsync -av dist/doc/html/GenI/ doc/_site/api-doc
	cd doc; ../cabal-dev/bin/geni-doc build
	@echo "REMEMBER TO DO: cd doc/_site; git push"
