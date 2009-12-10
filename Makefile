DOC_DIRS := doc

# --------------------------------------------------------------------
# genidoc options
# --------------------------------------------------------------------

# Add here any files that you want to compile.  For example:
#   MAKE_DOCS=foo/bar.pdf foo/other.pdf baz/filename.pdf
# If you are making slides instead of documents, you should
# uncomment and modify the MAKE_SLIDES variable.
MAKE_DOCS = doc/literateGenI.pdf doc/genimanual.pdf

# -- Latex or Pdflatex? (pdflatex by default) --
# If you use latex instead of pdflatex, you should change the line
# below as well as uncommenting DVIPDF
LATEX=pdflatex
# LATEX=latex

# -- dvipdf --     (currently off because we use pdflatex)
# If you use latex instead of pdflatex, you should uncomment
# DVIPDF
DVIPDF_CMD=dvips `basename $< .tex`.dvi -o `basename $< .tex`.ps;\
	ps2pdf `basename $< .tex`.ps
#DVIPDF=$(DVIPDF_CMD)

# --------------------------------------------------------------------
# source stuff
# --------------------------------------------------------------------

SOURCE_FILES=$(shell find src -name '*.*hs' -o -name '*.lhs')

SCRIPT_FILES = bin/tryXtimes\
	       etc/stupidmorph.pl\
	       etc/tommorph.pl\
	       etc/perf-latest.sh\
	       etc/perf-save.sh\

DOC_DIR = doc

# Phony targets do not keep track of file modification times
.PHONY: docs doc maindoc html release\

# --------------------------------------------------------------------
# main targets
# --------------------------------------------------------------------

all: build doc

build: dist/build/geni/geni

dist/build/geni/geni: $(SOURCE_FILES)
	cabal build

doc:  init maindoc

maindoc: $(SOURCE_FILES) $(MAKE_DOCS)

docs: doc

clean: tidy
	rm -f $(foreach d, $(DOC_DIRS), $(d)/*.{ps,pdf})

tidy:
	rm -f $(foreach d, $(DOC_DIRS), $(d)/*.{dvi,aux,log,bbl,blg,out,toc})

init: permissions

permissions: $(config_file)
	chmod u+x $(SCRIPT_FILES)

tags: $(SOURCE_FILES)
	hasktags -c $(shell find src -name '*.*hs')
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

.PHONY: depgraph

DOC_SRC=$(SOURCE_FILES) doc/realisation.tex

$(MAKE_DOCS): %.pdf: %.tex $(DOC_SRC) doc/images/genidep.pdf
	cd $(<D) &&\
	$(LATEX) $(<F) && bibtex $(<F:.tex=) &&\
       	$(LATEX) $(<F) && $(LATEX) $(<F)\
	$(DVIPDF)

doc/images/genidep.pdf :
	graphmod -i src src/MainGeni.lhs | tred | sed -e 's/style=filled/style=solid/' | dot -Tpdf > $@

publish:
	cabal haddock
	scp $(MAKE_DOCS) kowey@code.haskell.org:/srv/projects/GenI
	rsync -av dist/doc/html/GenI/ kowey@code.haskell.org:/srv/projects/GenI/api-doc
