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

SOURCE_FILES=$(shell find src -name '*.*hs')

SCRIPT_FILES = bin/tryXtimes\
	       etc/stupidmorph.pl\
	       etc/tommorph.pl\
	       etc/quickcheck.py\
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

maindoc: $(MAKE_DOCS)

docs: doc
html: $(MAKE_HTML)

clean: tidy
	rm -f $(foreach d, $(DOC_DIRS), $(d)/*.{ps,pdf})
	rm -f $(MAKE_HTML)

tidy:
	rm -f $(foreach d, $(DOC_DIRS), $(d)/*.{dvi,aux,log,bbl,blg,out,toc})

init: permissions

permissions: $(config_file)
	chmod u+x $(SCRIPT_FILES)

tags:
	hasktags -c $(shell find src -name '*.*hs')
	sort $@ > $@.tmp
	mv $@.tmp $@

# --------------------------------------------------------------------
# testing
# --------------------------------------------------------------------

test: unit regression

regression: etc/SumHUnit
	@chmod u+x etc/regression
	etc/regression

unit:
	etc/quickcheck.py libGenI/NLP/GenI/Btypes.lhs | ghci $(GHCI_FLAGS)

etc/SumHUnit : etc/SumHUnit.hs
	ghc --make -o $@ $<

# --------------------------------------------------------------------
# documentation
# --------------------------------------------------------------------

DOC_SRC=$(SOURCE_FILES)

$(MAKE_DOCS): %.pdf: %.tex $(DOC_SRC)
	cd $(<D) &&\
	$(LATEX) $(<F) && bibtex $(<F:.tex=) &&\
       	$(LATEX) $(<F) && $(LATEX) $(<F)\
	$(DVIPDF)
