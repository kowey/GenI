#-o vim: set noexpandtab:

# Useful commands
# - make         compiles your project 
# - make doc     compiles your documentation 
# - make tidy    removes all intermediary tex files (like *.aux)
# - make clean   removes all compiled files (like *.pdf, *.aux)
# - make release creates a tarball that you can give to others

# --------------------------------------------------------------------
# configuration 
# --------------------------------------------------------------------

SRC_GENI 	= ./src/geni
GHCFLAGS        = 
#-O
GHCINCLUDE      = -i$(SRC_GENI)
#:$(HXMLDIR)/hparser:$(HXMLDIR)/hdom
GHCPACKAGES     = -package wx -package HaXml
GHC             = ghc $(GHCFLAGS) $(GHCINCLUDE)

SOFTWARE        = Geni 
VERSION         = 0.5
VERSIONTAG      = geni-0-50  # use: make tagVersion for creating cvs tag
SOFTVERS        = $(SOFTWARE)-$(VERSION)

# You should replace the value of this variable with your project
# directory name.  The default assumption is that the project name
# is the same as the directory name.
MYDIR = Geni
DATE:=$(shell date +%Y-%m-%d)

# Add here any files that you want to compile.  For example:
#   MAKE_DOCS=foo/bar.pdf foo/other.pdf baz/filename.pdf
# If you are making slides instead of documents, you should
# uncomment and modify the MAKE_SLIDES variable.
MAKE_DOCS = src/geni/genidoc.pdf

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

# -- bibtex --     (currently on)
# If you use BibTeX you should uncomment 
BIBTEX_CMD=$(LATEX) `basename $<`;\
	   bibtex `basename $< .tex`;
#BIBTEX=$(BIBTEX_CMD)

# -- latex2html -- (currently off)
LATEX2HTML = latex2html -math -math_parsing -local_icons -noimages -split 5
# If you want to use latex2html you should uncomment this line
# and add here any tex files you want compiled to html; for 
# example, if you have foo.tex, you should add foo-html
#MAKE_HTML = Geni-html

# --------------------------------------------------------------------
# source stuff 
# --------------------------------------------------------------------

IFILE = $(SRC_GENI)/Main
DIFILE = $(SRC_GENI)/MainNoGui

OFILE = geni
DOFILE = debugger

LEXERS		= $(SRC_GENI)/Lex2.hs
PARSERS		= \
 $(SRC_GENI)/Mparser.hs \
 $(SRC_GENI)/Cparser.hs \
 $(SRC_GENI)/GIparser.hs \
 $(SRC_GENI)/Lparser.hs \
 $(SRC_GENI)/Tsparser.hs \
 $(SRC_GENI)/PolParser.hs 

# Phony targets do not keep track of file modification times
.PHONY: all clean docs html parsers release optimize

# --------------------------------------------------------------------
# main targets 
# --------------------------------------------------------------------

normal: compile
all: compile docs tidy
release: compile docs html tidy tarball

doc:  $(MAKE_DOCS)
docs: $(MAKE_DOCS) 
html: $(MAKE_HTML)

tarball:
	rm -f $(MYDIR)*.tar.gz;\
	cd .. ;\
	tar -czvf $(MYDIR)_$(DATE).tar.gz $(MYDIR);\
	mv $(MYDIR)_$(DATE).tar.gz $(MYDIR)

clean: tidy
	rm -f $(LEXERS) $(PARSERS)
	rm -f $(SRC_GENI)/*.{ps,pdf}
	rm -f $(MAKE_HTML)
	rm -rf $(OFILE) $(OFILE).app

tidy:
	rm -f $(SRC_GENI)/*.{dvi,aux,log,bbl,blg,out,toc}
	rm -f $(SRC_GENI)/*.{hi,o}

# --------------------------------------------------------------------
# compilation 
# --------------------------------------------------------------------

$(LEXERS): %.hs: %.x
	alex -g $< 

$(PARSERS): %.hs: %.y
	happy -m `basename $@ .hs` $< 

optimize: $(LEXERS) $(PARSERS)
	$(GHC) -O2 --make $(GHCPACKAGES) $(LEXERS) $(PARSERS) 
	$(GHC) -O2 -W --make $(GHCPACKAGES) $(IFILE).hs -o $(OFILE) 
	if [ `uname` = Darwin ]; then macosx-app $(OFILE); fi

compile: $(LEXERS) $(PARSERS)
	$(GHC) --make $(GHCPACKAGES) $(LEXERS) $(PARSERS) 
	$(GHC) -W --make $(GHCPACKAGES) $(IFILE).hs -o $(OFILE) 
	if [ `uname` = Darwin ]; then macosx-app $(OFILE); fi

debugger: $(LEXERS) $(PARSERS)
	$(GHC) -prof --make $(GHCPACKAGES) $(LEXERS) $(PARSERS) 
	$(GHC) -prof -auto-all --make $(GHCPACKAGES) $(DIFILE).hs -o $(DOFILE)
	if [ `uname` = Darwin ]; then macosx-app $(DOFILE); fi

# --------------------------------------------------------------------
# documentation 
# --------------------------------------------------------------------

DOC_SRC=$(SRC_GENI)/Mstate.lhs $(SRC_GENI)/Geni.lhs $(SRC_GENI)/Polarity.lhs

$(MAKE_DOCS): %.pdf: %.tex $(DOC_SRC)
	cd `dirname $<`;\
	$(BIBTEX)\
	$(LATEX) `basename $<`;\
	$(LATEX) `basename $<`;\
	$(DVIPDF)

$(MAKE_HTML): %-html: %.tex
	rm -rf $@
	$(LATEX2HTML)  $<
	mv `basename $< .tex` $@
