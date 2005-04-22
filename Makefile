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
GHCFLAGS        = -fglasgow-exts 
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
CIFILE = $(SRC_GENI)/Converter
DIFILE = $(SRC_GENI)/MainNoGui

OFILE = geni
COFILE = geniconvert
DOFILE = debugger

LEXERS		= $(SRC_GENI)/Lex2.hs
PARSERS		= \
 $(SRC_GENI)/Mparser.hs \
 $(SRC_GENI)/Cparser.hs \
 $(SRC_GENI)/Lparser.hs \
 $(SRC_GENI)/Tsparser.hs 

# Phony targets do not keep track of file modification times
.PHONY: all dep clean docs html parsers release optimize\
       	$(OFILE) $(COFILE)

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
	sed -e "s/^Geni/$(MYDIR)/" etc/geniexclude > /tmp/geniexclude
	rm -f $(MYDIR)*.tar.gz;\
	cd .. ;\
	tar --exclude-from /tmp/geniexclude -czf $(MYDIR)_$(DATE).tar.gz $(MYDIR);\
	mv $(MYDIR)_$(DATE).tar.gz $(MYDIR)

clean: tidy
	rm -f $(LEXERS) $(PARSERS)
	rm -f $(SRC_GENI)/*.{ps,pdf}
	rm -f $(MAKE_HTML)
	rm -rf $(OFILE) $(OFILE).app

tidy:
	rm -f $(SRC_GENI)/*.{dvi,aux,log,bbl,blg,out,toc}
	rm -f $(SRC_GENI)/*.{hi,o}

dep: 
	$(GHC) -M $(GHCPACKAGES) $(SRC_GENI)/*.hs $(SRC_GENI)/*.lhs

# --------------------------------------------------------------------
# compilation 
# --------------------------------------------------------------------

$(LEXERS): %.hs: %.x
	alex -g $< 

$(PARSERS): %.hs: %.y
	happy -a -g -c -m `basename $@ .hs` $< 

optimize: $(LEXERS) $(PARSERS)
	$(GHC) -O2 --make $(GHCPACKAGES) $(LEXERS) $(PARSERS) 
	$(GHC) -O2 -W --make $(GHCPACKAGES) $(IFILE).hs -o $(OFILE) 
	if [ `uname` = Darwin ]; then macosx-app $(OFILE); fi

compile: $(LEXERS) $(PARSERS) $(OFILE) $(COFILE)

$(OFILE) : $(LEXERS) $(PARSERS) 
	$(GHC) --make $(GHCPACKAGES) $(LEXERS) $(PARSERS) 
	$(GHC) -W --make $(GHCPACKAGES) $(IFILE).hs -o $(OFILE) 
	if [ `uname` = Darwin ]; then macosx-app $(OFILE); fi

$(COFILE) : $(LEXERS) $(PARSERS) 
	$(GHC) -O2 --make $(GHCPACKAGES) $(LEXERS) $(PARSERS) 
	$(GHC) -O2 -W --make $(GHCPACKAGES) $(CIFILE).lhs -o $(COFILE) 

debugger: $(LEXERS) $(PARSERS)
	$(GHC) -O2 -prof --make $(LEXERS) $(PARSERS) 
	$(GHC) -O2 -prof -auto-all --make $(DIFILE).hs -o $(DOFILE)
	if [ `uname` = Darwin ]; then macosx-app $(DOFILE); fi
	

# --------------------------------------------------------------------
# documentation 
# --------------------------------------------------------------------

DOC_SRC=$(SRC_GENI)/Mstate.lhs $(SRC_GENI)/Geni.lhs $(SRC_GENI)/Polarity.lhs

$(MAKE_DOCS): %.pdf: %.tex $(DOC_SRC)
	cd `dirname $<` &&\
	$(BIBTEX)\
	$(LATEX) `basename $<` &&\
	$(LATEX) `basename $<` ;\ 
	$(DVIPDF)

$(MAKE_HTML): %-html: %.tex
	rm -rf $@
	$(LATEX2HTML)  $<
	mv `basename $< .tex` $@

# --------------------------------------------------------------------
# dependencies (autogenerated by ghc)
# --------------------------------------------------------------------

# DO NOT DELETE: Beginning of Haskell dependencies
./src/geni/Cparser.o : ./src/geni/Cparser.hs
./src/geni/Cparser.o : ./src/geni/ParserLib.hi
./src/geni/Cparser.o : ./src/geni/Btypes.hi
./src/geni/EnableGUI.o : ./src/geni/EnableGUI.hs
./src/geni/EnableGUI.o : ./src/geni/Main.hi
./src/geni/Lex2.o : ./src/geni/Lex2.hs
./src/geni/Lex2.o : ./src/geni/ParserLib.hi
./src/geni/Lparser.o : ./src/geni/Lparser.hs
./src/geni/Lparser.o : ./src/geni/Btypes.hi
./src/geni/Lparser.o : ./src/geni/ParserLib.hi
./src/geni/Main.o : ./src/geni/Main.hs
./src/geni/Main.o : ./src/geni/Configuration.hi
./src/geni/Main.o : ./src/geni/Console.hi
./src/geni/Main.o : ./src/geni/Gui.hi
./src/geni/Main.o : ./src/geni/Geni.hi
./src/geni/MainNoGui.o : ./src/geni/MainNoGui.hs
./src/geni/MainNoGui.o : ./src/geni/Polarity.hi
./src/geni/MainNoGui.o : ./src/geni/Mstate.hi
./src/geni/MainNoGui.o : ./src/geni/Console.hi
./src/geni/MainNoGui.o : ./src/geni/Geni.hi
./src/geni/Mparser.o : ./src/geni/Mparser.hs
./src/geni/Mparser.o : ./src/geni/Btypes.hi
./src/geni/Mparser.o : ./src/geni/ParserLib.hi
./src/geni/ParserLib.o : ./src/geni/ParserLib.hs
./src/geni/PolParser.o : ./src/geni/PolParser.hs
./src/geni/PolParser.o : ./src/geni/ParserLib.hi
./src/geni/Tsparser.o : ./src/geni/Tsparser.hs
./src/geni/Tsparser.o : ./src/geni/Btypes.hi
./src/geni/Tsparser.o : ./src/geni/ParserLib.hi
./src/geni/Btypes.o : ./src/geni/Btypes.lhs
./src/geni/Configuration.o : ./src/geni/Configuration.lhs
./src/geni/Configuration.o : ./src/geni/ParserLib.hi
./src/geni/Configuration.o : ./src/geni/Mparser.hi
./src/geni/Configuration.o : ./src/geni/Cparser.hi
./src/geni/Configuration.o : ./src/geni/Lex2.hi
./src/geni/Console.o : ./src/geni/Console.lhs
./src/geni/Console.o : ./src/geni/Treeprint.hi
./src/geni/Console.o : ./src/geni/Configuration.hi
./src/geni/Console.o : ./src/geni/Mstate.hi
./src/geni/Console.o : ./src/geni/Geni.hi
./src/geni/Console.o : ./src/geni/Btypes.hi
./src/geni/Converter.o : ./src/geni/Converter.lhs
./src/geni/Converter.o : ./src/geni/Treeprint.hi
./src/geni/Converter.o : ./src/geni/GrammarXml.hi
./src/geni/Converter.o : ./src/geni/Btypes.hi
./src/geni/Geni.o : ./src/geni/Geni.lhs
./src/geni/Geni.o : ./src/geni/ParserLib.hi
./src/geni/Geni.o : ./src/geni/Tsparser.hi
./src/geni/Geni.o : ./src/geni/Lparser.hi
./src/geni/Geni.o : ./src/geni/Mparser.hi
./src/geni/Geni.o : ./src/geni/Lex2.hi
./src/geni/Geni.o : ./src/geni/Treeprint.hi
./src/geni/Geni.o : ./src/geni/Polarity.hi
./src/geni/Geni.o : ./src/geni/Mstate.hi
./src/geni/Geni.o : ./src/geni/Configuration.hi
./src/geni/Geni.o : ./src/geni/Tags.hi
./src/geni/Geni.o : ./src/geni/Btypes.hi
./src/geni/GrammarXml.o : ./src/geni/GrammarXml.lhs
./src/geni/GrammarXml.o : ./src/geni/Lex2.hi
./src/geni/GrammarXml.o : ./src/geni/Mparser.hi
./src/geni/GrammarXml.o : ./src/geni/Btypes.hi
./src/geni/Graphviz.o : ./src/geni/Graphviz.lhs
./src/geni/Gui.o : ./src/geni/Gui.lhs
./src/geni/Gui.o : ./src/geni/Polarity.hi
./src/geni/Gui.o : ./src/geni/Mstate.hi
./src/geni/Gui.o : ./src/geni/ParserLib.hi
./src/geni/Gui.o : ./src/geni/Configuration.hi
./src/geni/Gui.o : ./src/geni/Tags.hi
./src/geni/Gui.o : ./src/geni/Btypes.hi
./src/geni/Gui.o : ./src/geni/Geni.hi
./src/geni/Gui.o : ./src/geni/Treeprint.hi
./src/geni/Gui.o : ./src/geni/Graphviz.hi
./src/geni/GuiNew.o : ./src/geni/GuiNew.lhs
./src/geni/GuiNew.o : ./src/geni/Polarity.hi
./src/geni/GuiNew.o : ./src/geni/Mstate.hi
./src/geni/GuiNew.o : ./src/geni/ParserLib.hi
./src/geni/GuiNew.o : ./src/geni/Configuration.hi
./src/geni/GuiNew.o : ./src/geni/Tags.hi
./src/geni/GuiNew.o : ./src/geni/Btypes.hi
./src/geni/GuiNew.o : ./src/geni/Geni.hi
./src/geni/GuiNew.o : ./src/geni/Treeprint.hi
./src/geni/GuiNew.o : ./src/geni/Graphviz.hi
./src/geni/Mstate.o : ./src/geni/Mstate.lhs
./src/geni/Mstate.o : ./src/geni/Configuration.hi
./src/geni/Mstate.o : ./src/geni/Tags.hi
./src/geni/Mstate.o : ./src/geni/Btypes.hi
./src/geni/Polarity.o : ./src/geni/Polarity.lhs
./src/geni/Polarity.o : ./src/geni/Btypes.hi
./src/geni/Polarity.o : ./src/geni/Tags.hi
./src/geni/Polarity.o : ./src/geni/Graphviz.hi
./src/geni/Predictors.o : ./src/geni/Predictors.lhs
./src/geni/Predictors.o : ./src/geni/Polarity.hi
./src/geni/Predictors.o : ./src/geni/Mstate.hi
./src/geni/Predictors.o : ./src/geni/Configuration.hi
./src/geni/Predictors.o : ./src/geni/Tags.hi
./src/geni/Predictors.o : ./src/geni/Btypes.hi
./src/geni/Tags.o : ./src/geni/Tags.lhs
./src/geni/Tags.o : ./src/geni/Btypes.hi
./src/geni/Treeprint.o : ./src/geni/Treeprint.lhs
./src/geni/Treeprint.o : ./src/geni/Graphviz.hi
./src/geni/Treeprint.o : ./src/geni/Btypes.hi
./src/geni/Treeprint.o : ./src/geni/Tags.hi
# DO NOT DELETE: End of Haskell dependencies
