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

include config/config.mk

# i know, i know, i should be using autotools... 
# but *whine* it's so hard!
OS:=$(shell uname)

SRC_GENI 	= ./src/geni
GHCFLAGS        = $(LDFLAGS) -cpp -fglasgow-exts -threaded -O
ifeq ($(OS),Darwin)
GHCFLAGS += -framework AppKit 
endif

#-O
GHCINCLUDE      = -i$(SRC_GENI)
#:$(HXMLDIR)/hparser:$(HXMLDIR)/hdom
GHCPACKAGES     = 
#-package HaXml
GHCPACKAGES_GUI = -package wx $(GHCPACKAGES) 
GHC             = ghc $(GHCFLAGS) $(GHCINCLUDE)
GHC_PROF	= $(GHC) -prof -auto-all -hisuf p_hi -osuf p_o 

SOFTWARE        = Geni 
SOFTVERS        = $(SOFTWARE)-$(VERSION)

TO_INSTALL=geni xmgGeni runXMGselector.sh runXMGfilter.pl

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
BIBTEX_CMD=$(LATEX) `basename $<` &&\
	   bibtex `basename $< .tex`;
BIBTEX=$(BIBTEX_CMD)


# --------------------------------------------------------------------
# source stuff 
# --------------------------------------------------------------------

SCRIPT_FILES = bin/runXMGselector.sh\
	       bin/runXMGfilter.pl\
	       etc/stupidmorph.pl\
	       etc/tommorph.pl\
	       etc/quickcheck.py\
	       etc/perf-latest.sh\
	       etc/perf-save.sh\


IFILE = $(SRC_GENI)/Main
CIFILE = $(SRC_GENI)/Converter
DIFILE = $(SRC_GENI)/MainNoGui

OFILE = bin/geni
COFILE = bin/geniconvert

ifeq ($(OS),Darwin)
OS_SPECIFIC_STUFF = macosx-app $(OFILE)
endif


LEXERS		= $(SRC_GENI)/Lex2.hs
PARSERS		= $(SRC_GENI)/Mparser.hs 

PARSERS_OUT  := $(patsubst %.hs,%.o,$(PARSERS))

# Phony targets do not keep track of file modification times
.PHONY: all nogui dep clean docs html parsers release optimize\
       	permissions install\
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
	rm -f $(SRC_GENI)/*.{p_hi,p_o,hi,o}

dep: 
	$(GHC) -M $(GHCPACKAGES_GUI) $(IFILE).lhs

permissions:
	chmod u+x $(SCRIPT_FILES)
# --------------------------------------------------------------------
# compilation 
# --------------------------------------------------------------------

$(LEXERS): %.hs: %.x
	alex -g $< 

$(PARSERS): %.hs: %.y
	happy -a -g -c -m `basename $@ .hs` $< 

compile: $(OFILE) 

$(OFILE) : $(LEXERS) $(PARSERS) permissions
	#$(GHC) -c --make $(GHCPACKAGES_GUI) $(LEXERS) $(PARSERS) 
	$(GHC) -W --make $(GHCPACKAGES_GUI) $(IFILE).lhs -o $(OFILE)
	$(OS_SPECIFIC_STUFF)

$(COFILE) : $(LEXERS) $(PARSERS) permissions 
	#$(GHC) -c --make $(GHCPACKAGES_GUI) $(LEXERS) $(PARSERS) 
	$(GHC) -W --make $(GHCPACKAGES) $(CIFILE).lhs -o $(COFILE) 
	$(OS_SPECIFIC_STUFF)

nogui : $(LEXERS) $(PARSERS) permissions 
	#$(GHC) -c --make $(GHCPACKAGES_GUI) $(LEXERS) $(PARSERS) 
	$(GHC) -W --make $(GHCPACKAGES) $(DIFILE).lhs -o $(OFILE) 
	$(OS_SPECIFIC_STUFF)

debugger: $(LEXERS) $(PARSERS) permissions
	$(GHC_PROF) --make $(DIFILE).lhs -o debugger-$(OFILE)
#	$(GHC) -O -prof -auto-all --make $(CIFILE).lhs -o debugger-$(COFILE)

# --------------------------------------------------------------------
# installing 
# --------------------------------------------------------------------

# FIXME handling of macosx stuff very ugly?

install:	
	$(foreach file,$(TO_INSTALL), $(INSTALL) bin/$(file) $(BINDIR) &&)\
	:
ifeq ($(OS),Darwin)
	$(CP) -R bin/geni.app $(BINDIR)
endif

uninstall:
	$(foreach file,$(TO_INSTALL), $(RM) $(BINDIR)/$(file) &&)\
	:
ifeq ($(OS),Darwin)
	$(RM) -R $(BINDIR)/geni.app 
endif

# --------------------------------------------------------------------
# documentation 
# --------------------------------------------------------------------

DOC_SRC=$(SRC_GENI)/*.lhs

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
./src/geni/Graphviz.o : ./src/geni/Graphviz.lhs
./src/geni/SysGeni.o : ./src/geni/SysGeni.lhs
./src/geni/General.o : ./src/geni/General.lhs
./src/geni/ParserLib.o : ./src/geni/ParserLib.hs
./src/geni/Lex2.o : ./src/geni/Lex2.hs
./src/geni/Lex2.o : ./src/geni/ParserLib.hi
./src/geni/Btypes.o : ./src/geni/Btypes.lhs
./src/geni/Lparser.o : ./src/geni/Lparser.hs
./src/geni/Lparser.o : ./src/geni/Btypes.hi
./src/geni/Lparser.o : ./src/geni/ParserLib.hi
./src/geni/Mparser.o : ./src/geni/Mparser.hs
./src/geni/Mparser.o : ./src/geni/Btypes.hi
./src/geni/Mparser.o : ./src/geni/ParserLib.hi
./src/geni/Cparser.o : ./src/geni/Cparser.hs
./src/geni/Cparser.o : ./src/geni/ParserLib.hi
./src/geni/Cparser.o : ./src/geni/Btypes.hi
./src/geni/Bfuncs.o : ./src/geni/Bfuncs.lhs
./src/geni/Bfuncs.o : ./src/geni/General.hi
./src/geni/Bfuncs.o : ./src/geni/Btypes.hi
./src/geni/Tags.o : ./src/geni/Tags.lhs
./src/geni/Tags.o : ./src/geni/General.hi
./src/geni/Tags.o : ./src/geni/Bfuncs.hi
./src/geni/Polarity.o : ./src/geni/Polarity.lhs
./src/geni/Polarity.o : ./src/geni/General.hi
./src/geni/Polarity.o : ./src/geni/Bfuncs.hi
./src/geni/Polarity.o : ./src/geni/Tags.hi
./src/geni/Polarity.o : ./src/geni/Graphviz.hi
./src/geni/Morphology.o : ./src/geni/Morphology.lhs
./src/geni/Morphology.o : ./src/geni/Tags.hi
./src/geni/Morphology.o : ./src/geni/General.hi
./src/geni/Morphology.o : ./src/geni/Bfuncs.hi
./src/geni/Treeprint.o : ./src/geni/Treeprint.lhs
./src/geni/Treeprint.o : ./src/geni/Graphviz.hi
./src/geni/Treeprint.o : ./src/geni/Bfuncs.hi
./src/geni/Treeprint.o : ./src/geni/Tags.hi
./src/geni/Tsparser.o : ./src/geni/Tsparser.hs
./src/geni/Tsparser.o : ./src/geni/Btypes.hi
./src/geni/Tsparser.o : ./src/geni/ParserLib.hi
./src/geni/GeniParsers.o : ./src/geni/GeniParsers.hs
./src/geni/GeniParsers.o : ./src/geni/Tsparser.hi
./src/geni/GeniParsers.o : ./src/geni/Lparser.hi
./src/geni/GeniParsers.o : ./src/geni/Mparser.hi
./src/geni/GeniParsers.o : ./src/geni/Cparser.hi
./src/geni/GeniParsers.o : ./src/geni/ParserLib.hi
./src/geni/GeniParsers.o : ./src/geni/Lex2.hi
./src/geni/GeniParsers.o : ./src/geni/Mparser.hi
./src/geni/Configuration.o : ./src/geni/Configuration.lhs
./src/geni/Configuration.o : ./src/geni/GeniParsers.hi
./src/geni/Mstate.o : ./src/geni/Mstate.lhs
./src/geni/Mstate.o : ./src/geni/General.hi
./src/geni/Mstate.o : ./src/geni/Configuration.hi
./src/geni/Mstate.o : ./src/geni/Tags.hi
./src/geni/Mstate.o : ./src/geni/Bfuncs.hi
./src/geni/Geni.o : ./src/geni/Geni.lhs
./src/geni/Geni.o : ./src/geni/SysGeni.hi
./src/geni/Geni.o : ./src/geni/General.hi
./src/geni/Geni.o : ./src/geni/Btypes.hi
./src/geni/Geni.o : ./src/geni/GeniParsers.hi
./src/geni/Geni.o : ./src/geni/Polarity.hi
./src/geni/Geni.o : ./src/geni/Morphology.hi
./src/geni/Geni.o : ./src/geni/Mstate.hi
./src/geni/Geni.o : ./src/geni/Configuration.hi
./src/geni/Geni.o : ./src/geni/Tags.hi
./src/geni/Geni.o : ./src/geni/Bfuncs.hi
./src/geni/Geni.o : ./src/geni/General.hi
./src/geni/Console.o : ./src/geni/Console.lhs
./src/geni/Console.o : ./src/geni/Configuration.hi
./src/geni/Console.o : ./src/geni/Mstate.hi
./src/geni/Console.o : ./src/geni/Geni.hi
./src/geni/Console.o : ./src/geni/General.hi
./src/geni/Console.o : ./src/geni/Bfuncs.hi
./src/geni/Gui.o : ./src/geni/Gui.lhs
./src/geni/Gui.o : ./src/geni/Polarity.hi
./src/geni/Gui.o : ./src/geni/Mstate.hi
./src/geni/Gui.o : ./src/geni/GeniParsers.hi
./src/geni/Gui.o : ./src/geni/Configuration.hi
./src/geni/Gui.o : ./src/geni/Tags.hi
./src/geni/Gui.o : ./src/geni/Bfuncs.hi
./src/geni/Gui.o : ./src/geni/General.hi
./src/geni/Gui.o : ./src/geni/Geni.hi
./src/geni/Gui.o : ./src/geni/Morphology.hi
./src/geni/Gui.o : ./src/geni/Tags.hi
./src/geni/Gui.o : ./src/geni/Treeprint.hi
./src/geni/Gui.o : ./src/geni/Graphviz.hi
./src/geni/Main.o : ./src/geni/Main.lhs
./src/geni/Main.o : ./src/geni/Configuration.hi
./src/geni/Main.o : ./src/geni/Console.hi
./src/geni/Main.o : ./src/geni/Gui.hi
./src/geni/Main.o : ./src/geni/Geni.hi
# DO NOT DELETE: End of Haskell dependencies
