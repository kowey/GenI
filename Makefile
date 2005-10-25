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
#GHCFLAGS += -framework AppKit -framework Cocoa -framework Carbon -framework IOKit 
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
EIFILE = $(SRC_GENI)/ExtractTestCases
DIFILE = $(SRC_GENI)/MainNoGui

OFILE = bin/geni
COFILE = bin/geniconvert
EOFILE = bin/geniExtractCases

ifeq ($(OS),Darwin)
OS_SPECIFIC_STUFF = cd bin; ../etc/macstuff/macosx-app geni 
endif


LEXERS		= $(SRC_GENI)/Lex2.hs
PARSERS		= $(SRC_GENI)/Mparser.hs 

PARSERS_OUT  := $(patsubst %.hs,%.o,$(PARSERS))
SOURCE_FILES := $(wildcard $(SRC_GENI)/*.lhs)\
		$(wildcard $(SRC_GENI)/*.hs)

# Phony targets do not keep track of file modification times
.PHONY: all nogui dep clean docs html release optimize\
       	permissions install\
	extractor

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

permissions:
	chmod u+x $(SCRIPT_FILES)
# --------------------------------------------------------------------
# compilation 
# --------------------------------------------------------------------

$(LEXERS): %.hs: %.x
	alex -g $< 

$(PARSERS): %.hs: %.y
	happy -a -g -c -m `basename $@ .hs` $< 

compile: permissions $(OFILE) $(EOFILE)

extractor: $(EOFILE)

$(OFILE) : $(LEXERS) $(PARSERS) $(SOURCE_FILES)
	#$(GHC) -c --make $(GHCPACKAGES_GUI) $(LEXERS) $(PARSERS) 
	$(GHC) -W --make $(GHCPACKAGES_GUI) $(IFILE).lhs -o $(OFILE)
	$(OS_SPECIFIC_STUFF)

$(COFILE) : $(LEXERS) $(PARSERS) $(SOURCE_FILES)
	#$(GHC) -c --make $(GHCPACKAGES_GUI) $(LEXERS) $(PARSERS) 
	$(GHC) -W --make $(GHCPACKAGES) $(CIFILE).lhs -o $(COFILE) 
	$(OS_SPECIFIC_STUFF)

$(EOFILE) : $(SOURCE_FILES)
	$(GHC) -W --make $(GHCPACKAGES) $(EIFILE).lhs -o $(EOFILE) 

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
