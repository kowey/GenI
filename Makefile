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

# include this file if configure had been run
# see the $(config_file) target to make it complain
config_file:=config/config.mk
ifneq '$(wildcard $(config_file))' ''
include $(config_file)
endif

# i know, i know, i should be using autotools...
# but *whine* it's so hard!
OS:=$(shell uname)

SRC_GENI 	= ./src/geni

GHC             = ghc
#-O
GHCINCLUDE      = -i$(SRC_GENI) -i$(SRC_GENI)/simple -i$(SRC_GENI)/cky
#:$(HXMLDIR)/hparser:$(HXMLDIR)/hdom
GHCPACKAGES     =
#-package HaXml
GHCPACKAGES_GUI = -package wx $(GHCPACKAGES)

GHCFLAGS        = $(LDFLAGS) -W -cpp -fglasgow-exts -threaded -O $(GHCINCLUDE)
GHCFLAGS_PROF   = $(GHCFLAGS) -prof -hisuf p_hi -osuf p_o -DDISABLE_GUI

SOFTWARE        = Geni
SOFTVERS        = $(SOFTWARE)-$(VERSION)

TO_INSTALL=geni xmgGeni runXMGselector runXMGfilter

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


# --------------------------------------------------------------------
# source stuff
# --------------------------------------------------------------------

SCRIPT_FILES = bin/runXMGselector\
	       bin/runXMGfilter\
	       bin/xmgGeni\
	       etc/lhs2haddock\
	       etc/stupidmorph.pl\
	       etc/tommorph.pl\
	       etc/quickcheck.py\
	       etc/perf-latest.sh\
	       etc/perf-save.sh\
	       etc/macstuff/macosx-app\


IFILE = $(SRC_GENI)/Main
CIFILE = $(SRC_GENI)/Converter
EIFILE = $(SRC_GENI)/ExtractTestCases

OFILE = bin/geni
COFILE = bin/geniconvert
EOFILE = bin/geniExtractCases

DOC_DIR = doc
HADDOCK_OUT = $(DOC_DIR)/api

ifeq ($(OS),Darwin)
OS_SPECIFIC_STUFF = cd bin; ../etc/macstuff/macosx-app geni
endif


SOURCE_FILES_1 := $(wildcard $(SRC_GENI)/*.lhs $(SRC_GENI/*.hs) $(SRC_GENI)/simple/*.lhs $(SRC_GENI)/cky/*.lhs)
SOURCE_FILES_2 := $(wildcard $(SRC_GENI)/*.hs)
SOURCE_HSPP_1 := $(patsubst %.lhs,%.hspp,$(SOURCE_FILES_1))
SOURCE_HSPP_2 := $(SOURCE_FILES_2)

SOURCE_FILES := $(SOURCE_FILES_1) $(SOURCE_FILES_2)
SOURCE_HSPP  := $(SOURCE_HSPP_1) $(SOURCE_HSPP_2)

# Phony targets do not keep track of file modification times
.PHONY: all nogui dep clean docs html release optimize\
       	permissions install\
	extractor\
	tags haddock

# --------------------------------------------------------------------
# main targets
# --------------------------------------------------------------------

normal: compile
all: compile docs tidy
release: compile docs html tidy tarball

doc:  permissions $(MAKE_DOCS) haddock
	cp $(MAKE_DOCS) $(DOC_DIR)

docs: doc
html: $(MAKE_HTML)

tarball:
	sed -e "s/^Geni/$(MYDIR)/" etc/geniexclude > /tmp/geniexclude
	rm -f $(MYDIR)*.tar.gz;\
	cd .. ;\
	tar --exclude-from /tmp/geniexclude -czf $(MYDIR)_$(DATE).tar.gz $(MYDIR);\
	mv $(MYDIR)_$(DATE).tar.gz $(MYDIR)

clean: tidy
	rm -f $(SRC_GENI)/*.{ps,pdf}
	rm -f $(MAKE_HTML)
	rm -rf $(OFILE) $(OFILE).app
	rm -rf $(SOURCE_HSPP_1) $(HADDOCK_OUT)

tidy:
	rm -f $(SRC_GENI)/*.{dvi,aux,log,bbl,blg,out,toc}
	rm -f $(SRC_GENI)/*.{p_hi,p_o,hi,o}

permissions: $(config_file)
	chmod u+x $(SCRIPT_FILES)

$(config_file):
	chmod u+x configure
	@echo >&2 "Please run ./configure to setup the build system."
	@exit 1

tags:
	hasktags -c $(SOURCE_FILES)
	sort $@ > $@.tmp
	mv $@.tmp $@

# --------------------------------------------------------------------
# compilation
# --------------------------------------------------------------------

compile: permissions $(OFILE) $(EOFILE)

extractor: $(EOFILE)

$(OFILE) : $(SOURCE_FILES)
	$(GHC) $(GHCFLAGS) --make $(GHCPACKAGES_GUI) $(IFILE).lhs -o $(OFILE)
	$(OS_SPECIFIC_STUFF)

$(COFILE) : $(CIFILE).lhs $(SOURCE_FILES)
	$(GHC) $(GHCFLAGS) --make $(GHCPACKAGES) $< -o $@
	$(OS_SPECIFIC_STUFF)

$(EOFILE) : $(EIFILE).lhs $(SOURCE_FILES)
	$(GHC) $(GHCFLAGS) --make $(GHCPACKAGES) $< -o $@

nogui : permissions
	$(GHC) $(GHCFLAGS) --make -DDISABLE_GUI $(GHCPACKAGES) $(IFILE).lhs -o $(OFILE)
	$(OS_SPECIFIC_STUFF)

debugger: bin/debugger-geni

bin/debugger-geni: $(SRC_GENI)/Main.lhs permissions
	$(GHC) $(GHCFLAGS_PROF) $(GHCPACKAGES) --make $< -o $@

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

DOC_SRC=$(SOURCE_FILES)

haddock: $(SOURCE_HSPP)
	mkdir -p $(HADDOCK_OUT)
	haddock -h $(SOURCE_HSPP) -o $(HADDOCK_OUT)

$(SOURCE_HSPP_1): %.hspp: %.lhs
	etc/lhs2haddock < $< > $@

$(MAKE_DOCS): %.pdf: %.tex $(DOC_SRC)
	cd $(<D) &&\
	$(LATEX) $(<F) && bibtex $(<F:.tex=) &&\
       	$(LATEX) $(<F) && $(LATEX) $(<F)\
	$(DVIPDF)
