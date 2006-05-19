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

SRC      := ./src
SRC_GENI := ./src/NLP/GenI
SRC_DIRS := $(SRC) $(SRC_GENI) $(SRC_GENI)/CkyEarley $(SRC_GENI)/Simple $(SRC_GENI)/Converter

GHC             = ghc
#-O
GHCINCLUDE      = -i$(SRC)
#:$(HXMLDIR)/hparser:$(HXMLDIR)/hdom
GHCPACKAGES     =
GHCPACKAGES_GUI = -package wx $(GHCPACKAGES)

GHCFLAGS        = $(LDFLAGS) -Wall -cpp -fglasgow-exts -threaded -O $(GHCINCLUDE)
ifdef PROFILE
GHCFLAGS += -prof -hisuf p_hi -osuf p_o -hcsuf p_hc -DDISABLE_GUI -auto
endif

SOFTWARE        = Geni
SOFTVERS        = $(SOFTWARE)-$(VERSION)

TO_INSTALL=$(patsubst %, bin/%, tryXtimes geni xmgGeni runXMGselector runXMGfilter)

# You should replace the value of this variable with your project
# directory name.  The default assumption is that the project name
# is the same as the directory name.
MYDIR = Geni
DATE:=$(shell date +%Y-%m-%d)
DATE2:=$(shell date +%Y-%m-%dT%H%M)

# --------------------------------------------------------------------
# options for profiling
# --------------------------------------------------------------------

ifndef PROFILE_WITH
# -hm # modules
# -hy # types
# -hc # cost centre
# -hd # closure description
# -hr # retainer set
# -hb # biography
PROFILE_WITH := -hc
endif
empty :=
space := $(empty) $(empty)
PROFILE_WITH_SCRUNCHED := $(subst $(space),,$(PROFILE_WITH))

PERFTEST =
PERFTEST += -m etc/perftest/grammar-adjtest.geni
PERFTEST += -l etc/perftest/lemmas.glex
PERFTEST += -s etc/perftest/semantics-adjunctions-t8

ifndef RTS_FLAGS
RTS_FLAGS:=-p $(PROFILE_WITH)
endif

# --------------------------------------------------------------------
# genidoc options
# --------------------------------------------------------------------

# Add here any files that you want to compile.  For example:
#   MAKE_DOCS=foo/bar.pdf foo/other.pdf baz/filename.pdf
# If you are making slides instead of documents, you should
# uncomment and modify the MAKE_SLIDES variable.
MAKE_DOCS = src/NLP/GenI/genidoc.pdf

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
	       bin/tryXtimes\
	       etc/lhs2haddock\
	       etc/stupidmorph.pl\
	       etc/tommorph.pl\
	       etc/quickcheck.py\
	       etc/perf-latest.sh\
	       etc/perf-save.sh\
	       etc/macstuff/macosx-app\


GENI := bin/geni
GENI_MAIN := $(SRC)/MainGeni.lhs
GENI_DEPS = $(call getdeps,$(GENI_MAIN))

EXTRACTOR := bin/geniExtractCases
EXTRACTOR_MAIN := $(SRC)/ExtractTestCases.lhs
EXTRACTOR_DEPS = $(call getdeps,$(EXTRACTOR_MAIN))

CONVERTER := bin/geniconvert
CONVERTER_MAIN := $(SRC)/Converter.hs
CONVERTER_DEPS = $(call getdeps,$(CONVERTER_MAIN))

PROFGENI := bin/debugger-geni

SERVER := bin/geniserver
SERVER_MAIN := $(SRC)/Server.hs
SERVER_DEPS := $(call getdeps,$(CLIENT_MAIN))

CLIENT := bin/geniclient
CLIENT_MAIN := $(SRC)/Client.hs
CLIENT_DEPS := $(call getdeps,$(CLIENT_MAIN))

# dependencies
COMPILE_TARGETS:=$(GENI_MAIN) $(EXTRACTOR_MAIN) $(CONVERTER_MAIN)\
		 $(SERVER_MAIN) $(CLIENT_MAIN)
DEPENDS:=$(patsubst %,.depends/%.dep,$(COMPILE_TARGETS))

# if make deps has been called, use the dependencies file
# otherwise, default to a safer, but more annoying definition
getdeps = $(if $(wildcard .depends/$(1).dep),\
	   $(shell grep -E \.l?hs .depends/$(1).dep | sed -e 's/.* : //' | xargs),\
	   $(SOURCE_FILES))

DOC_DIR = doc
HADDOCK_OUT = $(DOC_DIR)/api

ifeq ($(OS),Darwin)
OS_SPECIFIC_STUFF = cd bin; ../etc/macstuff/macosx-app geni
endif


SOURCE_FILES_1 := $(foreach d, $(SRC_DIRS), $(wildcard $(d)/*.lhs))
SOURCE_FILES_2 := $(foreach d, $(SRC_DIRS), $(wildcard $(d)/*.hs))
SOURCE_HSPP_1 := $(patsubst %.lhs,%.hspp,$(SOURCE_FILES_1))
SOURCE_HSPP_2 := $(SOURCE_FILES_2)

SOURCE_FILES := $(SOURCE_FILES_1) $(SOURCE_FILES_2)
SOURCE_HSPP  := $(SOURCE_HSPP_1) $(SOURCE_HSPP_2)

# Phony targets do not keep track of file modification times
.PHONY: all nogui deps $(DEPENDS)\
	clean docs doc maindoc html release optimize\
       	init permissions install\
	extractor\
	unit profout profiler ghci\
	tags haddock

# --------------------------------------------------------------------
# main targets
# --------------------------------------------------------------------

normal: compile
all: compile docs tidy
release: compile docs html tidy tarball

doc:  init maindoc haddock

maindoc: $(MAKE_DOCS)
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
	rm -f bin/debugger-geni
	rm -f $(foreach d, $(SRC_DIRS), $(d)/*.{ps,pdf})
	rm -f $(MAKE_HTML)
	rm -rf $(GENI) $(GENI).app $(PROFGENI)
	rm -rf $(CONVERTER) $(EXTRACTOR) $(CLIENT) $(SERVER)
	rm -rf $(SOURCE_HSPP_1) $(HADDOCK_OUT)
	rm -rf .depends

tidy:
	rm -f $(foreach d, $(SRC_DIRS), $(d)/*.{dvi,aux,log,bbl,blg,out,toc})
	rm -f $(foreach d, $(SRC_DIRS), $(d)/*.{p_hi,p_o,hi,o})

init: permissions deps

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

ifdef PROFILE
deps:
else
deps: $(DEPENDS)
endif

$(DEPENDS): .depends/%.dep : %
	@echo Calculating dependencies for $<
	@mkdir -p $(dir $@)
	@$(GHC) $(GHCFLAGS) -M -optdep-f -optdep$@ $<

# --------------------------------------------------------------------
# compilation
# --------------------------------------------------------------------

compile: init $(GENI) $(EXTRACTOR) $(CONVERTER) $(SERVER) $(CLIENT)

converter: $(CONVERTER)
extractor: $(EXTRACTOR)
clientserver: $(SERVER) $(CLIENT)

$(GENI) : $(GENI_MAIN) $(GENI_DEPS)
	$(GHC) $(GHCFLAGS) --make $(GHCPACKAGES_GUI) $< -o $@
	$(OS_SPECIFIC_STUFF)

$(CONVERTER): $(CONVERTER_MAIN) $(CONVERTER_DEPS)
	@echo $(CONVERTER_DEPS)
	$(GHC) $(GHCFLAGS) --make $(GHCPACKAGES) -package HaXml $< -o $@

$(EXTRACTOR) : $(CONVERTER_MAIN) $(EXTRACTOR_DEPS)
	$(GHC) $(GHCFLAGS) --make $(GHCPACKAGES) $< -o $@

nogui : $(GENI_MAIN) $(GENI_DEPS) permissions
	$(GHC) $(GHCFLAGS) --make -DDISABLE_GUI $(GHCPACKAGES) $< -o $(GENI)

debugger:
	make $(PROFGENI) PROFILE=1

$(PROFGENI): $(GENI_MAIN) $(GENI_DEPS) permissions
	$(GHC) $(GHCFLAGS) $(GHCPACKAGES) --make $< -o $@

$(SERVER): $(SERVER_MAIN) $(SERVER_DEPS)
	$(GHC) $(GHCFLAGS) --make $(GHCPACKAGES) $< -o $@

$(CLIENT): $(CLIENT_MAIN) $(CLIENT_DEPS)
	$(GHC) $(GHCFLAGS) --make $(GHCPACKAGES) $< -o $@

# --------------------------------------------------------------------
# geni with a precompiled grammar
# --------------------------------------------------------------------

# this stuff is broken down into small pieces so that we can do the hard
# stuff (hs -> hc) on a very fast machine and the rest of the compilation
# locally
src/MyGeniGrammar.hspp : src/MyGeniGrammar.hs
	time nice $(GHC) $(GHCFLAGS) -DPRECOMPILED_GRAMMAR -E +RTS -K100m -RTS $(GHCPACKAGES) $<

src/MyGeniGrammar.hc : src/MyGeniGrammar.hspp
	time nice $(GHC) $(GHCFLAGS) -DPRECOMPILED_GRAMMAR -C +RTS -K100m -RTS $(GHCPACKAGES) $<

src/MyGeniGrammar.p_hc : src/MyGeniGrammar.hspp
	time nice $(GHC) $(GHCFLAGS) -DPRECOMPILED_GRAMMAR -C +RTS -K100m -RTS $(GHCPACKAGES) $<

ifdef PROFILE
MYGENIGRAMMAR_HC := src/MyGeniGrammar.p_hc
else
MYGENIGRAMMAR_HC := src/MyGeniGrammar.hc
endif

src/MyGeniGrammar.o : $(MYGENIGRAMMAR_HC)
	time nice $(GHC) $(GHCFLAGS) -DPRECOMPILED_GRAMMAR -c +RTS -K100m -RTS $(GHCPACKAGES) $<

precompiled: init src/MyGeniGrammar.o
	time $(GHC) $(GHCFLAGS) --make -DPRECOMPILED_GRAMMAR +RTS -K100m -RTS $(GHCPACKAGES) $(GENI_MAIN).lhs -o $(GENI)
	#$(OS_SPECIFIC_STUFF)

weedep: src/MyGeniGrammar.hs
	$(GHC) $(GHCFLAGS) -M $<

# --------------------------------------------------------------------
# installing
# --------------------------------------------------------------------

# FIXME handling of macosx stuff very ugly?

install: $(TO_INSTALL)
	$(foreach file,$^, $(INSTALL) $(file) $(BINDIR) &&)\
	:
ifeq ($(OS),Darwin)
	$(CP) -R bin/geni.app $(BINDIR)
endif

uninstall:
	$(foreach file,$(patsubst bin/%, %, $(TO_INSTALL)), $(RM) $(BINDIR)/$(file) &&)\
	:
ifeq ($(OS),Darwin)
	$(RM) -R $(BINDIR)/geni.app
endif

# --------------------------------------------------------------------
# testing
# --------------------------------------------------------------------

GHCI_FLAGS=$(GHCINCLUDE) -package QuickCheck -fglasgow-exts -cpp

ghci:
	ghci $(GHCI_FLAGS)

unit:
	etc/quickcheck.py src/NLP/GenI/Btypes.lhs | ghci $(GHCI_FLAGS)

profiler: $(PROFGENI_MAIN) profout debugger-geni$(PROFILE_WITH_SCRUNCHED).pdf

profout:
	bin/debugger-geni +RTS $(RTS_FLAGS) -RTS --nogui $(PERFTEST) --opts=pol -o profout

debugger-geni$(PROFILE_WITH_SCRUNCHED).hp: debugger-geni.hp
	cp $< $@

debugger-geni%.pdf: debugger-geni%.hp
	hp2ps $< > $(basename $@).ps
	ps2pdf $(basename $@).ps $(basename $@)-$(DATE2).pdf
	rm -f $@ $(basename $@).ps
	ln -s $(basename $@)-$(DATE2).pdf $@

clean-profiler:
	rm debugger-geni*.pdf

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
