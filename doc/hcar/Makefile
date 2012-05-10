# vim: set noexpandtab:

# Useful commands
# - make         compiles your latex project
# - make tidy    removes all intermediary files (like *.aux)
# - make clean   removes all compiled files (like *.pdf, *.aux)
# - make release creates a tarball that you can give to others

# --------------------------------------------------------------------
# Things you probably have to edit
#
# Let me know if the comments here could use any work
# (kow at loria point fr)
# --------------------------------------------------------------------

# Add here any files that you want to compile.  For example:
# MAKE_DOCS=./darcs-hcar-slides.pdf foo/other.pdf baz/filename.pdf
MAKE_DOCS=./hcar.pdf

# Define here any other files that your document depends on
# besides darcs-hcar.tex
EXTRA_DOC_SOURCE=GenI-EG.tex

TARBALL_BASENAME=hcar

# pdflatex or latex
LATEX=pdflatex

# Comment this line if you don't use BiBTeX
#BIBTEX_FILES=%.bib
# Uncomment this line if you want to stop when BiBTeX fails
#WATCH_BIBTEX=.

# Uncomment this line if you want to produce HTML
# MAKE_HTML=hcar$(HTML_SUFFIX)

# long or short?
HTML_TYPE=short

# --------------------------------------------------------------------
# Things you might have to edit, but probably not
# --------------------------------------------------------------------

COMPILE:=$(MAKE_DOCS)

# Files to tidy and clean
TIDY_EXTS=dvi aux log bbl blg out toc tex~ idx mtc mtc* ilg hind bmt ind nav
CLEAN_EXTS=ps pdf
TIDY_FILES=$(foreach i, $(COMPILE),\
	   $(foreach ext, $(TIDY_EXTS),$(basename $(i)).$(ext)))
CLEAN_FILES=$(foreach i, $(COMPILE),\
	    $(foreach ext, $(CLEAN_EXTS),$(basename $(i)).$(ext)))

# Tarball stuff
DATE:=$(shell date +%Y-%m-%d)
TARBALL_NAME:=$(TARBALL_BASENAME)_$(DATE).tar.gz

# Use ps2pdf if we're using latex instead of pdflatex
ifeq ($(LATEX),latex)
PS2PDFFLAGS=-sPAPERSIZE=a4
DVIPDF_CMD=dvips `basename $< .tex`.dvi -o `basename $< .tex`.ps;\
	ps2pdf $(PS2PDFFLAGS) `basename $< .tex`.ps
DVIPDF=$(DVIPDF_CMD)
endif

# Should we die if bibtex dies?
ifdef WATCH_BIBTEX
AFTER_BIBTEX=&&
else
AFTER_BIBTEX=;
endif
# Are we using bibtex?
ifdef BIBTEX_FILES
BIBTEX_STEP=$(LATEX) $(<F) && bibtex $(patsubst %.tex,%,$(<F)) $(AFTER_BIBTEX)
endif

# How do we produce HTML?
ifeq ($(HTML_TYPE),short)
LATEX2HTML= hevea
HTML_SUFFIX=.html
endif
ifeq ($(HTML_TYPE),long)
LATEX2HTML = latex2html -math -math_parsing -local_icons -noimages -split 5
HTML_SUFFIX=-html
endif


# --------------------------------------------------------------------
# The Makefile proper - even less stuff to edit here
#
# Normally, you will not have to edit anything here
# --------------------------------------------------------------------

.PHONY: clean $(MAKE_HTML) dist release srcrelease tarball

all: $(COMPILE) $(MAKE_HTML)

$(MAKE_DOCS): %.pdf: %.tex $(EXTRA_DOC_SOURCE) $(BIBTEX_FILES)
	cd $(<D) &&\
	$(BIBTEX_STEP)\
	$(LATEX) $(<F) && $(LATEX) $(<F) &&\
	:
	$(DVIPDF)

$(MAKE_HTML): %$(HTML_SUFFIX): %.tex
	rm -rf $@
	$(LATEX2HTML)  $<
ifeq ($(HTML_TYPE),long)
	mv $(basename $(<F)) $@
endif

tidy:
	rm -f $(TIDY_FILES)

clean: tidy
	rm -f $(CLEAN_FILES)
ifdef MAKE_HTML
	rm -Rf $(MAKE_HTML)
endif

srcrelease: tidy clean tarball

release: $(COMPILE) tidy tarball

tarball: $(TARBALL_NAME)

$(TARBALL_NAME):
	rm -rf $(TARBALL_BASENAME)*.tar.gz
	cd ..; tar -czv --exclude _darcs -f $@ $(notdir $(PWD))
	mv ../$@ .
