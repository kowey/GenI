# vim: set noexpandtab:

# Useful commands
# - make         compiles your latex project 
# - make tidy    removes all intermediary files (like *.aux)
# - make clean   removes all compiled files (like *.pdf, *.aux)
# - make release creates a tarball that you can give to others

# Unfortunately, you're going to have to edit this Makefile:
# Let me know if the comments here could use any work 
# (kow at loria point fr)

# -------------------------------------------------------------------- 
# the basics (IMPORTANT)
# -------------------------------------------------------------------- 

# Add here any files that you want to compile.  For example:
#   MAKE_DOCS=foo/bar.pdf foo/other.pdf baz/filename.pdf
# If you are making slides instead of documents, you should
# uncomment and modify the MAKE_SLIDES variable.
MAKE_DOCS = ./genimanual.pdf
#MAKE_SLIDES = ./genimanual.pdf

# If you use the MAKE_SLIDES variable, you can set instead:
# COMPILE=$(MAKE_SLIDES) or
# COMPILE=$(MAKE_DOCS) $(MAKE_SLIDES) 
COMPILE = $(MAKE_DOCS)

# You should replace the value of this variable with your project
# directory name.  The default assumption is that the project name
# is the same as the directory name.
MYDIR = genimanual
DATE=`date +%Y-%m-%d`

# -------------------------------------------------------------------- 
# Latex or Pdflatex? (pdflatex by default)
# -------------------------------------------------------------------- 

# If you use latex instead of pdflatex, you should change the line 
# below as well as uncommenting DVIPDF
LATEX=pdflatex
# LATEX=latex

# If you use latex instead of pdflatex, you should uncomment
# DVIPDF 
DVIPDF_CMD=dvips `basename $< .tex`.dvi -o `basename $< .tex`.ps;\
	ps2pdf `basename $< .tex`.ps
#DVIPDF=$(DVIPDF_CMD)

# -------------------------------------------------------------------- 
# BiBTeX (off by default)
# -------------------------------------------------------------------- 

# If you use BibTeX you should uncomment 
BIBTEX_CMD=$(LATEX) `basename $<`;\
	   bibtex `basename $< .tex`;

# If you use BiBTeX, you should uncomment the line below
# BIBTEX=$(BIBTEX_CMD)

# -------------------------------------------------------------------- 
# latex2html (off by default)
# -------------------------------------------------------------------- 

LATEX2HTML = latex2html -math -math_parsing -local_icons -noimages -split 5
# If you want to use latex2html you should uncomment this line
# and add here any tex files you want compiled to html; for 
# example, if you have foo.tex, you should add foo-html
#MAKE_HTML = genimanual-html

# -------------------------------------------------------------------- 
# PPower4 (off by default) 
# -------------------------------------------------------------------- 

PPOWER4_CMD=ppower4 `basename $< .tex`.pdf ppower4-tmp;\
	    mv ppower4-tmp `basename $< .tex`.pdf
# Uncomment the following the following line if you want to use
# PPower4 on your slides
# PPOWER4=$(PPOWER4_CMD)

# -------------------------------------------------------------------- 
# The Makefile (don't touch?)
#
# Normally, you will not have to edit anything here
# -------------------------------------------------------------------- 

.PHONY: clean $(MAKE_HTML) dist

all: $(COMPILE) $(MAKE_HTML) tidy

$(MAKE_DOCS): %.pdf: %.tex 
	cd `dirname $<`;\
	$(BIBTEX)\
	$(LATEX) `basename $<`;\
	$(LATEX) `basename $<`;\
	$(DVIPDF)

$(MAKE_SLIDES): %.pdf: %.tex 
	cd `dirname $<` ;\
	$(LATEX) `basename $<` 
	$(PPOWER4)

$(MAKE_HTML): %-html: %.tex
	rm -rf $@
	$(LATEX2HTML)  $<
	mv `basename $< .tex` $@

tidy:
	for i in $(MAKE_DOCS) $(MAKE_SLIDES); do\
		bn=`basename $${i} .pdf`;\
		rm -f $${bn}.dvi $${bn}.aux $${bn}.log\
		      $${bn}.bbl $${bn}.blg $${bn}.out\
		      $${bn}.toc;\
	done

clean: tidy
	for i in $(MAKE_DOCS); do\
		bn=`basename $${i} .pdf`;\
		rm -f $${bn}.ps $${bn}.pdf;\
	done
	rm -Rf $(MAKE_HTML)


release: $(COMPILE) tidy
	rm -f $(MYDIR)*.tar.gz;\
	cd .. ;\
	tar -czvf $(MYDIR)_$(DATE).tar.gz $(MYDIR);\
	mv $(MYDIR)_$(DATE).tar.gz $(MYDIR)

