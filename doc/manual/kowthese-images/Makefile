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
# MAKE_DOCS=foo/bar.pdf foo/other.pdf baz/filename.pdf
#MAKE_IMAGES:=$(patsubst %,images/%.eps,$(SELF_CONTAINED_IMAGES))
DIAGRAMS=kelvin-runs-1\
	 kelvin-runs-2
	 
MAKE_IMAGES=$(patsubst %,%.png,$(DIAGRAMS))

all: $(MAKE_IMAGES)

$(MAKE_IMAGES): %.png: %.tex
	cd $(<D); latex $(<F)
	dvips -E $(basename $<).dvi -o $(basename $<).eps
	convert -trim $(basename $<).eps $(basename $<).png
	#dvipng $(basename $<).dvi

images/realaut-%.eps: images/realaut-%.dot
	dot -T ps2 $< -o $@

clean:
	rm -rf *.png *.aux
