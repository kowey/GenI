SHARED_LIB_EXT=dll

PACKAGES=$(patsubst %, -package %,\
	 GenI utf8-string)

all: test-hs test-c test-hs-c

windows: all test-c-win

clean:
	rm -f *.dll *.dll.a
	rm -f *.o *.hi
	rm -f MinimalGenI_stub.*
	rm -f test-hs test-c
	rm -f *~

# Exporting a C API
# -----------------
MinimalGenI.o: MinimalGenI.hs
	ghc --make -fvia-C $<

StartEnd.o:
	ghc -c StartEnd.c

test-hs: test-hs.hs MinimalGenI.o
	ghc --make $<

test-hs-c: test-hs-c.hs MinimalGenI.o
	ghc --make $<

test-c: test-c.c MinimalGenI.o StartEnd.o
	ghc $^ -o $@ MinimalGenI_stub.o $(PACKAGES)

# Bundling into a single shared library
# -------------------------------------
MinimalGenI.$(SHARED_LIB_EXT): MinimalGenI.o StartEnd.o
	ghc -shared -o $@ $^ MinimalGenI_stub.o $(PACKAGES)

test-c-win: test-c.c MinimalGenI.$(SHARED_LIB_EXT)
	ghc $^ -o $@
