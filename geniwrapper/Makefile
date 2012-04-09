OS=$(shell uname)

ifeq ($(OS),Darwin)
  SHARED_LIB_EXT=dylib
  SHARED_LIB_FLAGS=-dynamic -shared
else
  SHARED_LIB_EXT=dll
  SHARED_LIB_FLAGS=-shared
endif

PACKAGES=$(patsubst %, -package %,\
	 GenI utf8-string)

all: test-hs test-c test-c2 test-hs-c

windows: all test-c-win

clean:
	rm -f *.dll *.dll.a *.dylib
	rm -f *.o *.hi
	rm -f MinimalGenI_stub.*
	rm -f test-hs test-c test-hs-c
	rm -f *~

# Exporting a C API
# -----------------
MinimalGenI.o: MinimalGenI.hs
	ghc --make -fvia-C $(GHC_OPTS) $<

StartEnd.o:
	ghc -c StartEnd.c

test-hs: test-hs.hs MinimalGenI.o
	ghc --make $(GHC_OPTS) $<

test-hs-c: test-hs-c.hs MinimalGenI.o
	ghc --make $(GHC_OPTS) $<

test-c: test-c.c MinimalGenI.o StartEnd.o
	ghc $^ -o $@ MinimalGenI_stub.o $(GHC_OPTS) $(PACKAGES)

# Bundling into a single shared library
# -------------------------------------
MinimalGenI.$(SHARED_LIB_EXT): MinimalGenI.o StartEnd.o
	ghc $(SHARED_LIB_FLAGS) -o $@ $^ MinimalGenI_stub.o $(GHC_OPTS) $(PACKAGES)

test-c2: test-c.c MinimalGenI.$(SHARED_LIB_EXT)
	ghc $^ -o $(GHC_OPTS) $@
