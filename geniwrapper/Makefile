PACKAGES=$(patsubst %, -package %,\
	 GenI utf8-string)

all: test-hs test-c

windows: all test-c-win

clean:
	rm -f *.dll *.dll.a
	rm -f *.o *.hi
	rm -f MinimalGenI_stub.*
	rm -f test-hs test-c
	rm -f *~

MinimalGenI.o: MinimalGenI.hs
	ghc --make -fvia-C $<

HsStart.o:
	ghc -c HsStart.c

MinimalGenI.dll: MinimalGenI.o HsStart.o
	ghc -shared -o $@ $^ MinimalGenI_stub.o $(PACKAGES)

test-hs: test-hs.hs MinimalGenI.o
	ghc --make $<

test-c: test-c.c MinimalGenI.o HsStart.o
	ghc $^ -o $@ MinimalGenI_stub.o $(PACKAGES)

test-c-win: test-c.hs MinimalGenI.dll
	ghc $^ -o $@
