all: test test-c

clean:
	rm -f *.o *.hi
	rm -f Wrapper_stub.*
	rm -f test test-c

Wrapper.o: Wrapper.hs
	ghc --make -fvia-C $<

test: test.hs Wrapper.o
	ghc --make $<

test-c: test-c.c Wrapper.o
	ghc $^ Wrapper_stub.o -package GenI -o $@
