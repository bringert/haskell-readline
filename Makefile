test: *.hs
	ghc --make -o test test.hs

unittests: 
	runhugs -P:/home/bjorn/src/HUnit-1.0/ DirectoryExtTests.hs

clean:
	-rm -f *.o *.hi test