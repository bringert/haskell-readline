test: *.hs
	ghc --make -o test test.hs

clean:
	-rm -f *.o *.hi test