GHC = ghc
GHCFLAGS = -package-conf $(HOME)/.ghc-packages

test: *.hs
	$(GHC) $(GHCFLAGS) --make -o $@ test.hs

unittests: *.hs
	$(GHC) $(GHCFLAGS) --make -o $@ DirectoryExtTests.hs

check: unittests
	./unittests

setup: Setup.lhs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

clean:
	-rm -f *.o *.hi setup test unittests