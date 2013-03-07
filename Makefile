.PHONY: config \
        test

config:
	cabal configure --disable-library-profiling --disable-shared

test:
	runhaskell test/Tests.hs
