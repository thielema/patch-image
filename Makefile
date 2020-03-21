run-test:
	runhaskell Setup configure --user --enable-executable-dynamic \
	    -fbuildDraft -fllvm -fcuda
	runhaskell Setup build
