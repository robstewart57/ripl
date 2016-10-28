all: ripl-frontend ripl-compiler

clean:
	stack clean
	cd src; rm -f Abs*.hs; rm -f Par*.hs; rm -f Skel*.hs; rm -f Lex*.hs; rm -f ErrM.hs; rm -f Print*.hs

ripl-frontend:
	bnfc -m --haskell -o src/ RIPL.cf
	cd src; make; rm LexRIPL.x ParRIPL.y; rm ErrM.hs

ripl-compiler:
	# run `riplc with +RTS -xc -RTS at the end of the command to see stack trace when things go wrong.
	stack install --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"
