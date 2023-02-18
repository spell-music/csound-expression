build:
	stack build csound-core

bench:
	stack build csound-expression:bench:csound-expression-benchmark  # --profile

run:
	stack runhaskell tutorial/core/live/01-play-file.hs # scripts/Core.hs
#	csound -odac ./csound-expression-typed-core/tmp.csd
