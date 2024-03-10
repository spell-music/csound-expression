.PHONY: build bench run

build:
	stack build csound-expression-dynamic

bench:
	stack build csound-expression:bench:csound-expression-benchmark  # --profile

run:
	stack runhaskell tutorial/core/live/02-play-notes.hs # scripts/Core.hs
#	csound -odac ./csound-expression-typed-core/tmp.csd
