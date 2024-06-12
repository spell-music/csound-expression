.PHONY: build bench run

build:
	stack build csound-expression-typed

bench:
	stack build csound-expression:bench:csound-expression-benchmark  # --profile

run:
	stack runhaskell scripts/Oscils.hs
#	csound -odac ./csound-expression-typed-core/tmp.csd
