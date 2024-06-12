.PHONY: build bench run

build:
	stack build gen-csound-opcodes

bench:
	stack build csound-expression:bench:csound-expression-benchmark  # --profile

test:
	stack build csound-expression-opcodes

run:
	stack run gen-csound-opcodes
#	stack runhaskell scripts/Oscils.hs
#	csound -odac ./csound-expression-typed-core/tmp.csd
