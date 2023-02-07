build:
	stack build csound-expression-typed-core

bench:
	stack build csound-expression:bench:csound-expression-benchmark  # --profile

run:
	stack runhaskell scripts/Core.hs
