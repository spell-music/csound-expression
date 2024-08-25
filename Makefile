.PHONY: build bench run

GHC_OPTIONS="-Wall -Werror -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wunused-packages -Wstar-is-type"

build:
	stack build 

# --ghc-options=${GHC_OPTIONS}

bench:
	stack build csound-expression:bench:csound-expression-benchmark  # --profile

run:
	stack runhaskell scripts/Oscils.hs
#	csound -odac ./csound-expression-typed-core/tmp.csd
