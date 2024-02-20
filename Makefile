.PHONY: build bench

build:
	stack build # csound-expression # -typed

bench:
	stack build csound-expression:bench:csound-expression-benchmark  # --profile
