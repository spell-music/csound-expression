build:
	stack build  # csound-expression

bench:
	stack build csound-expression:bench:csound-expression-benchmark  # --profile
