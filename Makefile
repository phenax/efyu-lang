CABAL = cabal
NODEMON = nodemon

build:
	$(CABAL) v2-build

build-w:
	$(NODEMON) --exec '(clear && make build) || true' -e .hs

test:
	$(CABAL) v2-test

test-w:
	$(NODEMON) --exec '(clear && make test) || true' -e .hs

