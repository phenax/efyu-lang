NAME = efyu

CABAL = cabal
NODEMON = nodemon

build:
	$(CABAL) v2-build

run:
	$(CABAL) v2-run $(NAME)

run-w:
	$(NODEMON) --exec '(clear && make run) || true' -e .hs --ignore dist-newstyle

test:
	$(CABAL) v2-test

test-w:
	$(NODEMON) --exec '(clear && make test) || true' -e .hs --ignore dist-newstyle

