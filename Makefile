NAME = efyu

CABAL = cabal
NODEMON = nodemon

hpack:
	hpack

build: hpack
	$(CABAL) v2-build

run: hpack
	$(CABAL) v2-run $(NAME)

run-w:
	$(NODEMON) --exec '(clear && make run) || true' -e .hs,.fu --ignore dist-newstyle

test: hpack
	$(CABAL) v2-test

test-w:
	$(NODEMON) --exec '(clear && make test) || true' -e .hs --ignore dist-newstyle

