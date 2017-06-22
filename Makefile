.PHONY: build clean ghci haddock haddock-server run test watch watch-test
all: build

build:
	stack build

clean:
	stack clean

ghci:
	stack ghci

haddock:
	stack build --haddock

# This runs a small python websever on port 8001 serving up haddocks for
# packages you have installed.
#
# In order to run this, you need to have run `make build-haddock`.
haddock-server:
	cd "$$(stack path --local-doc-root)" && python -m http.server 8001

run: build
	stack exec -- arow-sample-webapp

test:
	stack test

# Watch for changes.
watch:
	stack build --file-watch --fast

# Watch for changes.
watch-test:
	stack test --file-watch --fast
