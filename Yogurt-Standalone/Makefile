default: build

config: clean
	cabal configure

build: config
	cabal build

install: config
	cabal install

package: config
	cabal sdist

docs: config
	cabal haddock

clean:
	find . -name '*.hi' -or -name '*.o' | xargs -n 1 rm
	rm -rf dist build yogurt
