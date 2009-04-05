ghci:
	ghci Network.Yogurt

clean:
	find . -name '*.hi' -or -name '*.o' | xargs -n 1 rm
	rm -rf dist build

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

opendocs: docs
	open dist/doc/html/Yogurt/index.html
