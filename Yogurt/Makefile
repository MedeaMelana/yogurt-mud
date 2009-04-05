default:
	ghci NewMoon

mtag = $(shell svn info | grep URL | sed 's/^.*Yogurt-//g')
tag  = $(subst URL: https://yogurt-mud.googlecode.com/svn/trunk,99.99,$(mtag))

config: clean
	sed "s/@tag/$(tag)/g" < Yogurt.cabal-template > Yogurt.cabal
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
	rm -rf dist build Yogurt.cabal yogurt
