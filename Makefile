default:
	ghci NewMoon

mtag = $(shell svn info | grep URL | sed 's/^.*Yogurt-//g')
tag  = $(subst URL: https://yogurt-mud.googlecode.com/svn/trunk,99.99,$(mtag))

cabal-config: clean
	sed "s/@tag/$(tag)/g" < Yogurt.cabal-template > Yogurt.cabal
	cabal configure

package: cabal-config
	cabal sdist

docs: cabal-config
	cabal haddock

clean:
	find . -name '*.hi' -or -name '*.o' | xargs -n 1 rm
	rm -rf dist build Yogurt.cabal
