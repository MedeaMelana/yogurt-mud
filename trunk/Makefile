default:
	ghci NewMoon

mtag = $(shell svn info | grep URL | sed 's/^.*Yogurt-//g')
tag  = $(subst URL: svn://localhost/Yogurt/trunk,99.99,$(mtag))

cabal-config: clean
	sed "s/@tag/$(tag)/g" < Yogurt.cabal-template > Yogurt.cabal
	runghc Setup configure

package: cabal-config
	runghc Setup sdist

docs: cabal-config
	runghc Setup haddock

clean:
	find . -name '*.hi' -or -name '*.o' | xargs -n 1 rm
	rm -rf dist build Yogurt.cabal
