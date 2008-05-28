default:
	ghci NewMoon

tag = $(shell svn info | grep URL | sed 's/^.*Yogurt-//g')

package: cabal-config
	runghc Setup sdist

nm:
	runghc NewMoon

docs: cabal-config
	runghc Setup haddock

cabal-config:
	sed "s/@tag/$(tag)/g" < Yogurt.cabal-template > Yogurt.cabal
	runghc Setup configure

clean:
	find . -name '*.hi' -or -name '*.o' | xargs -n 1 rm
	rm -rf dist build Yogurt.cabal
