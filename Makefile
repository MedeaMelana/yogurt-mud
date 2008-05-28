default:
	ghci NewMoon

nm:
	runghc NewMoon

docs:
	mkdir docs
	haddock -B /usr/local/lib/ghc-6.8.2 -o docs --html Network/Yogurt.hs Network/Yogurt/Utils.hs
	#open -a Safari docs/index.html

clean:
	find . -name '*.hi' -or -name '*.o' | xargs -n 1 rm
	rm -rf docs dist

package:
	tar cvf Yogurt-0.1.tar Network Yogurt.cabal Setup.hs
	gzip Yogurt-0.1.tar
