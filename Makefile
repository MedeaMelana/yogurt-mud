default:
	ghci -isrc NewMoon

nm:
	runghc -isrc NewMoon

docs:
	mkdir docs
	haddock -B /usr/local/lib/ghc-6.8.2 --optghc=-isrc -o docs --html src/Yogurt.hs src/Yogurt/Utils.hs
	#open -a Safari docs/index.html

clean:
	find . -name '*.hi' -or -name '*.o' | xargs -n 1 rm
	rm -rf docs
