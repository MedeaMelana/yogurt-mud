default:
	ghci -isrc NewMoon

nm:
	runghc -isrc NewMoon

clean:
	rm *.hi *.o

