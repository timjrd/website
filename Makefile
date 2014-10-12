HC=ghc --make -static -optl-static -optl-pthread -outputdir Builds/.cache

devel:
	mkdir -p Builds
	$(HC) -o Builds/SERVER Main.hs
	$(HC) -o Builds/set-password Hash.hs

release: clean
	mkdir -p Builds
	$(HC) -O2 -fforce-recomp -o Builds/SERVER Main.hs
	$(HC) -O2 -fforce-recomp -o Builds/set-password Hash.hs


clean:
	rm -r Builds
