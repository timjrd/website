HC=ghc --make -static -optl-static -optl-pthread -outputdir Builds/.cache

.PHONY: devel release css clean

devel: Builds/bin
	$(HC) -o Builds/bin/SERVER Main.hs
	$(HC) -o Builds/bin/set-password Hash.hs

release: clean Builds/bin static
	$(HC) -O2 -fforce-recomp -o Builds/bin/SERVER Main.hs
	$(HC) -O2 -fforce-recomp -o Builds/bin/set-password Hash.hs



css: Builds/static Builds/static/style.css 

Builds/static/%.css: %.css
	autoprefixer -b "last 90 versions" -o - $< | cleancss -o $@

Builds/static:
	-mkdir -p Builds/static
Builds/bin:
	-mkdir -p Builds/bin


clean:
	-rm -r Builds
