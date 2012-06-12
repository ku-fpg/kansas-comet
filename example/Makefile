
kcomet-example: Main.hs
	ghc --make Main.hs -o kcomet-example -threaded \
             -fforce-recomp -O2 -dcore-lint 

run-kcomet-example: kcomet-example
	./kcomet-example

# This version can be used before cabal install
#
# You need to have done cabal build before this will find the 
# ../dist/build/autogen/Paths_kansas_comet.hs
kcomet-example-inplace: Main.hs ../Web/KansasComet.hs 
	ghc --make Main.hs -o kcomet-example-inplace -threaded \
		-fforce-recomp -O2 -dcore-lint \
		-i..:../dist/build/autogen/

run-kcomet-example-inplace: kcomet-example-inplace
	export kansas_comet_datadir=.. ; ./kcomet-example-inplace

ghci:
	export kansas_comet_datadir=.. ; ghci Main.hs \
		-i..:../dist/build/autogen/
