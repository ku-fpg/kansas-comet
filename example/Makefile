kcomet-example::
	rm -f Main.o
# 	ghc --make Main.hs -o kcomet-example -threaded
	ghc --make Main.hs -o kcomet-example -threaded -i..:../dist/build/autogen/
	export kansas_comet_datadir=.. ; ./kcomet-example

tictactoe::
	rm -f TicTacToe.o
	ghc --make TicTacToe.hs -o tictactoe -threaded -i..:../dist/build/autogen/
	export kansas_comet_datadir=.. ; ./tictactoe
