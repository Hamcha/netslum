all: netslum

clean:
	rm -rf *.hi *.o */*.hi */*.o
	cabal clean

netslum:
	cabal configure
	cabal build && cp dist/build/netslum/netslum .

install:
	cabal install
