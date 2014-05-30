all: netslum

clean:
	rm -rf *.hi *.o */*.hi */*.o
	rm -rf netslum
	cabal clean

netslum:
	cabal configure
	cabal build && cp dist/build/netslum/netslum .

install:
	cabal install
