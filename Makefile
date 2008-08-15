
all: build 

clean:
	runhaskell Setup.hs clean

configure: Setup.hs denominate.cabal *.hs
	runhaskell Setup.hs configure --user --prefix=${HOME} \
		--docdir=dist/doc \
		--haddock-options="-v \
		--source-module=http://protempore.net/denominate/doc/src/%M.hs"

build: configure
	runhaskell Setup.hs build

haddock: configure build
	runhaskell Setup.hs haddock --hyperlink-source

install:  configure build
	runhaskell Setup.hs install

test:
	runhaskell Setup.hs test

