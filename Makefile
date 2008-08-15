
all: build 

clean:
	runhaskell Setup.hs clean

configure: Setup.hs denominate.cabal *.hs
	runhaskell Setup.hs configure --user --prefix=${HOME}

build: configure
	runhaskell Setup.hs build

haddock: configure build
	runhaskell Setup.hs haddock
	find dist/doc/html -name '*.html' -exec \
		sed -i -r 's_/usr/share/doc/ghc-[^/]+/html/libraries/_http://www.haskell.org/ghc/docs/latest/html/libraries/_g' {} \;

install:  configure build
	runhaskell Setup.hs install

