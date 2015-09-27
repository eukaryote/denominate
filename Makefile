all: build

clean:
	runhaskell Setup.hs clean

configure: Setup.hs denominate.cabal *.hs
	runhaskell Setup.hs configure --user --prefix=${HOME}

build: configure
	runhaskell Setup.hs build

install:  configure build
	runhaskell Setup.hs install

configure-tests: Setup.hs denominate.cabal *.hs
	runhaskell Setup.hs configure --user --prefix=${HOME} --enable-tests

build-tests: configure-tests
	runhaskell Setup.hs build

test: build-tests
	runhaskell Setup.hs test  --test-options='--maximum-unsuitable-generated-tests 10000 --maximum-generated-tests 1000'

