all: build

clean:
	cabal clean

configure: Setup.hs denominate.cabal *.hs
	cabal configure --user --prefix=${HOME}

build: configure
	cabal build

install:  configure build
	cabal install

configure-tests: Setup.hs denominate.cabal *.hs
	cabal configure --user --prefix=${HOME} --enable-tests

build-tests: configure-tests
	cabal build

test: build-tests
	cabal test  --test-options='--maximum-unsuitable-generated-tests 10000 --maximum-generated-tests 1000'

