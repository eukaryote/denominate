#!/bin/sh

if [ "$(pwd)" != "/home/calvins/denominate" ]
then
    echo "Must run from denominate directory."
    exit 1
fi

ln -sf ./www/index.xhtml
ln -snf dist/doc/html doc
pushd www &> /dev/null
ln -sf ../dist/denominate*.tar.gz 
popd &> /dev/null

