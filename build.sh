#!/bin/sh
export OCAMLFIND_DESTDIR=$PWD/ocamlfind
export OCAMLPATH=$OCAMLFIND_DESTDIR:$OCAMLPATH
export OCAMLFIND_IGNORE_DUPS_IN=$OCAMLFIND_DESTDIR
rm -rf $OCAMLFIND_DESTDIR
mkdir -p $OCAMLFIND_DESTDIR
if ! (cd OCamlV4l2 && 
	( ([ -r setup.ml ] || oasis setup) && 
	    [ -r setup.data ] || ocaml setup.ml -configure &&
	    [ -r _build/v4l2.cma ] || ocaml setup.ml -build &&
	    ocaml setup.ml -install >/dev/null 2>/dev/null) ); then
    echo "Cannot build v4l2"
    if ocamlfind query v4l2; then
	echo "But v4l2 is already installed, using it"
    else
	echo "Aborting"
	exit 1
    fi
fi
ocamlbuild -use-ocamlfind -j 8 main.native main.byte
