SOURCES=schrotranscode.ml
RESULT=schrotranscode
OCAMLOGG_DIR=../../ocaml-ogg/src
INCDIRS=../src $(OCAMLOGG_DIR)
LIBS=unix bigarray ogg schroedinger
OCAMLC = ocamlfind ocamlc -g 
OCAMLOPT = ocamlfind ocamlopt -g 

all: nc

include OCamlMakefile
