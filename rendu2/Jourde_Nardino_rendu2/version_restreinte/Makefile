all: main.native
	mv main.native fouine

main.native:
	ocamlbuild -use-menhir -lib unix main.native

byte: 
	ocamlbuild -use-menhir main.byte

clean: 
	ocamlbuild -clean



# ocamlbuild -yaccflag -v -lib unix main.native
