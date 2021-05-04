test_files=$(wildcard tests-rendu4/*/*.ml)
do_test=$(patsubst %.ml, %.do_test, $(test_files))

OPT=-autotest -cps

all: main.native
	mv main.native fouine

main.native:
	ocamlbuild -use-menhir -lib unix main.native

byte:
	ocamlbuild -use-menhir main.byte

clean:
	ocamlbuild -clean

test: $(do_test)

%.do_test: %.ml all
	./fouine $(OPT) $<

# ocamlbuild -yaccflag -v -lib unix main.native
