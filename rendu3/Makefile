# test_files=$(wildcard tests-rendu3/*/*.ml)

all: fouine

fouine: main.native
	mv $^ $@

main.native:
	ocamlbuild -use-menhir -lib unix main.native

byte:
	ocamlbuild -use-menhir main.byte

clean:
	ocamlbuild -clean

# test:
# 	foreach test_file, $(test_files), ./fouine -debug -outval $(test_file)


# ocamlbuild -yaccflag -v -lib unix main.native
