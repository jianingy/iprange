.PHONY: all

all: iprange

iprange: iprange.ml
	ocamlfind ocamlopt -o iprange iprange.ml -linkpkg -package str

clean:
	rm -f iprange iprange.cmx iprange.cmi iprange.o
