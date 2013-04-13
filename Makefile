.PHONY: all

all: ip_compress ip_expand

iprange: iprange.ml
	ocamlfind ocamlopt -c iprange.ml -linkpkg -package pcre

ip_compress: ip_compress.ml iprange
	ocamlfind ocamlopt -c ip_compress.ml
	ocamlfind ocamlopt -o ipcompress iprange.cmx ip_compress.cmx -linkpkg -package pcre

ip_expand: ip_expand.ml iprange
	ocamlfind ocamlopt -c ip_expand.ml
	ocamlfind ocamlopt -o ipexpand iprange.cmx ip_expand.cmx -linkpkg -package pcre

clean:
	rm -f ip_compress *.cmx *.cmi *.o
