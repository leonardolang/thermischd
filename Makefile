OCFLAGS = -O3 -lto -use-lto 

all:
	ocamlfind ocamlopt $(OCFLAGS) -thread -package base,spawn,stdio,unix -linkpkg -o thermischd main.ml
	strip --strip-unneeded thermischd

clean:
	rm -f thermischd main.cmo main.cmi main.cmx main.o

install:
	./install.sh

opam:
	opam switch install 4.06.1+lto
	opam install ocamlfind base stdio
