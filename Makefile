code:	
	ocamlbuild -use-menhir main.native

toplevel:
	 ocamlbuild -use-menhir top.top;
	./top.top -I _build/

clean:
	rm -r _build main.native
