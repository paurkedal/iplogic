OCAMLBUILD = ocamlbuild

.PHONY: all
all:
	$(OCAMLBUILD) -use-ocamlfind iplogic.cmxa
