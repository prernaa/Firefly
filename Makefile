OCAMLBUILD = ocamlbuild

all: 
	$(OCAMLBUILD) -no-hygiene firefly.byte

clean:
	$(OCAMLBUILD) -clean
	rm -f firefly.byte