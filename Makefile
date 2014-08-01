OCAMLBUILD = ocamlbuild

all: 
	$(OCAMLBUILD) -no-hygiene firefly3D.byte

clean:
	$(OCAMLBUILD) -clean
	rm -f firefly3D.byte