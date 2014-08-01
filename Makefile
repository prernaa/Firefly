OCAMLBUILD = ocamlbuild

all: 
	$(OCAMLBUILD) firefly3D.byte

clean:
	$(OCAMLBUILD) -clean
	rm -f firefly3D.byte
