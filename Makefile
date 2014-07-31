SOURCES = scanner.mll \
          parser.mly \
          ast.ml \
          parser.ml \
          scanner.ml \
          firefly3D.ml \
          compile.ml

OCAMLBUILD = ocamlbuild

all: 
	$(OCAMLBUILD) firefly3D.byte

clean:
	$(OCAMLBUILD) -clean
	rm -f firefly3D.byte
	
FORCE: