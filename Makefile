MODULES=author
OBJECTS=$(MODULES:=.cmo)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

check:
	@bash check.sh
	
finalcheck:
	@bash check.sh final

clean:
	ocamlbuild -clean

zip:
	zip final_project.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit Makefile	
