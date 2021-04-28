MODULES=author commands basicOp main derivative dual
OBJECTS=$(MODULES:=.cmo)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

frontend:
	ocamlbuild -use-ocamlfind frontEnd.byte

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

clean:
	ocamlbuild -clean

calc:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

zip:
	zip final_project.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit Makefile *.md	

docs: docs-public
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d _doc.public $(MLIS)
