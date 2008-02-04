HHL_LIB_DIR = $(OCAML_LIB_DIR)/hhl
INCLUDES += -I $(HHL_LIB_DIR)
OCAMLFLAGS= 
OCAMLC=ocamlopt
OCAMLOPT=ocamlopt.opt
TESTPROG=testhmm
default: all
all: test

test: testhmm.ml
	$(OCAMLC) $(OCAMLFLAGS) $(INCLUDES) -o $(TESTPROG) str.cmxa hhl.cmxa $<

clean:
	rm -rf *.cm[iox] *.o $(TESTPROG)
