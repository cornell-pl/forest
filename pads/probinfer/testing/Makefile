HHL_LIB_DIR = hhld/hhl
INCLUDES += -I $(HHL_LIB_DIR)
OCAMLFLAGS= 
OCAMLC=ocamlopt
OCAMLOPT=ocamlopt.opt
TESTPROG=testhmm
default: all
hhl:
	$(MAKE) -C hhld

all: hhl test

test: testhmm.ml
	$(OCAMLC) $(OCAMLFLAGS) $(INCLUDES) -o $(TESTPROG) str.cmxa hhl.cmxa $<

clean:
	cd hhld; make clean
	rm -rf *.cm[iox] *.o $(TESTPROG)
