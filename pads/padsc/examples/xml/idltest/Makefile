#OCAMLBIN=/home/mff/ocaml-3.06-rh7//bin
CAMLIDLLIB=/usr/common/lib/ocaml
OCAMLBIN=/usr/common/bin
all:
	/usr/common/bin/camlidl -header test.idl
	$(OCAMLBIN)/ocamlc -I $(CAMLIDLLIB) -c test.mli
	$(OCAMLBIN)/ocamlc -I $(CAMLIDLLIB) -c test.ml
	gcc -Wall -g -I $(CAMLIDLLIB) -c test_stubs.c
	gcc -c test.c
	$(OCAMLBIN)/ocamlc -I $(CAMLIDLLIB) -c test_idl.ml
	$(OCAMLBIN)/ocamlc -I $(CAMLIDLLIB) -cc cc -custom -o test test.cmo test_stubs.o test.o test_idl.cmo $(CAMLIDLLIB)/libcamlidl.a

run: all
	./test

clean:
	rm -f a.out test *.o *.cmo *.cmi test.mli test.ml test_stubs.c test.h