SML=sml

all:	compiler 

compiler:
	echo Making compiler
	sml < build-exec.sml

redo_examples:
	echo Redoing all the examples
	./padsc examples/p/ai.p
	./padsc examples/p/dibbler.p
	./padsc examples/p/format1.p
	./padsc examples/p/format2.p
	./padsc examples/p/format3.p
	./padsc examples/p/format4.p
	./padsc examples/p/format5.p
	./padsc examples/p/format6.p
	./padsc examples/p/netflow.p
	./padsc examples/p/readinttest.p
	./padsc examples/p/from.p

error_examples:
	./padsc examples/p/enum.p
