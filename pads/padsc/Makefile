SML=sml

all:	compiler 

compiler:
	echo Making compiler
	sml < build-exec.sml

redo_examples:
	echo Redoing all the examples
	./padsc examples/ai.p
	./padsc examples/dibbler.p
	./padsc examples/format1.p
	./padsc examples/format2.p
	./padsc examples/format3.p
	./padsc examples/format4.p
	./padsc examples/format5.p
	./padsc examples/format6.p
	./padsc examples/netflow.p
	./padsc examples/readinttest.p
	./padsc examples/from.p

error_examples:
	./padsc examples/enum.p
