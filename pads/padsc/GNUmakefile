 # This is a GNU makefile.

all:	library compiler

library:
	echo Making padsc library
	@(cd libpadsc; $(MAKE) install)

compiler:
	echo Making padsc compiler
	@(cd comp; $(MAKE) all)

clean:
	@(cd libpadsc; $(MAKE) clean)
	@(cd comp; $(MAKE) clean)

veryclean:
	@(cd libpadsc; $(MAKE) veryclean)
	@(cd comp; $(MAKE) veryclean)
