:PACKAGE: ast

.SOURCE : ../include

PADS_HOME = ..

CCFLAGS = $(CC.DEBUG) $(CC.DLL)

all :
	echo "nmake in one of the architecture subdirs\n\tor use 'nmake gen' to just rebuild generated src files"

gen : libpadsc-macros-gen.h

also_built = "libpadsc-read-macros-gen.h libpadsc-acc-macros-gen.h libpadsc-read-gen.c libpadsc-accum-gen.c libpadsc-gen.c"

libpadsc-macros-gen.h : libpadsc.c mksrc.pl ../include/libpadsc.h ../include/libpadsc-internal.h ../include/rbuf.h ../include/rbuf-internal.h
	mksrc.pl libpadsc.c

clean :
	$(RM) -f *gen* *~ \#*

veryclean : clean
	$(RM) -f Makefile.ms Makefile.mo

