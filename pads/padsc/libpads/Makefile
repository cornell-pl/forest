:PACKAGE: ast

.SOURCE : ../include

CCFLAGS = $(CC.DEBUG) $(CC.DLL)

all : libpadsc-macros-gen.h

clean :
	$(RM) -f *gen* *~ #*

veryclean : clean
	$(RM) -f Makefile.ms Makefile.mo padsc.req

also_built = "libpadsc-read-macros-gen.h libpadsc-acc-macros-gen.h libpadsc-read-gen.c libpadsc-accum-gen.c libpadsc-gen.c"

libpadsc-macros-gen.h : libpadsc.c mksrc.pl ../include/libpadsc.h ../include/libpadsc-internal.h ../include/rbuf.h ../include/rbuf-internal.h
	mksrc.pl libpadsc.c

