
 # This is an nmake file.  See GNUmakefile for the gmake version.

if ! "$(PWD:N=*/$(CC.HOSTTYPE))"

PADS_HOME = ..
include $(PADS_HOME)/mk/redirect.nmake

else

 # The following rules are run from the arch directory

:PACKAGE: ast

PADS_HOME = ../..
include $(PADS_HOME)/mk/rules.nmake

SCRIPTS = $(PADS_HOME)/scripts

.SOURCE : .. $(PADS_HOME)/include $(SCRIPTS)

padsc 1.0 :LIBRARY: rbuf.c default_io_disc.c libpadsc-gen.c libpadsc-acc-gen.c libpadsc-read-gen.c libpadsc-misc-gen.c

$(INCLUDEDIR) :INSTALLPROTO: libpadsc.h libpadsc-internal.h rbuf.h

GEN_FILES = libpadsc-acc-gen.c \
            libpadsc-misc-gen.c \
            libpadsc-acc-macros-gen.h  \
            libpadsc-misc-macros-gen.h \
            libpadsc-gen.c             \
            libpadsc-read-gen.c \
            libpadsc-macros-gen.h      \
            libpadsc-read-macros-gen.h

$(GEN_FILES) :JOINT: mksrc.pl libpadsc.c
	$(*) $(PWD) $(PADS_HOME)

clean :
	$(RM) -f libpadsc.* libpadsc-g.* *~ *.o ../*~

veryclean : clean
	$(RM) -f Makefile.ms Makefile.mo padsc.req ../Makefile.ms ../Makefile.mo $(GEN_FILES) 

 # End of rules are run from the arch directory
end
