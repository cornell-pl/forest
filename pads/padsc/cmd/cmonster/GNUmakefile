
 # This is a GNU makefile.

PADS_HOME = ../../..
ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package)
  export AST_ARCH
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

PADS_HOME = ../../../..
GEN_DIR = .
GEN_WRITE = 1

# the following indicates that generated .c files should not be removed
.PRECIOUS: $(GEN_DIR)/%.c 

include $(PADS_HOME)/mk/rules.mk

VPATH = ..

GEN_SRC = $(GEN_DIR)/cmdline.c
GEN_OBJ_O = $(GEN_SRC:$(GEN_DIR)/%.c=%.o)
GEN_OBJ_D = $(GEN_SRC:$(GEN_DIR)/%.c=%-g.o)

LOC_SRC = rwfn.c tmap.c iodisc.c cmonster.c
LOC_OBJ_O = $(LOC_SRC:%.c=%.o)
LOC_OBJ_D = $(LOC_SRC:%.c=%-g.o)

SRC = $(GEN_SRC) $(LOC_SRC)
OBJ_O = $(GEN_OBJ_O) $(LOC_OBJ_O)
OBJ_D = $(GEN_OBJ_D) $(LOC_OBJ_D)

CMR_O = cmonster
CMR_D = cmonster-g

ALL_PROGS = $(CMR_O) $(CMR_D)

all: sanity_check $(ALL_PROGS)

sanity_check:
	@$(LibSanityCheck)

clean:
	$(RM) $(ALL_PROGS) *.o *~ tmp/*

veryclean: clean
	$(RM) cmdline.h cmdline.c

cmonster: $(OBJ_O) $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

cmonster-g: $(OBJ_D) $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

mk_rec1vlr-g: mk_rec1vlr.c rec1.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

do_mk: mk_rec1vlr-g
	./mk_rec1vlr-g < ../rec1nl.dat > ../rec1vlr.dat

test1: cmonster-g
	./cmonster-g 'fwrec(:0,8,1:)'         'c{qy1=Pa_string_FW(:8:)[0,8]}' < ../strfw8nl.dat
	./cmonster-g 'fwrec_noseek(:0,8,1:)'  'c{qy1=Pa_string_FW(:8:)[0,8]}' < ../strfw8nl.dat
	./cmonster-g 'nlrec(:0:)'             'c{qy1=Pa_string_FW(:8:)[0,8]}' < ../strfw8nl.dat
	./cmonster-g 'nlrec_noseek(:0:)'      'c{qy1=Pa_string_FW(:8:)[0,8]}' < ../strfw8nl.dat
	./cmonster-g 'ctrec(:10,0:)'          'c{qy1=Pa_string_FW(:8:)[0,8]}' < ../strfw8nl.dat
	./cmonster-g 'ctrec_noseek(:10,0:)'   'c{qy1=Pa_string_FW(:8:)[0,8]}' < ../strfw8nl.dat
	./cmonster-g 'vlrec(:0,0:)'           'c{qy1=Pe_string_FW(:8:)[0,8]}' < ../rec1vlr.dat 
	./cmonster-g 'vlrec_noseek(:0,0:)'    'c{qy1=Pe_string_FW(:8:)[0,8]}' < ../rec1vlr.dat 

 # End of rules that are run from the arch directory
endif


