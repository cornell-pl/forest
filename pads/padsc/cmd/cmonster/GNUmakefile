
 # This is a GNU makefile.

PADS_HOME = ../../..
ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/ast-base/bin/package)
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

CMR_O = cmonster
CMR_D = cmonster-g

ALL_PROGS = $(CMR_O) $(CMR_D)

C_FILES = $(GEN_DIR)/cmdline.c rwfn.c tmap.c cmonster.c

all: sanity_check $(ALL_PROGS)

sanity_check:
	@$(LibSanityCheck)

clean:
	$(RM) $(ALL_PROGS) *.o *~ tmp/*

veryclean: clean
	$(RM) cmdline.h cmdline.c

cmonster: $(C_FILES) $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

cmonster-g: $(C_FILES) $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

 # End of rules that are run from the arch directory
endif


