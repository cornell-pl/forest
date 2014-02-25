 # This makefile uses gmake extensions
 # such as $(shell ...) to execute a command

.PRECIOUS: %.c

# These variables need to be set
ifndef PADS_HOME
  PADS_HOME=/home/kfisher/pads
endif

# These variables should not need to change
PADSC=$(PADS_HOME)/scripts/padsc
AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
INSTALLROOT=$(PADS_HOME)/ast-ast/arch/$(AST_ARCH)
LIBDIR=$(INSTALLROOT)/lib
LIBS = $(LIBDIR)/libpads.a $(LIBDIR)/libast.a
INCLUDES = -I$(PADS_HOME)/padsc/include -I$(INSTALLROOT)/include/ast -I.

# Rule to run the pads compiler on adderley.p
adderley.c: adderley.p
	$(PADSC)  -D__ppc__ $<
#	$(PADSC) -wnone -D__ppc__ $<

short.c: short.p
	$(PADSC)  -D__ppc__ $<

adderley2.c: adderley2.p
	$(PADSC)  -D__ppc__ $<

include $(PADS_HOME)/mk/rules.mk

%.c: %.p
	$(PADSC) $<

# Rules to make programs
test_adderley: test_adderley.c adderley.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_adderley2: test_adderley2.c adderley2.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_adderley2_d: test_adderley2.c adderley2.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_short: test_short.c short.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_short_d: test_short.c short.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

rwxml_adderley: rwxml_adderley.c adderley.c read_orig_write_xml_trailer.h $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

rwxml_adderley_d: rwxml_adderley.c adderley.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_adderley_d: test_adderley.c adderley.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_adderley_dd: adderley_expanded.c test_adderley.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

adderley_expanded.c: adderley.c
	$(MKSRC_D) $< | $(PADS_HOME)/scripts/addnl.pl | $(PADS_HOME)/scripts/fix_FILE_LINE.pl $< > $@

# strange core dump case
test_core_d: test_core.c adderley.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

clean:
	$(RM) adderley.[ch] adderley.xsd test_adderley test_adderley_d test_adderley_dd adderley_expanded.c
