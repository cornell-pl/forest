 # This makefile uses gmake extensions
 # such as $(shell ...) to execute a command

.PRECIOUS: %.c

# These variables need to be set
ifndef PADS_HOME
  PADS_HOME=/home/kfisher/pads
endif

# These variables should not need to change
PADSC=$(PADS_HOME)/scripts/padsc
include $(PADS_HOME)/mk/rules.mk

%.c: %.p
	$(PADSC) -I. -anone $<

# Rules to make programs
test_hi: test_hi.c hi.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_hi_d: test_hi.c hi.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)


rwxml_hi: rwxml_hi.c hi.c $(INCLUDE_DEPS) $(LIB_DEPS_O) 
	@$(CCExec_DYNAMIC_O)

rwxml_hi_d: rwxml_hi.c hi.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

rwxml_iface: rwxml_iface.c iface.c $(INCLUDE_DEPS) $(LIB_DEPS_O) 
	@$(CCExec_DYNAMIC_O)

rwxml_iface_d: rwxml_iface.c iface.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

clean:
	$(RM) hi.[ch] hi.xsd hi.pxml test_hi test_hi_d test_hi_dd hi_expanded.c rwxml_hi rwxml_hi_d
