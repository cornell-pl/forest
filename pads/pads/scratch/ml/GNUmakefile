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
	$(PADSC) -I. $<

# Rules to make programs
test_ml: test_ml.c ml.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_ml_d: test_ml.c ml.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)


rwxml_ml: rwxml_ml.c ml.c $(INCLUDE_DEPS) $(LIB_DEPS_O) 
	@$(CCExec_DYNAMIC_O)

rwxml_ml_d: rwxml_ml.c ml.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

clean:
	$(RM) ml.[ch] ml.xsd ml.pxml test_ml test_ml_d test_ml_dd ml_expanded.c rwxml_ml rwxml_ml_d
