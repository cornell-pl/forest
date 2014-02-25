 # This makefile uses gmake extensions
 # such as $(shell ...) to execute a command

.PRECIOUS: %.c

# These variables need to be set
ifndef PADS_HOME
  PADS_HOME=/home/kfisher/pads
endif

include $(PADS_HOME)/mk/rules.mk
# Rule to run the pads compiler on foo.p
%.c: %.p
	$(PADSC) $<

all:	example-xml example-xml-g example-xml-shared example-xml-shared-g

opra-accum: opra-accum.c opra.c $(STATIC_LIBS_O)
	$(COMPILE_O) $^ -o $@

opra-accum-g: opra-accum.c opra.c $(STATIC_LIBS_D)
	$(COMPILE_D) $^ -o $@

opra-accum-shared: opra-accum.c opra.c $(LIB_DEPS_O)
	      @$(CCExec_DYNAMIC_O) 

opra-accum-shared-g: opra-accum.c opra.c $(LIB_DEPS_D)
	      @$(CCExec_DYNAMIC_D) 

opra2-accum-shared: opra2-accum.c opra2.c $(LIB_DEPS_O)
	      @$(CCExec_DYNAMIC_O) 

opra2-accum-shared-g: opra2-accum.c opra2.c $(LIB_DEPS_D)
	      @$(CCExec_DYNAMIC_D) 

test-accum-shared: test-accum.c test.c $(LIB_DEPS_O)
	      @$(CCExec_DYNAMIC_O) 

test-accum-shared-g: test-accum.c test.c $(LIB_DEPS_D)
	      @$(CCExec_DYNAMIC_D) 

opra-fmt: opra-fmt.c opra.c $(STATIC_LIBS_O)
	$(COMPILE_O) $^ -o $@

opra-fmt-g: opra-fmt.c opra.c $(STATIC_LIBS_D)
	$(COMPILE_D) $^ -o $@

opra-fmt-shared: opra-fmt.c opra.c $(LIB_DEPS_O)
	      @$(CCExec_DYNAMIC_O) 

opra-fmt-shared-g: opra-fmt.c opra.c $(LIB_DEPS_D)
	      @$(CCExec_DYNAMIC_D) 

opra-sum: opra-sum.c opra.c $(STATIC_LIBS_O)
	$(COMPILE_O) $^ -o $@

opra-sum-g: opra-sum.c opra.c $(STATIC_LIBS_D)
	$(COMPILE_D) $^ -o $@

opra-sum-shared: opra-sum.c opra.c $(LIB_DEPS_O)
	      @$(CCExec_DYNAMIC_O) 

opra-sum-shared-g: opra-sum.c opra.c $(LIB_DEPS_D)
	      @$(CCExec_DYNAMIC_D) 

clean:
	$(RM) opra.[ch] opra.xsd
	$(RM) opra-xml opra-xml-g opra-xml-shared opra-xml-shared-g
	$(RM) opra-accum opra-accum-g opra-accum-shared opra-accum-shared-g
	$(RM) opra-fmt opra-fmt-g opra-fmt-shared opra-fmt-shared-g
	$(RM) opra-sum opra-sum-g opra-sum-shared opra-sum-shared-g

