
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
P_DIR = ../../p
GEN_DIR = ../../gen
CPY_DIR = ../../copybook
GENPROG = $(PADS_HOME)/scripts/cb2pads.ksh
# GENPROG = $(PADS_HOME)/scripts/comp_cpy.ksh

include $(PADS_HOME)/mk/rules.mk

VPATH =  $(CPY_DIR)

all: $(P_DIR)/cpygen_foo.p $(P_DIR)/cpygen_crs.p

$(P_DIR)/cpygen_crs.p: $(P_DIR)/cpygen_crshdr.p $(P_DIR)/cpygen_crstlr.p $(P_DIR)/cpygen_crsdet.p
	(echo ""; echo "extern PDC_uint64 num_recs;"; echo "") > $@
	cat $^ | sed -e 's|Pstruct cpy_crshdr|Precord Pstruct cpy_crshdr|' \
		     -e 's|XX_TOTAL_RECORDS_3;|XX_TOTAL_RECORDS_3 : XX_TOTAL_RECORDS_3 == num_recs ;|' >> $@
	(echo "Precord Punion det_or_tlr {"; \
	 echo "	cpy_crstlr                                         is_tlr;"; \
	 echo "	cpy_crsdet                                         is_det;"; \
	 echo "};"; echo ""; \
	 echo "Parray det_tlr_array {"; \
	 echo "	det_or_tlr [];"; \
	 echo "};"; echo ""; \
	 echo "Pfile Pstruct crs_file {"; \
	 echo "	cpy_crshdr                                         is_hdr;"; \
	 echo "	det_tlr_array                                      is_rest;"; \
	 echo "};"; echo ""; ) >> $@

$(P_DIR)/cpygen_crsdet.p: crsdet.cpy
	@(LABEL=$(patsubst %.cpy,%,$(notdir $<)); \
	echo $(GENPROG) $< $$LABEL $(P_DIR) 11,12,15,24; \
	$(GENPROG) $< $$LABEL $(P_DIR) 11,12,15,24; \
	echo $(RM) $(P_DIR)/c_$$LABEL.c $(P_DIR)/p_$$LABEL.c $(P_DIR)/cpy_$$LABEL.c $(P_DIR)/cpy_$$LABEL.h)

$(P_DIR)/cpygen_%.p: %.cpy
	@(LABEL=$(patsubst %.cpy,%,$(notdir $<)); \
	echo $(GENPROG) $< $$LABEL $(P_DIR); \
	$(GENPROG) $< $$LABEL $(P_DIR); \
	$(RM) $(P_DIR)/c_$$LABEL.c $(P_DIR)/p_$$LABEL.c $(P_DIR)/cpy_$$LABEL.c $(P_DIR)/cpy_$$LABEL.h)

sanity_check:
	@$(SanityCheck)

clean:
	$(RM) $(P_DIR)/cpygen_*.p

 # End of rules that are run from the arch directory
endif
