PADSC = padsc
GEN_DIR = ../../gen
OUT_DIR = .
OPTS = -x -r $(OUT_DIR) 
PARSE_OPT = -parse
AST_OPT = -ast
INCL_DIRS = -I. -I..
DEFS = -DGALAXCOMPILE # -DOLD_PGLX

.PHONY: all parse ast install

all: install

enum.c: enum.p
	$(PADSC) enum.p $(OPTS) $(INCL_DIRS) $(DEFS)

parse: enum.p
	$(PADSC) enum.p $(PARSE_OPT) $(OPTS) $(INCL_DIRS) $(DEFS)

ast: enum.p
	$(PADSC) enum.p $(AST_OPT) $(OPTS) $(INCL_DIRS) $(DEFS)

$(GEN_DIR)/enum.c: enum.c
	cp enum.c $(GEN_DIR)

$(GEN_DIR)/enum.h: enum.h
	cp enum.h $(GEN_DIR)

install: $(GEN_DIR)/enum.c $(GEN_DIR)/enum.h

