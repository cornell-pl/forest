ifndef SML_BIN_DIR
 export SML_BIN_DIR=sml
endif

ifndef SML
  export SML=$(SML_BIN_DIR)/sml
endif

SML_VER = $(shell $(SML) '@SMLversion' | sed -e 's|.* ||')
SML_MINOR_VER = $(shell echo $(SML_VER) | sed -e 's|.*[.]||')
ifeq ($(SML_VER),$(SML_MINOR_VER))
SML_MINOR_VER = 0
endif
SML_MINOR_VER_TEST=$(shell test $(SML_MINOR_VER) -gt 39 && echo GT39)

ifeq ($(SML_MINOR_VER_TEST),GT39)
BUILD_EXTRACT = build-extract-45.sml
else
BUILD_EXTRACT = build-extract.sml
endif
SHELL=/bin/sh


LEX_SOURCES =		$(wildcard *-lexer)
SML_SOURCES =		$(wildcard *.sml)
SOURCES =		$(SML_SOURCES) $(LEX_SOURCES) sources.cm


all:	extractor

extractor:
	echo Making extractor
	$(SML) < $(BUILD_EXTRACT)

