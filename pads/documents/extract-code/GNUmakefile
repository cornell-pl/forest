
 # This is a GNU makefile

ifndef SML
  export SML=sml
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


all:	extractor

extractor:
	echo Making extractor
	$(SML) < $(BUILD_EXTRACT)


foo:
	@(echo SML = $(SML))
	@(echo SML_VER = $(SML_VER))
	@(echo SML_MINOR_VER = $(SML_MINOR_VER))
	@(echo SML_MINOR_VER_TEST = $(SML_MINOR_VER_TEST))
	@(echo BUILD_EXTRACT = $(BUILD_EXTRACT))


clean:
	rm extract-code.x86-linux
	rm -rf CM
	rm pads-lexer.sml
	rm sml-lexer.sml
