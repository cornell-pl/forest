# Mark Daly
# June 2006
# PADSLANG OSX makefile
# requires $WXWIDGETS_HOME be set in your environment

XERCES_INCLUDE = /Users/kfisher/sw/xerces-c-src_2_7_0/include/
FLAGS=-DMACOSX -D_REENTRANT -DNDEBUG  -DAPP_NO_THREADS -DXML_USE_NO_THREADS 
INCLUDEPATHS=-I${WXWIDGETS_HOME}/include -I. -I${XERCES_INCLUDE}
LDFLAGS = -framework Xerces

COMMONOBJECTS = PADS_lang.o PADS_lang_factory.o PADS_lang_helper_classes.o PADS_lang_simple_tree.o \
PADS_lang_pnode_tree.o PADS_lang_xerces_parser.o PADS_lang_unified_tree.o
PADSPARSEROBJECTS = pads_xml_object_test.o ${COMMONOBJECTS}
TARGETS = PADSPARSER
EXECUTABLES = pads_lang_parser

.SUFFIXES:	.o .cpp

.cpp.o :
	g++ ${FLAGS} -c ${INCLUDEPATHS} -o $@ $<

all:   ${TARGETS}

PADSPARSER: ${PADSPARSEROBJECTS}
	g++ ${FLAGS} ${LDFLAGS} -o pads_lang_parser ${PADSPARSEROBJECTS}

clean:
	rm *.o ${EXECUTABLES}


