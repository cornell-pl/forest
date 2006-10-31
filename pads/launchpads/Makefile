# Mark Daly
# June 2006
# LaunchPADS Makefile
# Borrowed from wxWidgets Minimal example, modified Bakefile-generated makefile

# required environment variables
#  WXWIDGETS_HOME: top-level wxwidgets directory (i.e. '~/tools/wxMac-2.6.3/')
#  WXWIDGETS_BUILD_DIR: build directory within WXWIDGTS_HOME (i.e. 'carbon' or 'build')

WXHOME=${WXWIDGETS_HOME}
WXBUILD=carbon3dot3/


# environment-specific vars
WX_MAJOR_VERSION=2
WX_MINOR_VERSION=6
WX_VERSION=2.6

PADSLANG_DIR=./padslang
INSPECTOR_DIR=./inspector
TINYXML_DIR = /Users/kfisher/sw/tinyxml
XERCES_INCLUDE = /Users/kfisher/sw/xerces-c-src_2_7_0/include/
#########

prefix = /usr/local
exec_prefix = ${prefix}
INSTALL = /usr/bin/install -c
EXEEXT = 
WINDRES = 
REZ = /Developer/Tools/Rez
SETFILE = /Developer/Tools/SetFile
NM = nm
srcdir = ./
top_srcdir = ./
top_lib_dir = ${WXHOME}/${WXBUILD}/
top_builddir = ${top_lib_dir}
LDFLAGS_GUI = 
CXX = gcc-3.3
CXXFLAGS2 = -I${WXHOME}/include \
-I${WXHOME}/${WXBUILD}/lib/wx/include/mac-ansi-release-static-${WX_VERSION} \
-I${WXHOME}/include \
-I${WXHOME}/contrib/include \
-I./bitmaps \
-I${WXHOME}/art \
-I./${PADSLANG_DIR} \
-D__WXMAC__ -D_FILE_OFFSET_BITS=64 -D_LARGE_FILES -DNO_GCC_PRAGMA \
-I${XERCES_INCLUDE}
WALL = 
CXXFLAGS = -O2 -fno-strict-aliasing ${WALL} -Wundef -Wno-ctor-dtor-privacy -fno-common
CPPFLAGS = -DWX_PRECOMP -I${top_lib_dir}lib/wx/include/mac-ansi-release-static-2.6 -I${top_lib_dir}/include -D_FILE_OFFSET_BITS=64 -D_LARGE_FILES -fpascal-strings -I${top_srcdir}/src/mac/carbon/morefilex -I/Developer/Headers/FlatCarbon ${CXXFLAGS2}
LDFLAGS =   -framework QuickTime -framework IOKit -framework Carbon -framework Cocoa -framework System -framework Xerces
WX_LIB_FLAVOUR = 
TOOLKIT = MAC
TOOLKIT_LOWERCASE = mac
TOOLKIT_VERSION = 
TOOLCHAIN_FULLNAME = mac-ansi-release-static-${WX_VERSION}
EXTRALIBS =   -framework ApplicationServices -framework QuickTime -framework IOKit -framework Carbon -framework Cocoa -framework System  -lz -lpthread -liconv 
FULLLDFLAGS =  -L${WXHOME}/${WXBUILD}/lib/ -lwx_mac-2.6 -lwxexpat-2.6 -lwxtiff-2.6 -lwxpng-2.6 -lwxjpeg-2.6 $(EXTRALIBS) $(LDFLAGS) -lstdc++
#FULLLDFLAGS = `${WXHOME}${WXBUILD}/wx-config --libs` $(EXTRALIBS) $(LDFLAGS)
EXTRALIBS_GUI = 
HOST_SUFFIX = 
SAMPLES_RPATH_FLAG = 
SAMPLES_RPATH_POSTLINK = 

### Variables: ###

DESTDIR = 
WX_RELEASE = 2.6
WX_VERSION = $(WX_RELEASE).3
LIBDIRNAME = $(top_lib_dir)lib
MINIMAL_CXXFLAGS = -D__WX$(TOOLKIT)__ $(__WXUNIV_DEFINE_p) \
	$(__EXCEPTIONS_DEFINE_p) $(__RTTI_DEFINE_p) $(__THREAD_DEFINE_p) \
	-I$(srcdir) $(__DLLFLAG_p) $(CPPFLAGS) $(CXXFLAGS) -I$(TINYXML_DIR)
PADSLANG_OBJECTS = ${PADSLANG_DIR}/PADS_lang.o ${PADSLANG_DIR}/PADS_lang_factory.o ${PADSLANG_DIR}/PADS_lang_helper_classes.o \
 ${PADSLANG_DIR}/PADS_lang_simple_tree.o ${PADSLANG_DIR}/PADS_lang_pnode_tree.o ${PADSLANG_DIR}/PADS_lang_xerces_parser.o \
 ${PADSLANG_DIR}/PADS_lang_unified_tree.o
INSPECTOR_OBJECTS = ${INSPECTOR_DIR}/PADS_inspector.o
TINYXML_OBJECTS = ${TINYXML_DIR}/tinyxml.o ${TINYXML_DIR}/tinystr.o ${TINYXML_DIR}/tinyxmlerror.o ${TINYXML_DIR}/tinyxmlparser.o
MINIMAL_OBJECTS = bottom.o lp_PADS.o mid.o right.o tool_proc.o left.o lp_XML.o shell_d.o tool_proc_event.o \
lp_CodeGen.o main_event.o prefs.o state_seq.o top.o padx_d.o LaunchPADS.o lp_PNode_Container.o left_new_additions.o \
${PADSLANG_OBJECTS} ${INSPECTOR_OBJECTS} ${TINYXML_OBJECTS}
#$(__launchpads___win32rc)
#$(__launchpads_os2_lib_res)


### Conditionally set variables: ###
# this was all done by Bakefile - be careful!

CXXC = $(top_builddir)./bk-deps $(CXX)

PORTNAME = $(TOOLKIT_LOWERCASE)$(TOOLKIT_VERSION)
WXBASEPORT = _carbon

EXTRALIBS_FOR_BASE = $(EXTRALIBS) $(EXTRALIBS_GUI)

EXTRALIBS_FOR_GUI = 
__launchpads___mac_setfilecmd = $(SETFILE) -a C LaunchPADS$(EXEEXT)
__launchpads___mac_rezcmd = $(__MACOSX_RESOURCES_p_1)

COND_PLATFORM_OS2_1___launchpads___os2_emxbindcmd = $(NM) LaunchPADS$(EXEEXT) | if \
	grep -q pmwin.763 ; then emxbind -ep LaunchPADS$(EXEEXT) ; fi

__launchpads_bundle___depname = LaunchPADS_bundle

____launchpads_BUNDLE_TGT_REF_DEP = LaunchPADS.app/Contents/PkgInfo
COND_MONOLITHIC_0___WXLIB_CORE_p = \
	-lwx_$(PORTNAME)$(WXUNIVNAME)$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)_core-$(WX_RELEASE)$(HOST_SUFFIX)

COND_MONOLITHIC_0___WXLIB_BASE_p = \
	-lwx_base$(WXBASEPORT)$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)

COND_MONOLITHIC_1___WXLIB_MONO_p = \
	-lwx_$(PORTNAME)$(WXUNIVNAME)$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
__WXLIB_MONO_p = $(COND_MONOLITHIC_1___WXLIB_MONO_p)
__LIB_TIFF_p = -lwxtiff$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
__LIB_JPEG_p = -lwxjpeg$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
__LIB_PNG_p = -lwxpng$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)

COND_WXUSE_REGEX_BUILTIN___LIB_REGEX_p = -lwxregex$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)

__LIB_EXPAT_p = -lwxexpat$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
COND_TOOLKIT_MAC___MACOSX_RESOURCES_p_1 = $(REZ) -d __DARWIN__ -t APPL -d \
	__WX$(TOOLKIT)__ $(__WXUNIV_DEFINE_p_1) $(__EXCEPTIONS_DEFINE_p_1) \
	$(__RTTI_DEFINE_p_1) $(__THREAD_DEFINE_p_1) -i $(srcdir) $(__DLLFLAG_p_1) -i \
	$(top_lib_dir)/../samples $(__RCDEFDIR_p) -i $(top_lib_dir)/include -o \
	LaunchPADS$(EXEEXT) Carbon.r sample.r
__MACOSX_RESOURCES_p_1 = $(COND_TOOLKIT_MAC___MACOSX_RESOURCES_p_1)

### Targets: ###

.SUFFIXES:	.o .cpp

.cpp.o :
	$(CXX) -c  $(MINIMAL_CXXFLAGS) $(FULLLDFLAGS) -o $@ $<

all: LaunchPADS$(EXEEXT) $(__launchpads_bundle___depname)

install: all

uninstall: 

install-strip: install

clean: 
	rm -rf ./.deps ./.pch
	rm -f ./*.o
	rm ${PADSLANG_DIR}/*.o
	rm ${INSPECTOR_DIR}/*.o
	rm -f LaunchPADSl$(EXEEXT)
	rm -rf LaunchPADS.app

#distclean: clean
#	rm -f config.cache config.log config.status bk-deps bk-make-pch shared-ld-sh

#LaunchPADSApp$(EXEEXT): $(MINIMAL_OBJECTS) $(__launchpads___win32rc)
#	$(CXX) -o $@ $(MINIMAL_OBJECTS) $(LDFLAGS)   -L$(LIBDIRNAME) $(LDFLAGS_GUI) $(SAMPLES_RPATH_FLAG)  $(__WXLIB_CORE_p)  $(__WXLIB_BASE_p)  $(__WXLIB_MONO_p) $(__LI#B_TIFF_p) $(__LIB_JPEG_p) $(__LIB_PNG_p) $(__LIB_ZLIB_p) $(__LIB_ODBC_p) $(__LIB_REGEX_p) $(__LIB_EXPAT_p) $(EXTRALIBS_FOR_BASE) $(EXTRALIBS_FOR_GUI)
#	$(__launchpads___mac_rezcmd)
#	$(__launchpads___mac_setfilecmd)
#	$(__launchpads___os2_emxbindcmd)
#	$(SAMPLES_RPATH_POSTLINK)

LaunchPADS: $(MINIMAL_OBJECTS)
	$(CXX) -o $@ $(MINIMAL_OBJECTS) $(FULLLDFLAGS)
	$(__launchpads___mac_rezcmd)
	$(__launchpads___mac_setfilecmd)
	$(__launchpads___os2_emxbindcmd)
	$(SAMPLES_RPATH_POSTLINK)

# $(MINIMAL_CXXFLAGS) 
#LaunchPADS.o: $(srcdir)/LaunchPADS.cpp
#	$(CXXC) -c -o $@ $(MINIMAL_CXXFLAGS) $(srcdir)/LaunchPADS.cpp

LaunchPADS.app/Contents/PkgInfo: LaunchPADS $(top_lib_dir)/../src/mac/carbon/Info.plist.in $(top_lib_dir)/../src/mac/carbon/wxmac.icns
	mkdir -p LaunchPADS.app/Contents
	mkdir -p LaunchPADS.app/Contents/MacOS
	mkdir -p LaunchPADS.app/Contents/Resources

	sed -e "s/IDENTIFIER/`echo LaunchPADS | sed -e 's,\.\./,,g' | sed -e 's,/,.,g'`/" \
	-e "s/EXECUTABLE/LaunchPADS/" \
	-e "s/VERSION/$(WX_VERSION)/" \
	$(top_lib_dir)/../src/mac/carbon/Info.plist.in >LaunchPADS.app/Contents/Info.plist
	
	
	echo -n "APPL????" >LaunchPADS.app/Contents/PkgInfo
	
	
	ln -f LaunchPADS$(EXEEXT) LaunchPADS.app/Contents/MacOS/LaunchPADS
	
# use wxWidgets default icon, since it looks nicer than the generic icon
	cp -f $(top_lib_dir)/../src/mac/carbon/wxmac.icns LaunchPADS.app/Contents/Resources/wxmac.icns

LaunchPADS_bundle: $(____launchpads_BUNDLE_TGT_REF_DEP)

#LaunchPADS_rc.o: $(top_lib_dir)/../../samples/sample.rc
#	$(WINDRES) -i$< -o$@   --define __WX$(TOOLKIT)__ $(__WXUNIV_DEFINE_p_2)  $(__EXCEPTIONS_DEFINE_p_2) $(__RTTI_DEFINE_p_2) $(__THREAD_DEFINE_p_2)   --include-dir $(srcdir) $(__DLLFLAG_p_2) --include-dir $(top_lib_dir)/samples $(__RCDEFDIR_p_1) --include-dir $(top_srcdir)/include


# Include dependency info, if present:
-include .deps/*.d

.PHONY: all install uninstall clean distclean minimal_bundle
