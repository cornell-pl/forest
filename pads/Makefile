#########################################################################
#                                                                       #
#                                PADS                                   #
#                   Processing Ad Hoc Data Engine                       #
#                                                                       #
#   Copyright 2006, AT&T Labs Research                                  #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

#########################################################################
# Author:	Pamela L. Dragosh (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for PADS that relies on the output
#		of a configuration script.
#
# History:
#	
#########################################################################

#########################################################################
# Section:	Default target
# Description:
#		The default target for this Makefile is "all"
#########################################################################
default:	all

#########################################################################
# Section:	Makefile pre-includes
# Description:
#		This is where the file(s) generated during by the Configure script
#		are included.  If config/Makefile.conf does not exist, the
#		make will fail.
#
#		Makefile.pads:	Variables defined for compiling and linking PADS
#				applications in the build environment
#########################################################################

include $(PADS_SRC)/config/Makefile.pads

#########################################################################
# Section:	Main targets
# Description:
#		This is where all of the named toplevel targets are placed.
#
#	world:		Force updating of .depend files and make all
#	all:		This target builds the PADS compiler
#	install::	This target does installation for files in the main section
#	uninstall::	This target uninstalls files installed in the main section
#	clean::		This target cleans files created in the all section
#	clobber::	This target cleans files created in the all section and .depend
#	
#########################################################################
world:
	$(MAKE) -f Makefile all DEPDEPEND=1

all: ast_package sml_package libs compiler regress

install:: all $(CONF_PADS_CONFIG) $(CONF_PADS_MAN)
	$(CP) config/Makefile.conf $(CONF_PADS_CONFIG)
	$(SED) -e "s:CONF_PADS_CONFIG:$(CONF_PADS_CONFIG):" config/Makefile.pads-inst > $(CONF_PADS_CONFIG)/Makefile.pads
	$(CP) LICENSE $(CONF_GALAX_MAN)
	$(CP) README $(CONF_GALAX_MAN)

uninstall::
	$(RM) $(CONF_PADS_CONFIG)/Makefile.conf
	$(RM) $(CONF_PADS_CONFIG)/Makefile.pads
	$(RM) $(CONF_PADS_MAN)/LICENSE
	$(RM) $(CONF_PADS_MAN)/README

#########################################################################
# Sub-Section:	Secondary targets
# Description:
#		These are the targets listed as dependencies of "all".  They each
#		have their own install, uninstall, and clean targets.
#
#		libs:			Standard PADS libraries
#		ast_package:	The AST library
#		sml_package:	The SML Library
#		compiler:		PADS compiler
#		regression:		The regression tests
#########################################################################

#########################################################################
# Sub-Sub-Section:	ast_package
# Description:
#		This is for the AST library that PADS uses. Optionally built.
#
#		ast:			Builds the AST library targets (static and optional shared)
#		clean:			Cleans up the files used to create the libraries that
#						are created by automatic rules
#########################################################################

#
# It would be nice if AST had a generic link to the latest release. For now we will have to
# hard code in the versions of INIT and AST that we wish to download.
#
AST_AUTHORIZE="I accept www.opensource.org/licenses/cpl"
AST_PASSWORD=.
AST_PACKAGE=bin/package
AST_INIT=lib/package/tgz/init.tgz
AST_AST=lib/package/tgz/ast.tgz
LATEST_INIT=INIT.2006-02-14.tgz
LATEST_AST=ast-ast.2006-02-14.tgz

PACKAGEURL=http://silk.research.att.com/~gsf/download/package
INITURL=http://www.research.att.com/~gsf/download/tgz/$(LATEST_INIT)
ASTURL=http://www.research.att.com/~gsf/download/tgz/$(LATEST_AST)

#
# This figures out the full path to the AST library, using AST's package script to determine
# the operating system/architecture name.
#
ifeq ($(CONF_BUILD_AST), true)
	#
	# Here we are building AST
	#
	AST_LIB=ast-ast/arch/$(shell ast-ast/$(AST_PACKAGE))/lib/libast.a
else
	#
	# Here we are using a pre-built version of AST
	#
	AST_LIB=$(CONF_AST_HOME)/arch/$(shell $(CONF_AST_BIN)/package)/lib/libast.a
endif

ifeq ($(CONF_BUILD_AST), true)
#
# In this case, the user wants to build the AST library. Here we will pull down the needed
# .tgz files as well as the package script. Untar them and build them. IF, you have a specific
# version of AST you want to use, you could simply store them in the ast-ast/lib/package/tgz
# directory and rename them as init.tgz and ast.tgz. Or, you could set CONF_BUILD_AST to false
# and then use the configure script to point to the location of the AST library you wish to use.
#
#ast: ast-ast/bin/package
	#
	# It would be nice if this would work, but although it detects the dependencies, it ends up failing
	# to download them.
	#@(cd ast-ast; bin/package setup source authorize "I accept www.opensource.org/licenses/cpl" password . http://www.research.att.com/~gsf/download ast-ast
	#
ast_package: ast_dirs ast-ast/$(AST_PACKAGE) ast-ast/$(AST_INIT) ast-ast/$(AST_AST)
	@(cd ast-ast; if [ ! -s src/cmd/INIT/LICENSE ]; then (gunzip -c $(AST_INIT) | tar -xv); fi;)
	@(cd ast-ast; if [ ! -s src/lib/libast/README ]; then (gunzip -c $(AST_AST) | tar -xv;); fi;)
	@(if [ ! -s $(AST_LIB) ]; then (cd ast-ast; $(AST_PACKAGE) make SHELL=$(SHELL)); fi)
	@(if [ ! -s $(AST_LIB) ]; then (echo "Could NOT find $(AST_LIB). Perhaps the build failed?"); fi)
else
#
# No build of AST is wanted. Here we will simply check that we can find the library according to the
# settings that were set during the configuration.
#
ast_package:
	@(if [ ! -s $(AST_LIB) ]; then (echo "$(AST_LIB) NOT Found. Check your configure settings."); fi)
endif

ifeq ($(CONF_BUILD_AST), true)
#
# Just delete the compilation output
#
veryveryclean:: ast-ast/$(AST_PACKAGE)
	@($(RM) -fr ast-ast/arch/$(shell $(CONF_AST_BIN)/package))
endif
	
ifeq ($(CONF_BUILD_AST), true)
#
# Deleting these will force re-downloading of the INIT/AST source code.
#
clobber:: veryveryclean
	@($(RM) -f ast-ast/$(AST_INIT))
	@($(RM) -f ast-ast/$(AST_AST))
	@(cd ast-ast; $(RM) -rf src)
	@(cd ast-ast; $(RM) -f $(AST_PACKAGE))
endif

#
# Make sure the directories exist
#
ast_dirs:
	@(if [ ! -d $(CONF_AST_HOME) ]; then ($(MKDIR) $(CONF_AST_HOME)); fi)
	@(if [ ! -d $(CONF_AST_BIN) ]; then ($(MKDIR) $(CONF_AST_BIN)); fi)
	@(if [ ! -d $(CONF_AST_LIB) ]; then ($(MKDIR) $(CONF_AST_LIB)); fi)
	@(if [ ! -d $(CONF_AST_LIB)/package ]; then ($(MKDIR) $(CONF_AST_LIB)/package); fi)
	@(if [ ! -d $(CONF_AST_LIB)/package/tgz ]; then ($(MKDIR) $(CONF_AST_LIB)/package/tgz); fi)

#
# Pull down the latest AST source code release. Easiest to just get the AST library and also INIT (AST
# is dependent on INIT). Also pull down the latest package script.
#
ast-ast/$(AST_PACKAGE):
	@(if [ ! -s $@ ]; then ( \
		if curl $(PACKAGEURL) >$@ || wget -O $@ $(PACKAGEURL); then ( \
			chmod +x $@; \
		); fi; \
		if [ ! -s $@ ]; then ( \
			echo "Failed to retrieve $@"; \
		); fi; \
	); fi)

ast-ast/$(AST_INIT):
	@(if [ ! -s $@ ]; then ( \
		if curl -u $(AST_AUTHORIZE):$(AST_PASSWORD) $(INITURL) >$@ ||  wget --http-user=$(AST_AUTHORIZE) --http-password=$(AST_PASSWORD) -O $@ $(INITURL); then ( \
			echo "Retrieved $@"; \
		) else ( \
			$(RM) -f $@; \
			echo "Failed to retrieve $@"; \
		); fi; \
	); fi)

ast-ast/$(AST_AST):
	@(if [ ! -s $@ ]; then ( \
		if curl -u $(AST_AUTHORIZE):$(AST_PASSWORD) $(ASTURL) >$@ || wget --http-user=$(AST_AUTHORIZE) --http-password=$(AST_PASSWORD) -O $@ $(ASTURL); then ( \
			echo "Retrieved $@"; \
		) else ( \
			$(RM) -f $@; \
			echo "Failed to retrieve $@"; \
		); fi; \
	); fi)

#########################################################################
# Sub-Sub-Section:	sml_package
# Description:
#		This is for the SML library that PADS uses. Optionally built.
#
#		sml:			Installs and builds the SML library
#		clean:			Cleans up the files used to create the libraries that
#						are created by automatic rules
#########################################################################

ifeq ($(CONF_BUILD_SML), true)
#
# Here we are going to try to download and install SML
#
SML_CONFIG_URL=http://smlnj.cs.uchicago.edu/dist/working/110.59/config.tgz


sml_package: $(CONF_SML_HOME)/bin/sml
		
$(CONF_SML_HOME)/bin/sml: $(CONF_SML_HOME)/config/install.sh
	@(echo "Building SML")
	@(cd sml; config/install.sh;)

$(CONF_SML_HOME)/config/install.sh: $(CONF_SML_HOME)/config.tgz
	@(echo "Unpacking config.tgz")
	@(cd sml; if [ ! -s config/install.sh ]; then (gunzip -c config.tgz | tar -xv); fi;)

$(CONF_SML_HOME)/config.tgz: $(CONF_SML_HOME)
	@(if [ ! -s $@ ]; then ( \
		echo "Retrieving $@"; \
		if curl $(SML_CONFIG_URL) >$@ ||  wget -O $@ $(SML_CONFIG_URL); then ( \
			echo "Retrieved $@"; \
		) else ( \
			$(RM) -f $@; \
			echo "Failed to retrieve $@"; \
		); fi; \
	) else ( \
		echo "config.tgz already exists"; \
	); fi)

$(CONF_SML_HOME):
	@(if [ ! -d $(CONF_SML_HOME) ]; then (echo "Creating $(CONF_SML_HOME)"; $(MKDIR) $(CONF_SML_HOME)); fi)

veryclean::
	@($(RM) $(CONF_SML_HOME)/bin/sml)

veryveryclean::
	@($(RM) $(CONF_SML_HOME)/config.tgz)

clobber::
	@($(DELTREE) $(CONF_SML_HOME))

else
#
# Here we are using a pre-built version of SML, we just verify it exists.
#
sml_package: $(CONF_SML_HOME)/bin/sml

endif

#########################################################################
# Sub-Sub-Section:	libs
# Description:
#		These are the targets for building libraries, installing them
#		uninstalling them, and cleaning them.
#
#		libs:			Builds the library targets (static and optional shared)
#		install:		Installs the library targets (static and optional shared)
#		uninstall:		Uninstalls the library targets (static and optional shared)
#		clean:			Cleans up the files used to create the libraries that
#						are created by automatic rules
#########################################################################

libs: ast_package sml_package
	@(cd padsc/libpads; $(MAKE) -f Makefile)

updatedepend::
	@(cd padsc/libpads; $(MAKE) -f Makefile updatedepend)

install:: libs 
	@(cd padsc/libpads; $(MAKE) -f Makefile install)

uninstall::
	@(cd padsc/libpads; $(MAKE) -f Makefile uninstall)

clean::
	@(cd padsc/libpads; $(MAKE) -f Makefile clean)

veryclean::
	@(cd padsc/libpads; $(MAKE) -f Makefile veryclean)

veryveryclean::
	@(cd padsc/libpads; $(MAKE) -f Makefile veryveryclean)

clobber::
	@(cd padsc/libpads; $(MAKE) -f Makefile clobber)


#########################################################################
# Sub-Sub-Section:	compiler
# Description:
#		These are the targets for building the toplevel applications
#
#		install:				Installs the compiler targets
#		uninstall:				Uninstalls the compiler targets
#		clean:					Clean is delegated to each of the actual commands
#########################################################################

compiler:	libs
	@(cd padsc/comp; $(MAKE) -f Makefile)

updatedepend::
	@(cd padsc/comp; $(MAKE) -f Makefile updatedepend)

install::	compiler $(CONF_PADS_BIN)
	@(cd padsc/comp; $(MAKE) -f Makefile install)

uninstall::
	@(cd padsc/comp; $(MAKE) -f Makefile uninstall)

clean::
	@(cd padsc/comp; $(MAKE) -f Makefile clean)

veryclean::
	@(cd padsc/comp; $(MAKE) -f Makefile veryclean)

veryveryclean::
	@(cd padsc/comp; $(MAKE) -f Makefile veryveryclean)

clobber::
	@(cd padsc/comp; $(MAKE) -f Makefile clobber)

#########################################################################
# Sub-Sub-Section:	regress
# Description:
#		These are the targets for building the testing applications
#
#		clean:					Clean is delegated to each of the actual commands
#########################################################################

regress:	compiler
	@(cd padsc/libpads/tests; $(MAKE) -f Makefile regress) | $(PADS_SRC)/scripts/count_differs.pl

updatedepend::
	@(cd padsc/libpads/tests; $(MAKE) -f Makefile updatedepend)

clean::
	@(cd padsc/libpads/tests; $(MAKE) -f Makefile clean)

veryclean::
	@(cd padsc/libpads/tests; $(MAKE) -f Makefile veryclean)

veryveryclean::
	@(cd padsc/libpads/tests; $(MAKE) -f Makefile veryveryclean)

clobber::
	@(cd padsc/libpads/tests; $(MAKE) -f Makefile clobber)

#########################################################################
# Section:	Makefile post-includes
# Description:
#		This is where the dependency file is included and generated
#		We place the dependencies to make sure that various files
#		are generated before we need them.
#########################################################################

