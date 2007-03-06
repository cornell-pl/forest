######################################################################
# This is a GNU makefile
######################################################################

######################################################################
#
# Infrastructure to test against a silver version
#
######################################################################
maxdepth-simplegroups = 50

%::
	cd ../output; make $@
	@if ! cmp --quiet ../output/yum.txt/Ty ../silver/yum.txt/Ty ; \
         then echo $@/Ty has changed since the silver version       ; \
              cmp ../output/yum.txt/Ty ../silver/yum.txt/Ty         ; \
         fi
	@if ! cmp --quiet ../output/yum.txt/Params ../silver/yum.txt/Params  ;\
         then echo $@/Params has changed since the silver version       ;\
              cmp ../output/yum.txt/Params ../silver/yum.txt/Params     ; \
         fi
	@if cmp --quiet ../output/yum.txt/Params ../silver/yum.txt/Params && \
            cmp --quiet ../output/yum.txt/Ty ../silver/yum.txt/Ty           ;\
         then echo No changes for test case $@ since the silver version     ;\
         fi
