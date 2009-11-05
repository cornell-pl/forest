ifndef LEARN_HOME
%:	forceabort
	@echo "ERROR: environment variable LEARN_HOME must contain path to root of PADS inference system."
	@exit 1
forceabort: ;
endif

%-learn:
	@(echo `pwd`)
	@(if [ ! -d $* ]; then mkdir $*; fi)
	$(LEARN_HOME)/scripts/learn -d $* -n $* -lex vanilla -u ../data/$*
	@(echo "Generated PADS description $*.p and supporting files are in `pwd`/$*.")

example: ai.3000-learn
	@(echo "Making ai.3000 in results/ai.3000")

clean:
	@(echo "Removing generated directories")
	@(dirs=`find * -type d -maxdepth 0`; $(RM) -rf $$dirs)
	@($(RM) *.~)

default: example


