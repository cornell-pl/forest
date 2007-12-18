%-learn:
	@(echo `pwd`)
	@(if [ ! -d $* ]; then mkdir $*; fi)
	$(PADS_HOME)/infer/scripts/learn -d $* -n $* -lex vanilla ../data/$*
	@(echo "Generated PADS description $*.p and supporting files are in `pwd`/$*.")

all:	ai.3000-learn
	@(echo "Making ai.3000 in results/ai.3000")

clean:
	@(echo "Removing generated directories")
	@(dirs=`find * -type d -maxdepth 0`; $(RM) -rf $$dirs)
	@($(RM) *.~)



