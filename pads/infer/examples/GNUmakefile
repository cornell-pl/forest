%-learn:
	@(echo `pwd`)
	@(if [ ! -d results ]; then mkdir results; fi)
	@(if [ ! -d results/$* ]; then mkdir results/$*; fi)
	$(PADS_HOME)/infer/scripts/learn -d results/$* -n $* -lex vanilla data/$*

all:	ai.3000-learn

clean:
	cd results; $(MAKE) clean


