######################################################################
# This is a GNU makefile
######################################################################

######################################################################
#
# Infrastructure to produce silver versions
#
######################################################################

%::
	@echo target=$@;
	@echo prerequisites=$^;
	@echo first prerequisite=$<;
	@echo basenames=$(basename $^);
	@echo notdirs=$(notdir $^);
	@if [ ! -d $@ ] ; then mkdir $@; cvs add $@; fi
	@if [ -e $< ] ; then cp $^ $@ ; else cp $^ $@; cd $@; cvs add $(notdir $^); fi

1967Transactions.short: ../output/1967Transactions.short/Ty ../output/1967Transactions.short/Params
MER_T01_01.csv: ../output/MER_T01_01.csv/Ty ../output/MER_T01_01.csv/Params
ProfSec.log: ../output/ProfSec.log/Ty ../output/ProfSec.log/Params
SCESETUP.LOG: ../output/SCESETUP.LOG/Ty ../output/SCESETUP.LOG/Params
access_log: ../output/access_log/Ty ../output/access_log/Params
ai-simple: ../output/ai-simple/Ty ../output/ai-simple/Params
ai.3000: ../output/ai.3000/Ty ../output/ai.3000/Params
apache.txt: ../output/apache.txt/Ty ../output/apache.txt/Params
asl.log: ../output/asl.log/Ty ../output/asl.log/Params
backup1.log: ../output/backup1.log/Ty ../output/backup1.log/Params
backup2.log: ../output/backup2.log/Ty ../output/backup2.log/Params
boot.log: ../output/boot.log/Ty ../output/boot.log/Params
crashreporter.log: ../output/crashreporter.log/Ty ../output/crashreporter.log/Params
daily.out: ../output/daily.out/Ty ../output/daily.out/Params
dibbler.1000: ../output/dibbler.1000/Ty ../output/dibbler.1000/Params
dmesg: ../output/dmesg/Ty ../output/dmesg/Params
error_log: ../output/error_log/Ty ../output/error_log/Params
hp-array: ../output/hp-array/Ty ../output/hp-array/Params
hp-struct: ../output/hp-struct/Ty ../output/hp-struct/Params
ijnltif: ../output/ijnltif/Ty ../output/ijnltif/Params
install.log: ../output/install.log/Ty ../output/install.log/Params
interactions.txt.small: ../output/interactions.txt.small/Ty ../output/interactions.txt.small/Params
interface.loop: ../output/interface.loop/Ty ../output/interface.loop/Params
lsof: ../output/lsof/Ty ../output/lsof/Params
netstat: ../output/netstat/Ty ../output/netstat/Params
netstat-an: ../output/netstat-an/Ty ../output/netstat-an/Params
page_log: ../output/page_log/Ty ../output/page_log/Params
quarterlypersonalincome: ../output/quarterlypersonalincome/Ty ../output/quarterlypersonalincome/Params
railroad.txt: ../output/railroad.txt/Ty ../output/railroad.txt/Params
rpmpkgs: ../output/rpmpkgs/Ty ../output/rpmpkgs/Params
scesetup2.log: ../output/scesetup2.log/Ty ../output/scesetup2.log/Params
scrollkeeper.log: ../output/scrollkeeper.log/Ty ../output/scrollkeeper.log/Params
simplegroups: ../output/simplegroups/Ty ../output/simplegroups/Params
simplexml: ../output/simplexml/Ty ../output/simplexml/Params
windowserver_last.log: ../output/windowserver_last.log/Ty ../output/windowserver_last.log/Params
yum.log: ../output/yum.log/Ty ../output/yum.log/Params
yum.txt: ../output/yum.txt/Ty ../output/yum.txt/Params
