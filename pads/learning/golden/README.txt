Suggested Order:

rpmpkgs.txt	3 hrs
quarterlypersonalincome.txt 2 days
yum.txt 5 hrs
boot.txt 1 hr
convert.log 12 hrs
ls-l.txt 1 hr
1967Transactions.short 4 hrs
ai.3000 1 hr
ai.big    -- same format as ai.3000 but longer
dibbler.1000 0.5 hr
dibbler.10001  -- same format as dibbler.1000 but longer
crashreporter.log 2 hr
railroad.txt	2 hr

ai.3000
boot.txt
crashreporter.log
dibbler.1000
ls-1.txt
railroad.txt
rpmpkgs.txt
quarterly...
yum.txt
1967Transactions.short

Current status 5/6
******************
Errors
------
ai.3000 1 hr
size: 293460
ai.p:  good vals:       3000    bad vals:          0    pcnt-bad:    0.000
number of correctly parsed records = 2673
number of incorrectly parsed records = 327
New complexity scores
{ TC = 970.860b, ADC = 889.349b, DC = 2660405.488b } normalized by 2347680 is 1.13361972168

ai.golden: 63 records incorrectly parsed. 97.9 correct
{ TC = 970.860b, ADC = 889.349b, DC = 2660405.488b } normalized by 2347680 is 1.13361972168
====== Timing information ======
Tokenization time = 3.052
Measure1 time = 0.200
Reduce1 time = 21.373
Reduce2 time = 17.455
Reduce3 time = 3.780
Measure2 time = 0.170
Total time = 46.029
================================

excel: good
doesn't dive into data inside quotations

boot.txt 1 hr
handwritten: boot.p good vals:        261    bad vals:          1    pcnt-bad:    0.382
number of correctly parsed records = 243
number of incorrectly parsed records = 19
{ TC = 2767.071b, ADC = 585.658b, DC = 149874.672b } normalized by 129920 is 1.17489026711
golden format
7 good, 2.7% good
XXX Golden complexity =
{ TC = 315.732b, ADC = nanb, DC = 2840.215b } normalized by 129920 is 0.02429146743

====== Timing information ======
Tokenization time = 0.116
Measure1 time = 0.008
Reduce1 time = 1.522
Reduce2 time = 6.850
Reduce3 time = 0.346
Measure2 time = 0.008
Total time = 8.849
================================
excel: fair, doesn't dive into semi-structured data at end of each record.

crashreporter.log 2 hr
handwritten: good vals:        441    bad vals:          0    pcnt-bad:    0.000
{ TC = 2069.123b, ADC = 736.908b, DC = 321536.070b } normalized by 401216 is 0.806561036533
number of correctly parsed records = 441
number of incorrectly parsed records = 0
golden
12 not parsed correctly. 97.3% correct
Golden complexity =
{ TC = 1490.853b, ADC = nanb, DC = 319271.113b } normalized by 401216 is 0.799474510304

====== Timing information ======
Tokenization time = 0.254
Measure1 time = 0.013
Reduce1 time = 2.122
Reduce2 time = 4.408
Reduce3 time = 0.432
Measure2 time = 0.015
Total time = 7.244
================================

excel: fair, doesn't dive into semi-structured data at end of each record.

dibbler.1000 0.5 hr
dibbler.p: good vals:        998    bad vals:          1    pcnt-bad:    0.100
New complexity scores
{ TC = 85.251b, ADC = 28.703b, DC = 535657.700b } normalized by 1140856 is 0.46959734698
number of correctly parsed records = 999
number of incorrectly parsed records = 0
Golden complexity =
{ TC = 325.150b, ADC = 351.792b, DC = 545463.146b } normalized by 1140856 is 0.478402441579
100% golden populated correctly


====== Timing information ======
Tokenization time = 3.870
Measure1 time = 0.062
Reduce1 time = 3.764
Reduce2 time = 162.116
Reduce3 time = 1.336
Measure2 time = 0.036
Total time = 171.184
================================
excel: weak as data is not tabular

ls-l.txt 1 hr
ls-l.txt.p good vals:         34    bad vals:          1    pcnt-bad:    2.857
New complexity scores
{ TC = 717.651b, ADC = 269.960b, DC = 9120.681b } normalized by 16112 is 0.610621405122
number of correctly parsed records = 35
number of incorrectly parsed records = 0
Golden
 0% parsed correctly; golden score meaningless

====== Timing information ======
Tokenization time = 0.131
Measure1 time = 0.004
Reduce1 time = 0.088
Reduce2 time = 0.094
Reduce3 time = 0.017
Measure2 time = 0.004
Total time = 0.338
================================
excel: data not available

railroad.txt	2 hr
{ TC = 2326.724b, ADC = 300.366b, DC = 23142.838b } normalized by 49744 is 0.512012740177
Union tag: good vals:         67    bad vals:          0    pcnt-bad:    0.000
number of correctly parsed records = 6
number of incorrectly parsed records = 61
golden:
64/67 good, 95.5% good
Golden complexity =
{ TC = 1982.868b, ADC = 310.963b, DC = 22976.555b } normalized by 49744 is 0.501757459958
excel: good, comma separated values; some noise in header, trailer, commas to be removed

====== Timing information ======
Tokenization time = 0.169
Measure1 time = 0.006
Reduce1 time = 0.496
Reduce2 time = 18.769
Reduce3 time = 0.254
Measure2 time = 0.008
Total time = 19.701
================================


rpmpkgs.txt	3 hrs
good vals:        884    bad vals:          2    pcnt-bad:    0.226
{ TC = 538.669b, ADC = 111.889b, DC = 119990.514b } normalized by 217784 is 0.553434518124
number of correctly parsed records = 883
number of incorrectly parsed records = 3
golden not really available: no records parse correctly
====== Timing information ======
Tokenization time = 0.291
Measure1 time = 0.019
Reduce1 time = 3.099
Reduce2 time = 1.899
Reduce3 time = 0.635
Measure2 time = 0.014
Total time = 5.956
================================
excel: perfect: load data into single column
       weak: doesn't find structure: program name, version info, architecture, file extension

quarterlypersonalincome.txt 2 days
good vals:         60    bad vals:          2    pcnt-bad:    3.226
{ TC = 1287.492b, ADC = 510.480b, DC = 28835.789b } normalized by 81424 is 0.369955792544
number of correctly parsed records = 62
number of incorrectly parsed records = 0
gold format parses 100%
Golden complexity =
{ TC = 1037.335b, ADC = 84.170b, DC = 28227.742b } normalized by 81424 is 0.359415870193

====== Timing information ======
Tokenization time = 0.087
Measure1 time = 0.005
Reduce1 time = 0.323
Reduce2 time = 17.642
Reduce3 time = 0.182
Measure2 time = 0.006
Total time = 18.246
================================
excel: perfect

yum.txt 5 hrs
good vals:        327    bad vals:          1    pcnt-bad:    0.305
{ TC = 1795.481b, ADC = 475.334b, DC = 150080.672b } normalized by 145760 is 1.04196043733
number of correctly parsed records = 27
number of incorrectly parsed records = 301
Golden complexity =
26 not parsed properly; 92.1 correct
{ TC = 288.132b, ADC = 538.335b, DC = 162103.416b } normalized by 145760 is 1.11410227653
====== Timing information ======
Tokenization time = 0.536
Measure1 time = 0.095
Reduce1 time = 2.762
Reduce2 time = 0.882
Reduce3 time = 1.185
Measure2 time = 0.007
Total time = 5.467
================================
excel perfect

1967Transactions.short 4 hrs
good vals:        999    bad vals:          0    pcnt-bad:    0.000
{ TC = 175.174b, ADC = 123.491b, DC = 123367.394b } normalized by 567432 is 0.217722243067
number of correctly parsed records = 999
number of incorrectly parsed records = 0
golden
 998/999 parsed correctly
Golden complexity =
{ TC = 111.705b, ADC = 365.845b, DC = 362128.049b } normalized by 567432 is 0.638384431424

====== Timing information ======
Tokenization time = 0.582
Measure1 time = 0.089
Reduce1 time = 2.022
Reduce2 time = 0.792
Reduce3 time = 0.241
Measure2 time = 0.015
Total time = 3.741
================================
excel perfect


******************
warning: P_open: Installing default IO discipline : newline-terminated records
warning: Error [in Union_69_read]: record 49 byte 1 : Failed to match any branch of union Union_69
[record 49]>>><<<at-3.1.8-80_EL4.i386.rpm
Note [in Union_69_read]: record 49 byte 1 : Resynching at EOR
[record 49]>>>at-3.1.8-80_EL4.i386.rpm<<<
read returned error
warning: Error [in Union_69_read]: record 150 byte 1 : Failed to match any branch of union Union_69
[record 150]>>><<<dhcpv6_client-0.10-14_EL4.i386.rpm
Note [in Union_69_read]: record 150 byte 1 : Resynching at EOR
[record 150]>>>dhcpv6_client-0.10-14_EL4.i386.rpm<<<
read returned error
warning: Error [in Union_69_read]: record 356 byte 1 : Failed to match any branch of union Union_69
[record 356]>>><<<java-1.4.2-gcj-compat-1.4.2.0-27jpp.noarch.rpm
Note [in Union_69_read]: record 356 byte 1 : Resynching at EOR
[record 356]>>>java-1.4.2-gcj-compat-1.4.2.0-27jpp.noarch.rpm<<<
read returned error


******************


The errors we get so far:

1967Transactions.short:
warning: Error [in BTy_27_read]: record 999 byte 71 : Found EOF when searching for EOR
[record 999]0103      5502      0                   0                   0.00179   >>><<<
read returned error

ls-l.txt
Invisible \r at the end of some of the data records

warning: P_open: Installing default IO discipline : newline-terminated records
warning: Error [in BTy_36_read]: record 1 byte 13 : Unexpected data before EOR
[record 1]total 275528>>>\r<<<
read returned error
warning: Error [in BTy_36_read]: record 3 byte 59 : Unexpected data before EOR
[record 3]drwxr-xr-x   4 dpw fac      4096 Jan 21  2005 assignment 8>>>\r<<<
read returned error
warning: Error [in BTy_36_read]: record 13 byte 1 : Failed to match any branch of union BTy_36
[record 13]>>><<<drwxr-xr-x   2 dpw fac      4096 Jan  2 13:44 cv\r
Note [in BTy_36_read]: record 13 byte 1 : Resynching at EOR
[record 13]>>>drwxr-xr-x   2 dpw fac      4096 Jan  2 13:44 cv\r<<<
read returned error
warning: Error [in BTy_36_read]: record 15 byte 1 : Failed to match any branch of union BTy_36
[record 15]>>><<<-rw-r--r--   1 dpw fac         0 Jan 14 16:50 davehome.txt\r
Note [in BTy_36_read]: record 15 byte 1 : Resynching at EOR
[record 15]>>>-rw-r--r--   1 dpw fac         0 Jan 14 16:50 davehome.txt\r<<<
read returned error
warning: Error [in BTy_36_read]: record 18 byte 64 : Unexpected data before EOR
[record 18]-rw-r--r--   1 dpw fac    363517 Jul 14  2004 EMSltl_2002.ps.gz>>>\r<<<
read returned error
warning: Error [in BTy_36_read]: record 20 byte 1 : Failed to match any branch of union BTy_36
[record 20]>>><<<drwxr-xr-x  21 dpw fac      4096 Nov 15 11:11 grants\r
Note [in BTy_36_read]: record 20 byte 1 : Resynching at EOR
[record 20]>>>drwxr-xr-x  21 dpw fac      4096 Nov 15 11:11 grants\r<<<
read returned error
warning: Error [in BTy_36_read]: record 24 byte 1 : Failed to match any branch of union BTy_36
[record 24]>>><<<drwx------   4 dpw fac      4096 Jul 25 15:22 other\r
Note [in BTy_36_read]: record 24 byte 1 : Resynching at EOR
[record 24]>>>drwx------   4 dpw fac      4096 Jul 25 15:22 other\r<<<
read returned error
warning: Error [in BTy_36_read]: record 26 byte 58 : Unexpected data before EOR
[record 26]-rw-r--r--   1 dpw fac    137573 Dec 15  2005 proposal.ps>>>\r<<<
read returned error
warning: Error [in BTy_36_read]: record 28 byte 1 : Failed to match any branch of union BTy_36
[record 28]>>><<<drwx------   3 dpw fac      4096 Jan  5 12:45 recommendations\r
Note [in BTy_36_read]: record 28 byte 1 : Resynching at EOR
[record 28]>>>drwx------   3 dpw fac      4096 Jan  5 12:45 recommendations\r<<<
read returned error
warning: Error [in BTy_36_read]: record 30 byte 1 : Failed to match any branch of union BTy_36
[record 30]>>><<<drwx------  15 dpw fac      4096 Jan  2 17:37 reviews\r
Note [in BTy_36_read]: record 30 byte 1 : Resynching at EOR
[record 30]>>>drwx------  15 dpw fac      4096 Jan  2 17:37 reviews\r<<<
read returned error
warning: Error [in BTy_36_read]: record 32 byte 63 : Unexpected data before EOR
[record 32]-rw-r--r--   1 dpw fac     16430 Jan 25  2002 synchronous.txt~>>>\r<<<
read returned error
warning: Error [in BTy_36_read]: record 34 byte 1 : Failed to match any branch of union BTy_36
[record 34]>>><<<drwxr-xr-x   5 dpw fac      4096 Sep 11 16:53 talks\r
Note [in BTy_36_read]: record 34 byte 1 : Resynching at EOR
[record 34]>>>drwxr-xr-x   5 dpw fac      4096 Sep 11 16:53 talks\r<<<
read returned error

quarterlypersonalincome:

Invisible \r at the end of one data record

warning: Error [in BTy_222_read]: record 62 byte 105 : Unexpected data before EOR
[record 62]"Source: Regional Economic Information System, Bureau of Economic Analysis, U.S. Department of Commerce">>>\r<<<
read returned error

Railroad.txt: (bad)
The problem may be due to the RArray without separator or terminator in the front part of the
records.

warning: Error [in BTy_307_read]: record 1 byte 1 : Extra data before char separator ','
[record 1]>>>Table 1-9:  ADA-Accessible Rail Transit Stations by Agency<<<,,,,,,,,,,,,,,,
warning: XXX_REMOVE PDCI_ALWAYS_GETPOS_PLUS called with bad offset
read returned error
warning: Error [in BTy_307_read]: record 2 byte 1 : Extra data before char separator ','
[record 2]>>>Type of rail transit / agency<<<,Primary city served,Number of stations,,,,,,,Number of ADA-accessible stations,,,,,,
warning: XXX_REMOVE PDCI_ALWAYS_GETPOS_PLUS called with bad offset
read returned error
warning: Error [in BTy_307_read]: record 4 byte 1 : Extra data before char separator ','
[record 4]>>>Heavy rail<<<,,,,,,,,,,,,,,,
warning: XXX_REMOVE PDCI_ALWAYS_GETPOS_PLUS called with bad offset
read returned error
warning: Error [in BTy_307_read]: record 5 byte 1 : Extra data before char separator ','
[record 5]>>>Bay Area Rapid Transit<<<,"San Francisco, CA",36,39,39,39,39,39,39,36,39,39,39,39,39,39
warning: Error [in BTy_307_read]: record 5 byte 82 : Unexpected data before EOR
[record 5]Bay Area Rapid Transit,"San Francisco, CA",36,39,39,39,39,39,39,36,39,39,39,39,39>>>,39<<<
read returned error
warning: Error [in BTy_307_read]: record 6 byte 1 : Extra data before char separator ','
[record 6]>>>Los Angeles County Metropolitan Transportation Authority <<<,"Los Angeles, CA",5,8,8,13,16,16,16,5,8,8,13,16,16,16
warning: Error [in BTy_307_read]: record 6 byte 109 : Unexpected data before EOR
[record 6]Los Angeles County Metropolitan Transportation Authority ,"Los Angeles, CA",5,8,8,13,16,16,16,5,8,8,13,16,16>>>,16<<<
read returned error
warning: Error [in BTy_307_read]: record 7 byte 1 : Extra data before char separator ','
[record 7]>>>Washington Metropolitan Area Transit Authority<<<,"Washington, DC",74,75,75,76,78,83,83,74,75,75,76,78,83,54
read returned error
warning: Error [in BTy_307_read]: record 8 byte 1 : Extra data before char separator ','
[record 8]>>>Miami-Dade Transit Agency<<<,"Miami, FL",21,21,21,21,21,21,21,0,0,0,0,0,0,21
read returned error
warning: Error [in BTy_307_read]: record 9 byte 1 : Extra data before char separator ','
[record 9]>>>Metropolitan Atlanta Rapid Transit Authority<<<,"Atlanta, GA",36,36,36,36,36,38,38,36,36,36,36,36,38,38
read returned error
warning: Error [in BTy_307_read]: record 10 byte 1 : Extra data before char separator ','
[record 10]>>>Chicago Transit Authority<<<,"Chicago, IL",140,141,141,142,142,144,144,0,0,0,14,54,64,64
read returned error
warning: Error [in BTy_307_read]: record 11 byte 1 : Extra data before char separator ','
[record 11]>>>Massachusetts Bay Transportation Authority<<<,"Boston, MA",53,53,53,53,53,53,53,33,33,33,37,37,38,40
read returned error
warning: Error [in BTy_307_read]: record 12 byte 1 : Extra data before char separator ','
[record 12]>>>Mass Transit Administration - Maryland DOT<<<,"Baltimore, MD",14,14,14,14,14,14,14,14,14,14,14,14,14,14
read returned error
warning: Error [in BTy_307_read]: record 13 byte 1 : Extra data before char separator ','
[record 13]>>>Metropolitan Transportation Authority New York City Transit<<<,"New York, NY",468,468,468,468,468,468,468,28,30,30,31,41,41,44
warning: Error [in BTy_307_read]: record 13 byte 121 : Unexpected data before EOR
[record 13]Metropolitan Transportation Authority New York City Transit,"New York, NY",468,468,468,468,468,468,468,28,30,30,31,41,41>>>,44<<<
read returned error
warning: Error [in BTy_307_read]: record 14 byte 1 : Extra data before char separator ','
[record 14]>>>Port Authority Trans-Hudson Corporation (PATH)<<<,"New York, NY",13,13,13,13,13,13,11,6,6,6,6,6,6,5
warning: Error [in BTy_307_read]: record 14 byte 95 : Unexpected data before EOR
[record 14]Port Authority Trans-Hudson Corporation (PATH),"New York, NY",13,13,13,13,13,13,11,6,6,6,6,6,6>>>,5<<<
read returned error
warning: Error [in BTy_307_read]: record 15 byte 1 : Extra data before char separator ','
[record 15]>>>Metropolitan Transit Authority Staten Island Railway<<<,"New York, NY",22,22,22,22,22,23,23,2,2,2,2,2,3,4
warning: Error [in BTy_307_read]: record 15 byte 101 : Unexpected data before EOR
[record 15]Metropolitan Transit Authority Staten Island Railway,"New York, NY",22,22,22,22,22,23,23,2,2,2,2,2,3>>>,4<<<
read returned error
warning: Error [in BTy_307_read]: record 16 byte 1 : Extra data before char separator ','
[record 16]>>>Greater Cleveland Regional Transit Authority<<<,"Cleveland, OH",18,18,18,18,18,18,18,4,6,6,7,8,8,9
read returned error
warning: Error [in BTy_307_read]: record 17 byte 1 : Extra data before char separator ','
[record 17]>>>Southeastern Pennsylvania Transportation Authority<<<,"Philadelphia, PA",76,76,76,76,76,76,53,4,4,4,4,4,4,13
read returned error
warning: Error [in BTy_307_read]: record 18 byte 1 : Extra data before char separator ','
[record 18]>>>Port Authority Transit Corporation (PATCO)<<<,"Philadelphia, PA",13,13,13,13,13,13,13,3,3,5,5,5,5,5
read returned error
warning: Error [in BTy_307_read]: record 19 byte 1 : Extra data before char separator ','
[record 19]>>>Commuter rail<<<,,,,,,,,,,,,,,,
warning: XXX_REMOVE PDCI_ALWAYS_GETPOS_PLUS called with bad offset
read returned error
warning: Error [in BTy_307_read]: record 20 byte 1 : Extra data before char separator ','
[record 20]>>>Altamont Commuter Express Authority<<<,"San Jose, CA",U,U,U,U,U,U,10,U,U,U,U,U,U,10
warning: Error [in BTy_307_read]: record 20 byte 78 : Unexpected data before EOR
[record 20]Altamont Commuter Express Authority,"San Jose, CA",U,U,U,U,U,U,10,U,U,U,U,U,U>>>,10<<<
read returned error
warning: Error [in BTy_307_read]: record 21 byte 1 : Extra data before char separator ','
[record 21]>>>North San Diego County Transit Development Board <<<,"San Diego, CA",U,U,U,U,U,U,8,U,U,U,U,U,U,8
warning: Error [in BTy_307_read]: record 21 byte 92 : Unexpected data before EOR
[record 21]North San Diego County Transit Development Board ,"San Diego, CA",U,U,U,U,U,U,8,U,U,U,U,U,U>>>,8<<<
read returned error
warning: Error [in BTy_307_read]: record 22 byte 1 : Extra data before char separator ','
[record 22]>>>Peninsula Corridor Joint Powers Board <<<,"San Francisco, CA",U,U,U,U,U,U,34,U,U,U,U,U,U,22
warning: Error [in BTy_307_read]: record 22 byte 86 : Unexpected data before EOR
[record 22]Peninsula Corridor Joint Powers Board ,"San Francisco, CA",U,U,U,U,U,U,34,U,U,U,U,U,U>>>,22<<<
read returned error
warning: Error [in BTy_307_read]: record 23 byte 1 : Extra data before char separator ','
[record 23]>>>Southern California Regional Railroad Authority<<<,"Los Angeles, CA",U,45,46,46,47,49,51,U,45,46,46,47,49,51
warning: Error [in BTy_307_read]: record 23 byte 103 : Unexpected data before EOR
[record 23]Southern California Regional Railroad Authority,"Los Angeles, CA",U,45,46,46,47,49,51,U,45,46,46,47,49>>>,51<<<
read returned error
warning: Error [in BTy_307_read]: record 24 byte 1 : Extra data before char separator ','
[record 24]>>>Connecticut Department of Transportation <<<,"New Haven, CT",U,U,U,U,U,U,8,U,U,U,U,U,U,8
warning: Error [in BTy_307_read]: record 24 byte 84 : Unexpected data before EOR
[record 24]Connecticut Department of Transportation ,"New Haven, CT",U,U,U,U,U,U,8,U,U,U,U,U,U>>>,8<<<
read returned error
warning: Error [in BTy_307_read]: record 25 byte 1 : Extra data before char separator ','
[record 25]>>>Tri-County Commuter Rail Authority <<<,"Miami, FL",U,U,U,U,U,U,18,U,U,U,U,U,U,18
read returned error
warning: Error [in BTy_307_read]: record 26 byte 1 : Extra data before char separator ','
[record 26]>>>Northeast Illinois Regional Commuter Railroad Corporation<<<,"Chicago, IL",226,226,226,227,227,227,227,91,104,104,111,115,125,131
read returned error
warning: Error [in BTy_307_read]: record 27 byte 1 : Extra data before char separator ','
[record 27]>>>Northern Indiana Commuter Transportation District<<<,"Chicago, IL",18,18,18,18,18,18,20,7,7,7,7,7,7,11
read returned error
warning: Error [in BTy_307_read]: record 28 byte 1 : Extra data before char separator ','
[record 28]>>>Massachusetts Bay Transportation Authority<<<,"Boston, MA",U,U,117,119,120,121,124,U,U,67,69,74,75,78
read returned error
warning: Error [in BTy_307_read]: record 29 byte 1 : Extra data before char separator ','
[record 29]>>>Mass Transit Administration - Maryland DOT <<<,"Baltimore, MD",U,U,U,U,U,U,42,U,U,U,U,U,U,22
read returned error
warning: Error [in BTy_307_read]: record 30 byte 1 : Extra data before char separator ','
[record 30]>>>New Jersey Transit Corporation <<<,"New York, NY",158,158,158,162,162,162,167,22,22,41,46,46,46,51
warning: Error [in BTy_307_read]: record 30 byte 93 : Unexpected data before EOR
[record 30]New Jersey Transit Corporation ,"New York, NY",158,158,158,162,162,162,167,22,22,41,46,46,46>>>,51<<<
read returned error
warning: Error [in BTy_307_read]: record 31 byte 1 : Extra data before char separator ','
[record 31]>>>Metropolitan Transportation Authority Long Island Railroad<<<,"New York, NY",134,134,124,124,124,124,124,15,15,88,97,97,97,99
warning: Error [in BTy_307_read]: record 31 byte 120 : Unexpected data before EOR
[record 31]Metropolitan Transportation Authority Long Island Railroad,"New York, NY",134,134,124,124,124,124,124,15,15,88,97,97,97>>>,99<<<
read returned error
warning: Error [in BTy_307_read]: record 32 byte 1 : Extra data before char separator ','
[record 32]>>>Metropolitan Transportation Authority Metro-North Railroad Company<<<,"New York, NY",106,106,106,106,108,108,109,17,19,20,20,20,28,29
warning: Error [in BTy_307_read]: record 32 byte 128 : Unexpected data before EOR
[record 32]Metropolitan Transportation Authority Metro-North Railroad Company,"New York, NY",106,106,106,106,108,108,109,17,19,20,20,20,28>>>,29<<<
read returned error
warning: Error [in BTy_307_read]: record 33 byte 1 : Extra data before char separator ','
[record 33]>>>Pennsylvania Department of Transportation<<<,"Pennslyvania, PA",U,U,U,U,U,U,4,U,U,U,U,U,U,3
read returned error
warning: Error [in BTy_307_read]: record 34 byte 1 : Extra data before char separator ','
[record 34]>>>Southeastern Pennsylvania Transportation Authority <<<,"Philadelphia, PA",181,177,177,177,177,177,153,25,30,30,30,30,30,48
read returned error
warning: Error [in BTy_307_read]: record 35 byte 1 : Extra data before char separator ','
[record 35]>>>Dallas Area Rapid Transit <<<,"Dallas, TX",U,U,U,U,U,U,4,U,U,U,U,U,U,4
read returned error
warning: Error [in BTy_307_read]: record 36 byte 1 : Extra data before char separator ','
[record 36]>>>Fort Worth Transportation Authority <<<,"Fort Worth,TX",U,U,U,U,U,U,5,U,U,U,U,U,U,5
warning: Error [in BTy_307_read]: record 36 byte 79 : Unexpected data before EOR
[record 36]Fort Worth Transportation Authority ,"Fort Worth,TX",U,U,U,U,U,U,5,U,U,U,U,U,U>>>,5<<<
read returned error
warning: Error [in BTy_307_read]: record 37 byte 1 : Extra data before char separator ','
[record 37]>>>Virginia Railway Express <<<,"Washington, DC",U,U,U,U,U,U,18,U,U,U,U,U,U,18
read returned error
warning: Error [in BTy_307_read]: record 38 byte 1 : Extra data before char separator ','
[record 38]>>>Central Puget Sound Regional Transit Authority<<<,"Seattle, WA",U,U,U,U,U,U,7,U,U,U,U,U,U,7
read returned error
warning: Error [in BTy_307_read]: record 39 byte 1 : Extra data before char separator ','
[record 39]>>>Light rail<<<,,,,,,,,,,,,,,,
warning: XXX_REMOVE PDCI_ALWAYS_GETPOS_PLUS called with bad offset
read returned error
warning: Error [in BTy_307_read]: record 40 byte 1 : Extra data before char separator ','
[record 40]>>>Los Angeles County Metropolitan Transportation Authority <<<,"Los Angeles, CA",36,36,36,36,36,36,36,36,36,36,36,36,36,36
warning: Error [in BTy_307_read]: record 40 byte 115 : Unexpected data before EOR
[record 40]Los Angeles County Metropolitan Transportation Authority ,"Los Angeles, CA",36,36,36,36,36,36,36,36,36,36,36,36,36>>>,36<<<
read returned error
warning: Error [in BTy_307_read]: record 41 byte 1 : Extra data before char separator ','
[record 41]>>>San Francisco Municipal Railway <<<,"San Francisco, CA",11,11,11,11,11,11,9,0,0,0,0,0,0,9
warning: Error [in BTy_307_read]: record 41 byte 85 : Unexpected data before EOR
[record 41]San Francisco Municipal Railway ,"San Francisco, CA",11,11,11,11,11,11,9,0,0,0,0,0,0>>>,9<<<
read returned error
warning: Error [in BTy_307_read]: record 42 byte 1 : Extra data before char separator ','
[record 42]>>>Sacramento Regional Transit District<<<,"Sacramento, CA",28,28,28,29,29,29,29,0,0,0,29,29,29,29
read returned error
warning: Error [in BTy_307_read]: record 43 byte 27 : Extra data before char separator ','
[record 43]"San Diego Trolley, Inc.",>>>"San Diego<<<, CA",38,41,49,49,49,49,49,38,41,49,49,49,49,48
warning: Error [in BTy_307_read]: record 43 byte 81 : Unexpected data before EOR
[record 43]"San Diego Trolley, Inc.","San Diego, CA",38,41,49,49,49,49,49,38,41,49,49,49,49>>>,48<<<
read returned error
warning: Error [in BTy_307_read]: record 44 byte 1 : Extra data before char separator ','
[record 44]>>>Santa Clara Valley Transit Authority<<<,"San Jose, CA",33,34,34,34,47,49,44,5,5,5,5,21,23,44
warning: Error [in BTy_307_read]: record 44 byte 87 : Unexpected data before EOR
[record 44]Santa Clara Valley Transit Authority,"San Jose, CA",33,34,34,34,47,49,44,5,5,5,5,21,23>>>,44<<<
read returned error
warning: Error [in BTy_307_read]: record 45 byte 1 : Extra data before char separator ','
[record 45]>>>Regional Transportation District<<<,"Denver, CO",15,15,15,15,20,20,20,15,15,15,15,20,20,20
read returned error
warning: Error [in BTy_307_read]: record 46 byte 1 : Extra data before char separator ','
[record 46]>>>Regional Transit Authority of Orleans and Jefferson<<<,"New Orleans, LA",2,9,9,9,9,9,9,2,9,9,9,9,9,9
warning: Error [in BTy_307_read]: record 46 byte 96 : Unexpected data before EOR
[record 46]Regional Transit Authority of Orleans and Jefferson,"New Orleans, LA",2,9,9,9,9,9,9,2,9,9,9,9,9>>>,9<<<
read returned error
warning: Error [in BTy_307_read]: record 47 byte 1 : Extra data before char separator ','
[record 47]>>>Massachusetts Bay Transportation Authority<<<,"Boston, MA",95,95,95,95,95,78,78,9,9,9,12,12,16,16
read returned error
warning: Error [in BTy_307_read]: record 48 byte 1 : Extra data before char separator ','
[record 48]>>>Mass Transit Administration - Maryland DOT<<<,"Baltimore, MD",24,24,32,32,32,32,32,24,24,32,32,32,32,32
read returned error
warning: Error [in BTy_307_read]: record 49 byte 1 : Extra data before char separator ','
[record 49]>>>City of Detroit Department of Transportation <<<,"Detroit, MI",NA,NA,NA,NA,NA,8,8,NA,NA,NA,NA,NA,0,0
read returned error
warning: Error [in BTy_307_read]: record 50 byte 1 : Extra data before char separator ','
[record 50]>>>Bi-State Development Agency<<<,"St. Louis, MO",18,18,18,18,18,26,26,18,18,18,18,18,26,26
read returned error
warning: Error [in BTy_307_read]: record 51 byte 1 : Extra data before char separator ','
[record 51]>>>New Jersey Transit Corporation <<<,"Newark, NJ",11,11,11,11,11,11,26,0,0,0,0,0,0,15
read returned error
warning: Error [in BTy_307_read]: record 53 byte 1 : Extra data before char separator ','
[record 53]>>>Greater Cleveland Regional Transit Authority<<<,"Cleveland, OH",33,33,33,34,34,34,34,2,5,5,7,7,8,8
read returned error
warning: Error [in BTy_307_read]: record 54 byte 1 : Extra data before char separator ','
[record 54]>>>Tri-County Metropolitan Transportation District of Oregon<<<,"Portland, OR",27,27,29,47,47,47,52,26,26,28,46,46,46,52
read returned error
warning: Error [in BTy_307_read]: record 55 byte 1 : Extra data before char separator ','
[record 55]>>>Port Authority of Allegheny County<<<,"Pittsburgh, PA",13,13,13,13,13,13,14,0,13,13,13,13,13,14
read returned error
warning: Error [in BTy_307_read]: record 56 byte 1 : Extra data before char separator ','
[record 56]>>>Southeastern Pennsylvania Transportation Authority <<<,"Philadelphia, PA",64,64,64,64,64,64,68,0,0,0,0,0,0,3
read returned error
warning: Error [in BTy_307_read]: record 57 byte 1 : Extra data before char separator ','
[record 57]>>>Memphis Area Transit Authority<<<,"Memphis, TN",20,20,27,28,28,28,1,20,20,27,28,28,28,1
read returned error
warning: Error [in BTy_307_read]: record 58 byte 1 : Extra data before char separator ','
[record 58]>>>Dallas Area Rail Transit Authority<<<,"Dallas, TX",14,20,20,20,20,22,29,14,20,20,20,20,22,29
read returned error
warning: Error [in BTy_307_read]: record 59 byte 1 : Extra data before char separator ','
[record 59]>>>Galveston-Island Transit<<<,"Galveston, TX",3,3,3,U,U,U,3,3,3,3,U,U,U,3
read returned error
warning: Error [in BTy_307_read]: record 60 byte 1 : Extra data before char separator ','
[record 60]>>>Utah Transit Authority<<<,"Salt Lake City, UT",NA,NA,NA,16,16,20,20,NA,NA,NA,16,16,20,20
warning: Error [in BTy_307_read]: record 60 byte 83 : Unexpected data before EOR
[record 60]Utah Transit Authority,"Salt Lake City, UT",NA,NA,NA,16,16,20,20,NA,NA,NA,16,16,20>>>,20<<<
read returned error
warning: Error [in BTy_307_read]: record 61 byte 1 : Extra data before char separator ','
[record 61]>>>King County Department of Transportation <<<,"Seattle, WA",14,14,14,9,9,9,U,14,14,14,9,9,9,U
read returned error
warning: Error [in BTy_307_read]: record 62 byte 1 : Extra data before char separator ','
[record 62]>>>Kenosha Transit<<<,"Kenosha, WI",NA,NA,NA,NA,1,1,2,NA,NA,NA,NA,0,0,1
read returned error
warning: Error [in BTy_307_read]: record 63 byte 1 : Extra data before char separator ','
[record 63]>>>KEY:  ADA = Americans with Disabilities Act of 1992; NA = not applicable; U = data are not available<<<,,,,,,,,,,,,,,,
warning: XXX_REMOVE PDCI_ALWAYS_GETPOS_PLUS called with bad offset
read returned error
warning: Error [in BTy_307_read]: record 64 byte 1 : Extra data before char separator ','
[record 64]>>>NOTE<<<,,,,,,,,,,,,,,,
warning: XXX_REMOVE PDCI_ALWAYS_GETPOS_PLUS called with bad offset
read returned error
warning: XXX_REMOVE PDCI_ALWAYS_GETPOS_PLUS called with bad offset
warning: Error [in BTy_307_read]: record 66 byte 1 : Extra data before char separator ','
[record 66]>>>SOURCE<<<,,,,,,,,,,,,,,,


Yum.txt

warning: Error [in BTy_146_read]: record 7 byte 47 : Unexpected data before EOR
[record 7]Dec 10 04:07:58 Updated: shadow-utils.x86_64 2>>>:4.0.3-58.RHEL4<<<
read returned error
warning: Error [in BTy_15_read]: record 39 byte 45 : Did not match any branch of enum BTy_15
[record 39]Dec 10 04:11:27 Installed: fonts-xorg-75dpi.>>><<<noarch 6.8.1.1-1.EL.1
warning: Error [in BTy_142_read]: record 39 byte 45 : Failed to match branch vBTy_141_1
[record 39]Dec 10 04:11:27 Installed: fonts-xorg-75dpi.>>><<<noarch 6.8.1.1-1.EL.1
read returned error
warning: Error [in BTy_15_read]: record 59 byte 37 : Did not match any branch of enum BTy_15
[record 59]Dec 10 04:12:18 Installed: perl-URI.>>><<<noarch 1.30-4
warning: Error [in BTy_142_read]: record 59 byte 37 : Failed to match branch vBTy_141_1
[record 59]Dec 10 04:12:18 Installed: perl-URI.>>><<<noarch 1.30-4
read returned error
warning: Error [in BTy_146_read]: record 63 byte 42 : Unexpected data before EOR
[record 63]Dec 10 04:12:19 Installed: guile.x86_64 5>>>:1.6.4-14<<<
read returned error
warning: Error [in BTy_146_read]: record 71 byte 41 : Unexpected data before EOR
[record 71]Dec 10 04:21:19 Installed: nmap.x86_64 2>>>:3.70-1<<<
read returned error
warning: Error [in BTy_146_read]: record 72 byte 42 : Unexpected data before EOR
[record 72]Dec 13 00:46:30 Installed: bind.x86_64 20>>>:9.2.4-2<<<
read returned error
warning: Error [in BTy_146_read]: record 73 byte 48 : Unexpected data before EOR
[record 73]Dec 13 00:46:31 Installed: bind-devel.x86_64 20>>>:9.2.4-2<<<
read returned error
warning: Error [in BTy_146_read]: record 74 byte 45 : Unexpected data before EOR
[record 74]Dec 13 00:46:32 Installed: bind-libs.i386 20>>>:9.2.4-2<<<
read returned error
warning: Error [in BTy_146_read]: record 75 byte 49 : Unexpected data before EOR
[record 75]Dec 13 00:46:32 Installed: bind-chroot.x86_64 20>>>:9.2.4-2<<<
read returned error
warning: Error [in BTy_146_read]: record 94 byte 44 : Unexpected data before EOR
[record 94]Dec 13 18:05:36 Installed: postfix.x86_64 2>>>:2.1.5-4.2.RHEL4<<<
read returned error
warning: Error [in BTy_146_read]: record 116 byte 40 : Unexpected data before EOR
[record 116]Dec 20 03:50:22 Installed: tix.x86_64 1>>>:8.1.4-98<<<
read returned error
warning: Error [in BTy_146_read]: record 123 byte 41 : Unexpected data before EOR
[record 123]Dec 24 00:44:39 Updated: perl.x86_64 3:5>>>.8.5-24.RHEL4<<<
read returned error
warning: Error [in BTy_15_read]: record 131 byte 30 : Did not match any branch of enum BTy_15
[record 131]May 02 06:10:35 Updated: yum.>>><<<noarch 2.4.2-2.centos4
warning: Error [in BTy_142_read]: record 131 byte 30 : Failed to match branch vBTy_141_2
[record 131]May 02 06:10:35 Updated: yum.>>><<<noarch 2.4.2-2.centos4
read returned error
warning: Error [in BTy_15_read]: record 134 byte 33 : Did not match any branch of enum BTy_15
[record 134]May 02 06:19:25 Updated: hwdata.>>><<<noarch 0.146.18.EL-1
warning: Error [in BTy_142_read]: record 134 byte 33 : Failed to match branch vBTy_141_2
[record 134]May 02 06:19:25 Updated: hwdata.>>><<<noarch 0.146.18.EL-1
read returned error
warning: Error [in BTy_15_read]: record 135 byte 33 : Did not match any branch of enum BTy_15
[record 135]May 02 06:19:26 Updated: tzdata.>>><<<noarch 2006a-2.EL4
warning: Error [in BTy_142_read]: record 135 byte 33 : Failed to match branch vBTy_141_2
[record 135]May 02 06:19:26 Updated: tzdata.>>><<<noarch 2006a-2.EL4
read returned error
warning: Error [in BTy_146_read]: record 148 byte 47 : Unexpected data before EOR
[record 148]May 02 06:19:51 Updated: shadow-utils.x86_64 2>>>:4.0.3-60.RHEL4<<<
read returned error
warning: Error [in BTy_15_read]: record 172 byte 33 : Did not match any branch of enum BTy_15
[record 172]May 02 06:20:06 Updated: rhnlib.>>><<<noarch 1.8.2-1.p23.1
warning: Error [in BTy_142_read]: record 172 byte 33 : Failed to match branch vBTy_141_2
[record 172]May 02 06:20:06 Updated: rhnlib.>>><<<noarch 1.8.2-1.p23.1
read returned error
warning: Error [in BTy_146_read]: record 178 byte 49 : Unexpected data before EOR
[record 178]May 02 06:20:08 Updated: centos-release.x86_64 6>>>:4-3.2<<<
read returned error
warning: Error [in BTy_15_read]: record 194 byte 50 : Did not match any branch of enum BTy_15
[record 194]May 02 06:20:18 Updated: selinux-policy-targeted.>>><<<noarch 1.17.30-2.126
warning: Error [in BTy_142_read]: record 194 byte 50 : Failed to match branch vBTy_141_2
[record 194]May 02 06:20:18 Updated: selinux-policy-targeted.>>><<<noarch 1.17.30-2.126
read returned error
warning: Error [in BTy_146_read]: record 198 byte 41 : Unexpected data before EOR
[record 198]May 02 06:20:20 Updated: autofs.x86_64 1>>>:4.1.3-169<<<
read returned error
warning: Error [in BTy_146_read]: record 211 byte 41 : Unexpected data before EOR
[record 211]May 02 06:20:26 Updated: ypbind.x86_64 3>>>:1.17.2-8<<<
read returned error
warning: Error [in BTy_15_read]: record 220 byte 41 : Did not match any branch of enum BTy_15
[record 220]May 02 06:20:29 Updated: centos-yumconf.>>><<<noarch 4-4.5
warning: Error [in BTy_142_read]: record 220 byte 41 : Failed to match branch vBTy_141_2
[record 220]May 02 06:20:29 Updated: centos-yumconf.>>><<<noarch 4-4.5
read returned error
warning: Error [in BTy_15_read]: record 225 byte 39 : Did not match any branch of enum BTy_15
[record 225]May 02 06:20:42 Updated: redhat-logos.>>><<<noarch 1.1.26-1.centos4.1
warning: Error [in BTy_142_read]: record 225 byte 39 : Failed to match branch vBTy_141_2
[record 225]May 02 06:20:42 Updated: redhat-logos.>>><<<noarch 1.1.26-1.centos4.1
read returned error
warning: Error [in BTy_15_read]: record 235 byte 43 : Did not match any branch of enum BTy_15
[record 235]May 02 06:20:52 Updated: fonts-xorg-75dpi.>>><<<noarch 6.8.2-1.EL
warning: Error [in BTy_142_read]: record 235 byte 43 : Failed to match branch vBTy_141_2
[record 235]May 02 06:20:52 Updated: fonts-xorg-75dpi.>>><<<noarch 6.8.2-1.EL
read returned error
warning: Error [in BTy_15_read]: record 243 byte 52 : Did not match any branch of enum BTy_15
[record 243]May 02 06:21:06 Updated: system-config-network-tui.>>><<<noarch 1.3.22.0.EL.4.2-1
warning: Error [in BTy_142_read]: record 243 byte 52 : Failed to match branch vBTy_141_2
[record 243]May 02 06:21:06 Updated: system-config-network-tui.>>><<<noarch 1.3.22.0.EL.4.2-1
read returned error
warning: Error [in BTy_146_read]: record 247 byte 47 : Unexpected data before EOR
[record 247]May 02 06:21:10 Updated: kernel-utils.x86_64 1>>>:2.4-13.1.80<<<
read returned error
warning: Error [in BTy_146_read]: record 249 byte 43 : Unexpected data before EOR
[record 249]May 02 06:21:13 Updated: dhclient.x86_64 7>>>:3.0.1-54.EL4<<<
read returned error
warning: Error [in BTy_15_read]: record 269 byte 50 : Did not match any branch of enum BTy_15
[record 269]May 12 12:20:57 Installed: fedora-usermgmt-setup.>>><<<noarch 0.8-1
warning: Error [in BTy_142_read]: record 269 byte 50 : Failed to match branch vBTy_141_1
[record 269]May 12 12:20:57 Installed: fedora-usermgmt-setup.>>><<<noarch 0.8-1
read returned error
warning: Error [in BTy_15_read]: record 270 byte 43 : Did not match any branch of enum BTy_15
[record 270]May 12 12:20:57 Installed: perl-DateManip.>>><<<noarch 5.42a-3
warning: Error [in BTy_142_read]: record 270 byte 43 : Failed to match branch vBTy_141_1
[record 270]May 12 12:20:57 Installed: perl-DateManip.>>><<<noarch 5.42a-3
read returned error
warning: Error [in BTy_15_read]: record 273 byte 47 : Did not match any branch of enum BTy_15
[record 273]May 12 12:20:58 Installed: perl-HTML-Template.>>><<<noarch 2.7-2
warning: Error [in BTy_142_read]: record 273 byte 47 : Failed to match branch vBTy_141_1
[record 273]May 12 12:20:58 Installed: perl-HTML-Template.>>><<<noarch 2.7-2
read returned error
warning: Error [in BTy_15_read]: record 274 byte 46 : Did not match any branch of enum BTy_15
[record 274]May 12 12:20:58 Installed: perl-IO-Multiplex.>>><<<noarch 1.08-4
warning: Error [in BTy_142_read]: record 274 byte 46 : Failed to match branch vBTy_141_1
[record 274]May 12 12:20:58 Installed: perl-IO-Multiplex.>>><<<noarch 1.08-4
read returned error
warning: Error [in BTy_15_read]: record 275 byte 44 : Did not match any branch of enum BTy_15
[record 275]May 12 12:20:59 Installed: perl-Net-Server.>>><<<noarch 0.90-1.el4.kb
warning: Error [in BTy_142_read]: record 275 byte 44 : Failed to match branch vBTy_141_1
[record 275]May 12 12:20:59 Installed: perl-Net-Server.>>><<<noarch 0.90-1.el4.kb
read returned error
warning: Error [in BTy_15_read]: record 276 byte 57 : Did not match any branch of enum BTy_15
[record 276]May 12 12:21:00 Installed: fedora-usermgmt-shadow-utils.>>><<<noarch 0.8-1
warning: Error [in BTy_142_read]: record 276 byte 57 : Failed to match branch vBTy_141_1
[record 276]May 12 12:21:00 Installed: fedora-usermgmt-shadow-utils.>>><<<noarch 0.8-1
read returned error
warning: Error [in BTy_15_read]: record 277 byte 44 : Did not match any branch of enum BTy_15
[record 277]May 12 12:21:00 Installed: fedora-usermgmt.>>><<<noarch 0.8-1
warning: Error [in BTy_142_read]: record 277 byte 44 : Failed to match branch vBTy_141_1
[record 277]May 12 12:21:00 Installed: fedora-usermgmt.>>><<<noarch 0.8-1
read returned error
warning: Error [in BTy_15_read]: record 278 byte 34 : Did not match any branch of enum BTy_15
[record 278]May 12 12:21:01 Installed: munin.>>><<<noarch 1.2.4-7.el4.kb
warning: Error [in BTy_142_read]: record 278 byte 34 : Failed to match branch vBTy_141_1
[record 278]May 12 12:21:01 Installed: munin.>>><<<noarch 1.2.4-7.el4.kb
read returned error
warning: Error [in BTy_15_read]: record 279 byte 45 : Did not match any branch of enum BTy_15
[record 279]May 12 12:39:30 Installed: perl-HTML-Tagset.>>><<<noarch 3.03-30
warning: Error [in BTy_142_read]: record 279 byte 45 : Failed to match branch vBTy_141_1
[record 279]May 12 12:39:30 Installed: perl-HTML-Tagset.>>><<<noarch 3.03-30
read returned error
warning: Error [in BTy_15_read]: record 281 byte 45 : Did not match any branch of enum BTy_15
[record 281]May 12 12:39:31 Installed: perl-libwww-perl.>>><<<noarch 5.79-5
warning: Error [in BTy_142_read]: record 281 byte 45 : Failed to match branch vBTy_141_1
[record 281]May 12 12:39:31 Installed: perl-libwww-perl.>>><<<noarch 5.79-5
read returned error
warning: Error [in BTy_15_read]: record 283 byte 39 : Did not match any branch of enum BTy_15
[record 283]May 12 12:39:39 Installed: munin-node.>>><<<noarch 1.2.4-7.el4.kb
warning: Error [in BTy_142_read]: record 283 byte 39 : Failed to match branch vBTy_141_1
[record 283]May 12 12:39:39 Installed: munin-node.>>><<<noarch 1.2.4-7.el4.kb
read returned error
warning: Error [in BTy_15_read]: record 307 byte 32 : Did not match any branch of enum BTy_15
[record 307]Jul 12 15:29:41 Updated: munin.>>><<<noarch 1.2.4-8.el4.kb
warning: Error [in BTy_142_read]: record 307 byte 32 : Failed to match branch vBTy_141_2
[record 307]Jul 12 15:29:41 Updated: munin.>>><<<noarch 1.2.4-8.el4.kb
read returned error
warning: Error [in BTy_15_read]: record 309 byte 37 : Did not match any branch of enum BTy_15
[record 309]Jul 12 15:29:56 Updated: munin-node.>>><<<noarch 1.2.4-8.el4.kb
warning: Error [in BTy_142_read]: record 309 byte 37 : Failed to match branch vBTy_141_2
[record 309]Jul 12 15:29:56 Updated: munin-node.>>><<<noarch 1.2.4-8.el4.kb
read returned error
warning: Error [in BTy_146_read]: record 310 byte 43 : Unexpected data before EOR
[record 310]Jul 16 03:59:20 Installed: mcelog.x86_64 1>>>:0.4-1.9.EL<<<
read returned error
warning: Error [in BTy_146_read]: record 311 byte 41 : Unexpected data before EOR
[record 311]Jul 16 04:07:49 Installed: dhcp.x86_64 7>>>:3.0.1-54.EL4<<<
read returned error
warning: Error [in BTy_146_read]: record 313 byte 47 : Unexpected data before EOR
[record 313]Jul 16 04:07:50 Installed: dhcp-devel.x86_64 7>>>:3.0.1-54.EL4<<<
read returned error
warning: Error [in BTy_146_read]: record 322 byte 45 : Unexpected data before EOR
[record 322]Jul 21 02:52:07 Updated: vixie-cron.x86_64 4>>>:4.1-44.EL4<<<
read returned error
warning: Error [in BTy_146_read]: record 328 byte 47 : Found EOF when searching for EOR
[record 328]Jul 21 02:52:13 Updated: php.x86_64 4.3.9-3.15>>><<<
read returned error

*****************************************************************************************************
<top> : struct BTy_146
*****************************************************************************************************
good vals:        283    bad vals:         45    pcnt-bad:   13.720

Boot.txt:
Not working for any records. 
Possible cause: Array of size 0 exists for some records, but pads insists on having at least
one element in all arrays.

Example errors:

warning: Error [in BTy_50_read]: record 1 byte 29 : Failed to match any branch of union BTy_50
[record 1]Apr 24 04:02:24 srv7 snortd:>>><<< snort shutdown succeeded
Note [in BTy_194_read]: record 1 byte 29 : Resynching at EOR
[record 1]Apr 24 04:02:24 srv7 snortd:>>> snort shutdown succeeded<<<
read returned error
warning: XXX_REMOVE PDCI_ALWAYS_GETPOS_PLUS called with bad offset
warning: Error [in BTy_50_read]: record 2 byte 29 : Failed to match any branch of union BTy_50
[record 2]Apr 24 04:02:24 srv7 snortd:>>><<< snort startup succeeded
Note [in BTy_194_read]: record 2 byte 29 : Resynching at EOR
[record 2]Apr 24 04:02:24 srv7 snortd:>>> snort startup succeeded<<<
read returned error
warning: Error [in BTy_50_read]: record 3 byte 26 : Failed to match any branch of union BTy_50
[record 3]Apr 24 14:44:28 srv7 yum:>>><<< Disabling nightly yum update: 



