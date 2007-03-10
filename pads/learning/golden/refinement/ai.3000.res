Source file to process: data/ai.3000
Output directory: gen/
Max depth to explore: 50
Print line numbers in output contexts: false
Print ids and output type tokens: true
Histogram comparison tolerance (percentage): 0.01
Struct determination tolerance (percentage): 0.1
Noise level threshold (percentage): 0.0
Minimum width threshold for array: 2
Junk threshold (percentage): 0.1
Starting on file data/ai.3000
3000 records.
Histogram of number of tokens per record:
	13:	1704
	17:	507
	18:	4
	19:	666
	21:	58
	23:	20
	24:	18
	26:	23

Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: (\")[Group Body](\")
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0

Token: ([)[Group Body](])
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0

Token: [white space]
Total number of token occurrences: 18000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	6:	3000	1.0


Cluster 1:
Token: (-)
Total number of token occurrences: 6348.
Number of records with at least one token occurrence: 3000.
StructScore: 352.
	1:	1	0.000333333333333
	2:	2650	0.883333333333
	3:	349	0.116333333333


Cluster 2:
Token: [int]
Total number of token occurrences: 5709.
Number of records with at least one token occurrence: 3000.
StructScore: 511.
	1:	349	0.116333333333
	2:	2601	0.867
	3:	46	0.0153333333333
	5:	4	0.00133333333333


Cluster 3:
Token: [IP]
Total number of token occurrences: 1704.
Number of records with at least one token occurrence: 1704.
StructScore: 1296.
	1:	1704	0.568


Cluster 4:
Token: [string]
Total number of token occurrences: 4868.
Number of records with at least one token occurrence: 1296.
StructScore: 26691.
	3:	514	0.171333333333
	4:	667	0.222333333333
	5:	55	0.0183333333333
	6:	37	0.0123333333333
	7:	23	0.00766666666667

Token: (.)
Total number of token occurrences: 3576.
Number of records with at least one token occurrence: 1296.
StructScore: 26691.
	2:	511	0.170333333333
	3:	670	0.223333333333
	4:	54	0.018
	5:	38	0.0126666666667
	6:	23	0.00766666666667


Junk Tolerance Threshold: 300
Coverage: 3000
Num Tokens: 8
Struct
Coverage:3000
Token count:8
(\")[Group Body](\")	Occurrences:1
([)[Group Body](])	Occurrences:1
[white space]	Occurrences:6
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: [IP]
Total number of token occurrences: 1704.
Number of records with at least one token occurrence: 1704.
StructScore: 1296.
	1:	1704	0.568


Cluster 1:
Token: [int]
Total number of token occurrences: 58.
Number of records with at least one token occurrence: 50.
StructScore: 8854.
	1:	46	0.0153333333333
	3:	4	0.00133333333333


Cluster 2:
Token: (.)
Total number of token occurrences: 3576.
Number of records with at least one token occurrence: 1296.
StructScore: 26691.
	2:	511	0.170333333333
	3:	670	0.223333333333
	4:	54	0.018
	5:	38	0.0126666666667
	6:	23	0.00766666666667

Token: [string]
Total number of token occurrences: 4867.
Number of records with at least one token occurrence: 1296.
StructScore: 26692.
	3:	515	0.171666666667
	4:	666	0.222
	5:	55	0.0183333333333
	6:	37	0.0123333333333
	7:	23	0.00766666666667


Junk Tolerance Threshold: 300
Coverage: 1704
Num Tokens: 1
Struct
Coverage:1704
Token count:1
[IP]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 13.
Computed clusters
Cluster 0:
Token: (.)
Total number of token occurrences: 3576.
Number of records with at least one token occurrence: 1296.
StructScore: 1131.
	2:	511	0.394290123457
	3:	670	0.516975308642
	4:	54	0.0416666666667
	5:	38	0.0293209876543
	6:	23	0.0177469135802

Token: [string]
Total number of token occurrences: 4867.
Number of records with at least one token occurrence: 1296.
StructScore: 1132.
	3:	515	0.39737654321
	4:	666	0.513888888889
	5:	55	0.0424382716049
	6:	37	0.028549382716
	7:	23	0.0177469135802


Cluster 1:
Token: [int]
Total number of token occurrences: 58.
Number of records with at least one token occurrence: 50.
StructScore: 3742.
	1:	46	0.0354938271605
	3:	4	0.00308641975309


Junk Tolerance Threshold: 130
Coverage: 1296
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: (.)
Total number of token occurrences: 3576.
Number of records with at least one token occurrence: 1296.
StructScore: 1131.
	2:	511	0.394290123457
	3:	670	0.516975308642
	4:	54	0.0416666666667
	5:	38	0.0293209876543
	6:	23	0.0177469135802

Token: [string]
Total number of token occurrences: 4867.
Number of records with at least one token occurrence: 1296.
StructScore: 1132.
	3:	515	0.39737654321
	4:	666	0.513888888889
	5:	55	0.0424382716049
	6:	37	0.028549382716
	7:	23	0.0177469135802


Cluster 1:
Token: [int]
Total number of token occurrences: 58.
Number of records with at least one token occurrence: 50.
StructScore: 3742.
	1:	46	0.0354938271605
	3:	4	0.00308641975309


Possible array tokens:
(.)
Records in possible array context:1296
Total:3576
Coverage:1296
Width:5
Possible array tokens:
[string]
Records in possible array context:1296
Total:4867
Coverage:1296
Width:5
Array	(.)	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 13.
Computed clusters
Cluster 0:
Token: (.)
Total number of token occurrences: 1296.
Number of records with at least one token occurrence: 1296.
StructScore: 0.
	1:	1296	1.0

Token: [string]
Total number of token occurrences: 1291.
Number of records with at least one token occurrence: 1291.
StructScore: 5.
	1:	1291	0.996141975309


Cluster 1:
Token: [int]
Total number of token occurrences: 58.
Number of records with at least one token occurrence: 50.
StructScore: 3742.
	1:	46	0.0354938271605
	3:	4	0.00308641975309


Junk Tolerance Threshold: 130
Coverage: 1291
Num Tokens: 2
Struct
Coverage:1291
Token count:2
(.)	Occurrences:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 13.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 1246.
Number of records with at least one token occurrence: 1246.
StructScore: 45.
	1:	1246	0.965143299768


Cluster 1:
Token: [int]
Total number of token occurrences: 45.
Number of records with at least one token occurrence: 45.
StructScore: 1246.
	1:	45	0.0348567002324


Junk Tolerance Threshold: 130
Coverage: 1246
Num Tokens: 1
Struct
Coverage:1246
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 45.
Number of records with at least one token occurrence: 45.
StructScore: 0.
	1:	45	1.0


Junk Tolerance Threshold: 5
Coverage: 45
Num Tokens: 1
Struct
Coverage:45
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (.)
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 0.
	1:	5	1.0


Cluster 1:
Token: [int]
Total number of token occurrences: 13.
Number of records with at least one token occurrence: 5.
StructScore: 1.
	1:	1	0.2
	3:	4	0.8


Junk Tolerance Threshold: 1
Coverage: 5
Num Tokens: 1
Struct
Coverage:5
Token count:1
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 13.
Number of records with at least one token occurrence: 5.
StructScore: 1.
	1:	1	0.2
	3:	4	0.8


Junk Tolerance Threshold: 1
Coverage: 5
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [int]
Total number of token occurrences: 13.
Number of records with at least one token occurrence: 5.
StructScore: 1.
	1:	1	0.2
	3:	4	0.8


Possible array tokens:
[int]
Records in possible array context:5
Total:13
Coverage:5
Width:2
Array	[int]	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 0.
	1:	5	1.0


Junk Tolerance Threshold: 1
Coverage: 5
Num Tokens: 1
Struct
Coverage:5
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 8.
StructScore: 0.
	1:	8	1.0


Junk Tolerance Threshold: 1
Coverage: 8
Num Tokens: 1
Struct
Coverage:8
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 0.
	1:	5	1.0


Junk Tolerance Threshold: 1
Coverage: 5
Num Tokens: 1
Struct
Coverage:5
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 23.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 2280.
Number of records with at least one token occurrence: 2280.
StructScore: 0.
	1:	2280	1.0

Token: (.)
Total number of token occurrences: 2280.
Number of records with at least one token occurrence: 2280.
StructScore: 0.
	1:	2280	1.0


Junk Tolerance Threshold: 228
Coverage: 2280
Num Tokens: 2
Struct
Coverage:2280
Token count:2
[string]	Occurrences:1
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 13.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 1296.
Number of records with at least one token occurrence: 1296.
StructScore: 0.
	1:	1296	1.0


Junk Tolerance Threshold: 130
Coverage: 1296
Num Tokens: 1
Struct
Coverage:1296
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: (-)
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0


Junk Tolerance Threshold: 300
Coverage: 3000
Num Tokens: 1
Struct
Coverage:3000
Token count:1
(-)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: (-)
Total number of token occurrences: 2999.
Number of records with at least one token occurrence: 2999.
StructScore: 1.
	1:	2999	0.999666666667


Cluster 1:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2999.
	1:	1	0.000333333333333


Junk Tolerance Threshold: 300
Coverage: 2999
Num Tokens: 1
Struct
Coverage:2999
Token count:1
(-)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0


Junk Tolerance Threshold: 1
Coverage: 1
Num Tokens: 1
Struct
Coverage:1
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: [Time]
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0

Token: [int]
Total number of token occurrences: 9000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	3:	3000	1.0

Token: [string]
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0

Token: [white space]
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0

Token: (/)
Total number of token occurrences: 6000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	2:	3000	1.0

Token: (:)
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0

Token: ([)
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0

Token: (])
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0


Junk Tolerance Threshold: 300
Coverage: 3000
Num Tokens: 11
Struct
Coverage:3000
Token count:11
[Time]	Occurrences:1
[int]	Occurrences:3
[string]	Occurrences:1
[white space]	Occurrences:1
(/)	Occurrences:2
(:)	Occurrences:1
([)	Occurrences:1
(])	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: [white space]
Total number of token occurrences: 6000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	2:	3000	1.0

Token: (\")
Total number of token occurrences: 6000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	2:	3000	1.0

Token: [int]
Total number of token occurrences: 6021.
Number of records with at least one token occurrence: 3000.
StructScore: 22.
	2:	2980	0.993333333333
	3:	19	0.00633333333333
	4:	1	0.000333333333333


Cluster 1:
Token: (.)
Total number of token occurrences: 5872.
Number of records with at least one token occurrence: 3000.
StructScore: 128.
	1:	128	0.0426666666667
	2:	2872	0.957333333333


Cluster 2:
Token: (&)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 2993.
	1:	7	0.00233333333333

Token: (%)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2996.
	1:	4	0.00133333333333

Token: (=)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2996.
	1:	4	0.00133333333333

Token: (*)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 2998.
	2:	2	0.000666666666667

Token: (')
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2999.
	1:	1	0.000333333333333

Token: (@)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2999.
	1:	1	0.000333333333333


Cluster 3:
Token: (/)
Total number of token occurrences: 10600.
Number of records with at least one token occurrence: 3000.
StructScore: 3126.
	2:	161	0.0536666666667
	3:	1362	0.454
	4:	1216	0.405333333333
	5:	238	0.0793333333333
	6:	23	0.00766666666667


Cluster 4:
Token: [string]
Total number of token occurrences: 16378.
Number of records with at least one token occurrence: 3000.
StructScore: 4534.
	2:	45	0.015
	3:	56	0.0186666666667
	4:	133	0.0443333333333
	5:	1303	0.434333333333
	6:	1196	0.398666666667
	7:	239	0.0796666666667
	8:	28	0.00933333333333


Junk Tolerance Threshold: 300
Coverage: 2980
Num Tokens: 6
Struct
Coverage:2980
Token count:6
[white space]	Occurrences:2
(\")	Occurrences:2
[int]	Occurrences:2
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0


Junk Tolerance Threshold: 300
Coverage: 3000
Num Tokens: 1
Struct
Coverage:3000
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: (.)
Total number of token occurrences: 2872.
Number of records with at least one token occurrence: 2872.
StructScore: 128.
	1:	2872	0.957333333333


Cluster 1:
Token: (&)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 2993.
	1:	7	0.00233333333333

Token: (%)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2996.
	1:	4	0.00133333333333

Token: (=)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2996.
	1:	4	0.00133333333333

Token: (*)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 2998.
	2:	2	0.000666666666667

Token: (')
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2999.
	1:	1	0.000333333333333

Token: (@)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2999.
	1:	1	0.000333333333333


Cluster 2:
Token: (/)
Total number of token occurrences: 7600.
Number of records with at least one token occurrence: 3000.
StructScore: 3126.
	1:	161	0.0536666666667
	2:	1362	0.454
	3:	1216	0.405333333333
	4:	238	0.0793333333333
	5:	23	0.00766666666667


Cluster 3:
Token: [string]
Total number of token occurrences: 10378.
Number of records with at least one token occurrence: 2955.
StructScore: 4636.
	1:	56	0.0186666666667
	2:	133	0.0443333333333
	3:	1303	0.434333333333
	4:	1196	0.398666666667
	5:	239	0.0796666666667
	6:	28	0.00933333333333


Cluster 4:
Token: [int]
Total number of token occurrences: 21.
Number of records with at least one token occurrence: 20.
StructScore: 8941.
	1:	19	0.00633333333333
	2:	1	0.000333333333333


Junk Tolerance Threshold: 300
Coverage: 2872
Num Tokens: 1
Struct
Coverage:2872
Token count:1
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 29.
Computed clusters
Cluster 0:
Token: (/)
Total number of token occurrences: 7383.
Number of records with at least one token occurrence: 2872.
StructScore: 2779.
	1:	106	0.0369080779944
	2:	1304	0.454038997214
	3:	1202	0.41852367688
	4:	237	0.0825208913649
	5:	23	0.00800835654596

Token: [string]
Total number of token occurrences: 7391.
Number of records with at least one token occurrence: 2872.
StructScore: 2794.
	1:	106	0.0369080779944
	2:	1303	0.453690807799
	3:	1196	0.41643454039
	4:	244	0.0849582172702
	5:	23	0.00800835654596


Cluster 1:
Token: (&)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 2865.
	1:	7	0.00243732590529

Token: (')
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2871.
	1:	1	0.000348189415042

Token: (@)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2871.
	1:	1	0.000348189415042


Cluster 2:
Token: [int]
Total number of token occurrences: 17.
Number of records with at least one token occurrence: 16.
StructScore: 8569.
	1:	15	0.00522284122563
	2:	1	0.000348189415042


Junk Tolerance Threshold: 288
Coverage: 2872
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: (/)
Total number of token occurrences: 7383.
Number of records with at least one token occurrence: 2872.
StructScore: 2779.
	1:	106	0.0369080779944
	2:	1304	0.454038997214
	3:	1202	0.41852367688
	4:	237	0.0825208913649
	5:	23	0.00800835654596

Token: [string]
Total number of token occurrences: 7391.
Number of records with at least one token occurrence: 2872.
StructScore: 2794.
	1:	106	0.0369080779944
	2:	1303	0.453690807799
	3:	1196	0.41643454039
	4:	244	0.0849582172702
	5:	23	0.00800835654596


Cluster 1:
Token: (&)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 2865.
	1:	7	0.00243732590529

Token: (')
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2871.
	1:	1	0.000348189415042

Token: (@)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2871.
	1:	1	0.000348189415042


Cluster 2:
Token: [int]
Total number of token occurrences: 17.
Number of records with at least one token occurrence: 16.
StructScore: 8569.
	1:	15	0.00522284122563
	2:	1	0.000348189415042


Possible array tokens:
(/)
Records in possible array context:2872
Total:7383
Coverage:2872
Width:5
Possible array tokens:
[string]
Records in possible array context:2872
Total:7391
Coverage:2872
Width:5
Array	(/)	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 29.
Computed clusters
Cluster 0:
Token: (/)
Total number of token occurrences: 2872.
Number of records with at least one token occurrence: 2872.
StructScore: 0.
	1:	2872	1.0


Junk Tolerance Threshold: 288
Coverage: 2872
Num Tokens: 1
Struct
Coverage:2872
Token count:1
(/)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 46.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 4511.
Number of records with at least one token occurrence: 4511.
StructScore: 0.
	1:	4511	1.0

Token: (/)
Total number of token occurrences: 4511.
Number of records with at least one token occurrence: 4511.
StructScore: 0.
	1:	4511	1.0


Junk Tolerance Threshold: 452
Coverage: 4511
Num Tokens: 2
Struct
Coverage:4511
Token count:2
[string]	Occurrences:1
(/)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 29.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 2880.
Number of records with at least one token occurrence: 2872.
StructScore: 8.
	1:	2864	0.99721448468
	2:	8	0.00278551532033


Cluster 1:
Token: (&)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 2865.
	1:	7	0.00243732590529

Token: (')
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2871.
	1:	1	0.000348189415042

Token: (@)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2871.
	1:	1	0.000348189415042


Cluster 2:
Token: [int]
Total number of token occurrences: 17.
Number of records with at least one token occurrence: 16.
StructScore: 8569.
	1:	15	0.00522284122563
	2:	1	0.000348189415042


Junk Tolerance Threshold: 288
Coverage: 2864
Num Tokens: 1
Struct
Coverage:2864
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 29.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 2856.
Number of records with at least one token occurrence: 2856.
StructScore: 16.
	1:	2856	0.994428969359


Cluster 1:
Token: (')
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2871.
	1:	1	0.000348189415042


Cluster 2:
Token: [int]
Total number of token occurrences: 17.
Number of records with at least one token occurrence: 16.
StructScore: 8569.
	1:	15	0.00522284122563
	2:	1	0.000348189415042


Junk Tolerance Threshold: 288
Coverage: 2856
Num Tokens: 1
Struct
Coverage:2856
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 17.
Number of records with at least one token occurrence: 16.
StructScore: 1.
	1:	15	0.9375
	2:	1	0.0625


Cluster 1:
Token: (')
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 15.
	1:	1	0.0625


Junk Tolerance Threshold: 2
Coverage: 15
Num Tokens: 1
Struct
Coverage:15
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 15.
Number of records with at least one token occurrence: 15.
StructScore: 1.
	1:	15	0.9375


Cluster 1:
Token: [int]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 15.
	1:	1	0.0625

Token: (')
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 15.
	1:	1	0.0625


Junk Tolerance Threshold: 2
Coverage: 15
Num Tokens: 1
Struct
Coverage:15
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0

Token: (')
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0


Junk Tolerance Threshold: 1
Coverage: 1
Num Tokens: 2
Struct
Coverage:1
Token count:2
[int]	Occurrences:1
(')	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 29.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 2864.
Number of records with at least one token occurrence: 2864.
StructScore: 8.
	1:	2864	0.99721448468


Cluster 1:
Token: [string]
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 8.
StructScore: 2864.
	1:	8	0.00278551532033

Token: (&)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 2865.
	1:	7	0.00243732590529

Token: (@)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2871.
	1:	1	0.000348189415042


Junk Tolerance Threshold: 288
Coverage: 2864
Num Tokens: 1
Struct
Coverage:2864
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 8.
StructScore: 0.
	1:	8	1.0


Cluster 1:
Token: (&)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 1.
	1:	7	0.875


Cluster 2:
Token: (@)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 7.
	1:	1	0.125


Junk Tolerance Threshold: 1
Coverage: 8
Num Tokens: 1
Struct
Coverage:8
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (&)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 1.
	1:	7	0.875


Cluster 1:
Token: (@)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 7.
	1:	1	0.125


Junk Tolerance Threshold: 1
Coverage: 7
Num Tokens: 1
Struct
Coverage:7
Token count:1
(&)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (@)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0


Junk Tolerance Threshold: 1
Coverage: 1
Num Tokens: 1
Struct
Coverage:1
Token count:1
(@)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 29.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 2877.
Number of records with at least one token occurrence: 2872.
StructScore: 5.
	1:	2867	0.998259052925
	2:	5	0.00174094707521


Cluster 1:
Token: [int]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2868.
	1:	4	0.00139275766017

Token: (%)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2868.
	1:	4	0.00139275766017

Token: (=)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2868.
	1:	4	0.00139275766017

Token: (*)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 2870.
	2:	2	0.000696378830084

Token: (/)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2871.
	1:	1	0.000348189415042


Junk Tolerance Threshold: 288
Coverage: 2867
Num Tokens: 1
Struct
Coverage:2867
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 29.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 2865.
Number of records with at least one token occurrence: 2865.
StructScore: 7.
	1:	2865	0.997562674095


Cluster 1:
Token: [string]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 2867.
	1:	5	0.00174094707521

Token: [int]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2868.
	1:	4	0.00139275766017

Token: (%)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2868.
	1:	4	0.00139275766017

Token: (=)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2868.
	1:	4	0.00139275766017

Token: (*)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 2870.
	2:	2	0.000696378830084

Token: (/)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2871.
	1:	1	0.000348189415042


Junk Tolerance Threshold: 288
Coverage: 2865
Num Tokens: 1
Struct
Coverage:2865
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 2.
	1:	5	0.714285714286


Cluster 1:
Token: [int]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 3.
	1:	4	0.571428571429

Token: (%)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 3.
	1:	4	0.571428571429

Token: (=)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 3.
	1:	4	0.571428571429


Cluster 2:
Token: (*)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 5.
	2:	2	0.285714285714


Cluster 3:
Token: (/)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 6.
	1:	1	0.142857142857


Junk Tolerance Threshold: 1
Coverage: 5
Num Tokens: 1
Struct
Coverage:5
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 1.
	1:	4	0.8

Token: (%)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 1.
	1:	4	0.8


Cluster 1:
Token: (/)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 4.
	1:	1	0.2


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 2
Struct
Coverage:4
Token count:2
[int]	Occurrences:1
(%)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (/)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0


Junk Tolerance Threshold: 1
Coverage: 1
Num Tokens: 1
Struct
Coverage:1
Token count:1
(/)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (=)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 1.
	1:	4	0.8


Cluster 1:
Token: [empty]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 4.
	1:	1	0.2


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 1
Struct
Coverage:4
Token count:1
(=)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0


Junk Tolerance Threshold: 1
Coverage: 1
Num Tokens: 1
Struct
Coverage:1
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (*)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	2:	2	1.0


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 2
Struct
Coverage:2
Token count:2
(*)	Occurrences:2
Building histograms...
THRESHOLD for histogram equality: 2.
Computed clusters
Cluster 0:
Token: (/)
Total number of token occurrences: 216.
Number of records with at least one token occurrence: 128.
StructScore: 100.
	1:	55	0.4296875
	2:	58	0.453125
	3:	15	0.1171875


Cluster 1:
Token: [string]
Total number of token occurrences: 110.
Number of records with at least one token occurrence: 83.
StructScore: 162.
	1:	56	0.4375
	2:	27	0.2109375


Junk Tolerance Threshold: 13
Coverage: 128
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: (/)
Total number of token occurrences: 216.
Number of records with at least one token occurrence: 128.
StructScore: 100.
	1:	55	0.4296875
	2:	58	0.453125
	3:	15	0.1171875


Cluster 1:
Token: [string]
Total number of token occurrences: 110.
Number of records with at least one token occurrence: 83.
StructScore: 162.
	1:	56	0.4375
	2:	27	0.2109375


Possible array tokens:
(/)
Records in possible array context:128
Total:216
Coverage:128
Width:3
Array	(/)	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 2.
Computed clusters
Cluster 0:
Token: (/)
Total number of token occurrences: 128.
Number of records with at least one token occurrence: 128.
StructScore: 0.
	1:	128	1.0


Junk Tolerance Threshold: 13
Coverage: 128
Num Tokens: 1
Struct
Coverage:128
Token count:1
(/)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 88.
Number of records with at least one token occurrence: 88.
StructScore: 0.
	1:	88	1.0

Token: (/)
Total number of token occurrences: 88.
Number of records with at least one token occurrence: 88.
StructScore: 0.
	1:	88	1.0


Junk Tolerance Threshold: 9
Coverage: 88
Num Tokens: 2
Struct
Coverage:88
Token count:2
[string]	Occurrences:1
(/)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 2.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 106.
Number of records with at least one token occurrence: 106.
StructScore: 22.
	1:	106	0.828125


Cluster 1:
Token: [string]
Total number of token occurrences: 22.
Number of records with at least one token occurrence: 22.
StructScore: 106.
	1:	22	0.171875


Junk Tolerance Threshold: 13
Coverage: 106
Num Tokens: 1
Struct
Coverage:106
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 22.
Number of records with at least one token occurrence: 22.
StructScore: 0.
	1:	22	1.0


Junk Tolerance Threshold: 3
Coverage: 22
Num Tokens: 1
Struct
Coverage:22
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0

Token: (/)
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0


Junk Tolerance Threshold: 300
Coverage: 3000
Num Tokens: 2
Struct
Coverage:3000
Token count:2
[string]	Occurrences:1
(/)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: (.)
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0


Junk Tolerance Threshold: 300
Coverage: 3000
Num Tokens: 1
Struct
Coverage:3000
Token count:1
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0


Junk Tolerance Threshold: 300
Coverage: 3000
Num Tokens: 1
Struct
Coverage:3000
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 2651.
Number of records with at least one token occurrence: 2651.
StructScore: 349.
	1:	2651	0.883666666667


Cluster 1:
Token: (-)
Total number of token occurrences: 349.
Number of records with at least one token occurrence: 349.
StructScore: 2651.
	1:	349	0.116333333333


Junk Tolerance Threshold: 300
Coverage: 2651
Num Tokens: 1
Struct
Coverage:2651
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: (-)
Total number of token occurrences: 349.
Number of records with at least one token occurrence: 349.
StructScore: 0.
	1:	349	1.0


Junk Tolerance Threshold: 35
Coverage: 349
Num Tokens: 1
Struct
Coverage:349
Token count:1
(-)	Occurrences:1

Before reduction:
Pstruct(Id = BTy_110 3000, 0b, 0b)
	Punion(Id = BTy_1 3000, 0b, 0b)
		[IP](Id = BTy_0 1704, 0b, 0b);
		Parray(Id = BTy_2 1296, 0b, 0b)((.) )
		First:
			Punion(Id = BTy_9 1296, 0b, 0b)
				Pstruct(Id = BTy_8 1291, 0b, 0b)
					Punion(Id = BTy_4 1291, 0b, 0b)
						[empty](Id = BTy_3 1246, 0b, 0b);
						[int](Id = BTy_5 45, 0b, 0b);
					End Punion;
					[string](Id = BTy_6 1291, 0b, 0b);
					(.)(Id = BTy_7 1291, 0b, 0b);
				End Pstruct;
				Pstruct(Id = BTy_15 5, 0b, 0b)
					Parray(Id = BTy_10 5, 0b, 0b)([int] )
					First:
						[int](Id = BTy_11 5, 0b, 0b);
					Body:
						[int](Id = BTy_12 8, 0b, 0b);
					Tail:
						[empty](Id = BTy_13 5, 0b, 0b);
					End Parray;
					(.)(Id = BTy_14 5, 0b, 0b);
				End Pstruct;
			End Punion;
		Body:
			Pstruct(Id = BTy_18 2280, 0b, 0b)
				[string](Id = BTy_16 2280, 0b, 0b);
				(.)(Id = BTy_17 2280, 0b, 0b);
			End Pstruct;
		Tail:
			[string](Id = BTy_19 1296, 0b, 0b);
		End Parray;
	End Punion;
	[white space](Id = BTy_20 3000, 0b, 0b);
	(-)(Id = BTy_21 3000, 0b, 0b);
	[white space](Id = BTy_22 3000, 0b, 0b);
	Punion(Id = BTy_24 3000, 0b, 0b)
		(-)(Id = BTy_23 2999, 0b, 0b);
		[string](Id = BTy_25 1, 0b, 0b);
	End Punion;
	[white space](Id = BTy_26 3000, 0b, 0b);
	([)(Id = BTy_27 3000, 0b, 0b);
	[int](Id = BTy_28 3000, 0b, 0b);
	(/)(Id = BTy_29 3000, 0b, 0b);
	[string](Id = BTy_30 3000, 0b, 0b);
	(/)(Id = BTy_31 3000, 0b, 0b);
	[int](Id = BTy_32 3000, 0b, 0b);
	(:)(Id = BTy_33 3000, 0b, 0b);
	[Time](Id = BTy_34 3000, 0b, 0b);
	[white space](Id = BTy_35 3000, 0b, 0b);
	[int](Id = BTy_36 3000, 0b, 0b);
	(])(Id = BTy_37 3000, 0b, 0b);
	[white space](Id = BTy_39 3000, 0b, 0b);
	(\")(Id = BTy_40 3000, 0b, 0b);
	[string](Id = BTy_41 3000, 0b, 0b);
	[white space](Id = BTy_42 3000, 0b, 0b);
	Punion(Id = BTy_86 3000, 0b, 0b)
		Pstruct(Id = BTy_85 2872, 0b, 0b)
			Parray(Id = BTy_43 2872, 0b, 0b)((/) )
			First:
				(/)(Id = BTy_44 2872, 0b, 0b);
			Body:
				Pstruct(Id = BTy_47 4511, 0b, 0b)
					[string](Id = BTy_45 4511, 0b, 0b);
					(/)(Id = BTy_46 4511, 0b, 0b);
				End Pstruct;
			Tail:
				Pstruct(Id = BTy_65 2872, 0b, 0b)
					Punion(Id = BTy_49 2872, 0b, 0b)
						[empty](Id = BTy_48 2856, 0b, 0b);
						Pstruct(Id = BTy_56 16, 0b, 0b)
							[int](Id = BTy_50 16, 0b, 0b);
							Punion(Id = BTy_52 16, 0b, 0b)
								[empty](Id = BTy_51 15, 0b, 0b);
								Pstruct(Id = BTy_55 1, 0b, 0b)
									(')(Id = BTy_53 1, 0b, 0b);
									[int](Id = BTy_54 1, 0b, 0b);
								End Pstruct;
							End Punion;
						End Pstruct;
					End Punion;
					[string](Id = BTy_57 2872, 0b, 0b);
					Punion(Id = BTy_59 2872, 0b, 0b)
						[empty](Id = BTy_58 2864, 0b, 0b);
						Pstruct(Id = BTy_64 8, 0b, 0b)
							Punion(Id = BTy_61 8, 0b, 0b)
								(&)(Id = BTy_60 7, 0b, 0b);
								(@)(Id = BTy_62 1, 0b, 0b);
							End Punion;
							[string](Id = BTy_63 8, 0b, 0b);
						End Pstruct;
					End Punion;
				End Pstruct;
			End Parray;
			(.)(Id = BTy_66 2872, 0b, 0b);
			[string](Id = BTy_67 2872, 0b, 0b);
			Punion(Id = BTy_69 2872, 0b, 0b)
				[empty](Id = BTy_68 2865, 0b, 0b);
				Pstruct(Id = BTy_79 5, 0b, 0b)
					Punion(Id = BTy_73 5, 0b, 0b)
						Pstruct(Id = BTy_72 4, 0b, 0b)
							(%)(Id = BTy_70 4, 0b, 0b);
							[int](Id = BTy_71 4, 0b, 0b);
						End Pstruct;
						(/)(Id = BTy_74 1, 0b, 0b);
					End Punion;
					[string](Id = BTy_75 5, 0b, 0b);
					Punion(Id = BTy_77 5, 0b, 0b)
						(=)(Id = BTy_76 4, 0b, 0b);
						[empty](Id = BTy_78 1, 0b, 0b);
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_83 2, 0b, 0b)
					(*)(Id = BTy_81 2, 0b, 0b);
					(*)(Id = BTy_82 2, 0b, 0b);
				End Pstruct;
			End Punion;
		End Pstruct;
		Parray(Id = BTy_87 128, 0b, 0b)((/) )
		First:
			(/)(Id = BTy_88 128, 0b, 0b);
		Body:
			Pstruct(Id = BTy_91 88, 0b, 0b)
				[string](Id = BTy_89 88, 0b, 0b);
				(/)(Id = BTy_90 88, 0b, 0b);
			End Pstruct;
		Tail:
			Punion(Id = BTy_93 128, 0b, 0b)
				[empty](Id = BTy_92 106, 0b, 0b);
				[string](Id = BTy_94 22, 0b, 0b);
			End Punion;
		End Parray;
	End Punion;
	[white space](Id = BTy_95 3000, 0b, 0b);
	[string](Id = BTy_96 3000, 0b, 0b);
	(/)(Id = BTy_97 3000, 0b, 0b);
	[int](Id = BTy_99 3000, 0b, 0b);
	(.)(Id = BTy_100 3000, 0b, 0b);
	[int](Id = BTy_101 3000, 0b, 0b);
	(\")(Id = BTy_102 3000, 0b, 0b);
	[white space](Id = BTy_104 3000, 0b, 0b);
	[int](Id = BTy_105 3000, 0b, 0b);
	[white space](Id = BTy_106 3000, 0b, 0b);
	Punion(Id = BTy_108 3000, 0b, 0b)
		[int](Id = BTy_107 2651, 0b, 0b);
		(-)(Id = BTy_109 349, 0b, 0b);
	End Punion;
End Pstruct


After final reduction:
Pstruct(Id = BTy_110 3000, 0b, 0b)
	Punion(Id = BTy_1 3000, 0b, 0b)
		[IP](Id = BTy_0 1704, 0b, 0b);
		Pstruct(Id = BTy_112 1296, 0b, 0b)
			Punion(Id = BTy_9 1296, 0b, 0b)
				Pstruct(Id = BTy_8 1291, 0b, 0b)
					Switch(BTy_6)(Id = BTy_4 1291, 0b, 0b):
					case "*":
						""(Id = BTy_3 1246, 0b, 0b);
					case {"cust108", "cust230", "cust40", "inf121", }:
						[1...130](Id = BTy_5 45, 0b, 0b);
					End Switch;
					[string](Id = BTy_6 1291, 0b, 0b);
					"."(Id = BTy_7 1291, 0b, 0b);
				End Pstruct;
				Pstruct(Id = BTy_15 5, 0b, 0b)
					RArray(Id = BTy_10 5, 0b, 0b)
						Terminator: "."
						[~164](Id = BTy_12 8, 0b, 0b);
					End RArray;
					"."(Id = BTy_14 5, 0b, 0b);
					[142...171](Id = BTy_11 5, 0b, 0b);
				End Pstruct;
			End Punion;
			RArray(Id = BTy_2 1296, 0b, 0b)
				Separator: "."
				[string](Id = BTy_16 3576, 0b, 0b);
			End RArray;
		End Pstruct;
	End Punion;
	" - "(Id = BTy_20 3000, 0b, 0b);
	Punion(Id = BTy_24 3000, 0b, 0b)
		"-"(Id = BTy_23 2999, 0b, 0b);
		"amnesty"(Id = BTy_25 1, 0b, 0b);
	End Punion;
	" ["(Id = BTy_26 3000, 0b, 0b);
	[15...16](Id = BTy_28 3000, 0b, 0b);
	"/Oct/"(Id = BTy_29 3000, 0b, 0b);
	[1997](Id = BTy_32 3000, 0b, 0b);
	":"(Id = BTy_33 3000, 0b, 0b);
	[Time](Id = BTy_34 3000, 0b, 0b);
	" "(Id = BTy_35 3000, 0b, 0b);
	[~700](Id = BTy_36 3000, 0b, 0b);
	"] \""(Id = BTy_37 3000, 0b, 0b);
	{"GET", "POST", }(Id = BTy_41 3000, 0b, 0b);
	" /"(Id = BTy_42 3000, 0b, 0b);
	RArray(Id = BTy_87 3000, 0b, 0b)
		Pstruct(Id = BTy_91 4599, 0b, 0b)
			[string](Id = BTy_89 4599, 0b, 0b);
			"/"(Id = BTy_90 4599, 0b, 0b);
		End Pstruct;
	End RArray;
	Punion(Id = BTy_86 3000, 0b, 0b)
		Pstruct(Id = BTy_85 2872, 0b, 0b)
			Switch(BTy_57)(Id = BTy_49 2872, 0b, 0b):
			case "*":
				""(Id = BTy_48 2856, 0b, 0b);
			case {"candle", "uaup", "ways", }:
				Pstruct(Id = BTy_56 16, 0b, 0b)
					[3...8](Id = BTy_50 16, 0b, 0b);
					Punion(Id = BTy_52 16, 0b, 0b)
						""(Id = BTy_51 15, 0b, 0b);
						Pstruct(Id = BTy_55 1, 0b, 0b)
							"'"(Id = BTy_53 1, 0b, 0b);
							[5](Id = BTy_54 1, 0b, 0b);
						End Pstruct;
					End Punion;
				End Pstruct;
			End Switch;
			[string](Id = BTy_57 2872, 0b, 0b);
			Punion(Id = BTy_59 2872, 0b, 0b)
				""(Id = BTy_58 2864, 0b, 0b);
				Pstruct(Id = BTy_64 8, 0b, 0b)
					Switch(BTy_63)(Id = BTy_61 8, 0b, 0b):
					case "a":
						"&"(Id = BTy_60 7, 0b, 0b);
					case "aiusa":
						"@"(Id = BTy_62 1, 0b, 0b);
					End Switch;
					{"a", "aiusa", }(Id = BTy_63 8, 0b, 0b);
				End Pstruct;
			End Punion;
			"."(Id = BTy_66 2872, 0b, 0b);
			[string](Id = BTy_67 2872, 0b, 0b);
			Punion(Id = BTy_69 2872, 0b, 0b)
				""(Id = BTy_68 2865, 0b, 0b);
				Pstruct(Id = BTy_79 5, 0b, 0b)
					Switch(BTy_75)(Id = BTy_73 5, 0b, 0b):
					case "ALT":
						Pstruct(Id = BTy_72 4, 0b, 0b)
							"%"(Id = BTy_70 4, 0b, 0b);
							[20](Id = BTy_71 4, 0b, 0b);
						End Pstruct;
					case "confirm":
						"/"(Id = BTy_74 1, 0b, 0b);
					End Switch;
					{"ALT", "confirm", }(Id = BTy_75 5, 0b, 0b);
					Punion(Id = BTy_77 5, 0b, 0b)
						"="(Id = BTy_76 4, 0b, 0b);
						""(Id = BTy_78 1, 0b, 0b);
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_83 2, 0b, 0b)
					"**"(Id = BTy_81 2, 0b, 0b);
				End Pstruct;
			End Punion;
		End Pstruct;
		Pstruct(Id = BTy_116 128, 0b, 0b)
			Punion(Id = BTy_93 128, 0b, 0b)
				""(Id = BTy_92 106, 0b, 0b);
				[string](Id = BTy_94 22, 0b, 0b);
			End Punion;
		End Pstruct;
	End Punion;
	" HTTP/"(Id = BTy_95 3000, 0b, 0b);
	[1](Id = BTy_99 3000, 0b, 0b);
	"."(Id = BTy_100 3000, 0b, 0b);
	[0...1](Id = BTy_101 3000, 0b, 0b);
	"\" "(Id = BTy_102 3000, 0b, 0b);
	[200...404](Id = BTy_105 3000, 0b, 0b);
	" "(Id = BTy_106 3000, 0b, 0b);
	Punion(Id = BTy_108 3000, 0b, 0b)
		[35...37947](Id = BTy_107 2651, 0b, 0b);
		"-"(Id = BTy_109 349, 0b, 0b);
	End Punion;
End Pstruct

Complexity of inferred type:
	numAlt = 8  numTBD = 0  numBtm = 0
Outputing partitions to directory: gen/

Completed data/ai.3000
Overall type complexity = 77.585b
Overall data complexity = 2700.296b

