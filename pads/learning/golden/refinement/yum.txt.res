Source file to process: data/yum.txt
Output directory: gen/
Max depth to explore: 50
Print line numbers in output contexts: false
Print ids and output type tokens: true
Print Entropy: false
Histogram comparison tolerance (percentage): 0.01
Struct determination tolerance (percentage): 0.1
Noise level threshold (percentage): 0.0
Minimum width threshold for array: 2
Junk threshold (percentage): 0.1
Starting on file data/yum.txt
328 records.
Histogram of number of tokens per record:
	8:	11
	13:	4
	14:	11
	15:	18
	16:	1
	17:	62
	18:	5
	19:	102
	20:	7
	21:	82
	22:	4
	23:	16
	25:	5

Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [Date]
Total number of token occurrences: 328.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	1:	328	1.0

Token: [other](:)
Total number of token occurrences: 328.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	1:	328	1.0


Cluster 1:
Token: [white]
Total number of token occurrences: 1301.
Number of records with at least one token occurrence: 328.
StructScore: 11.
	3:	11	0.0335365853659
	4:	317	0.966463414634


Cluster 2:
Token: [Time]
Total number of token occurrences: 353.
Number of records with at least one token occurrence: 328.
StructScore: 25.
	1:	303	0.923780487805
	2:	25	0.0762195121951


Cluster 3:
Token: [Host]
Total number of token occurrences: 25.
Number of records with at least one token occurrence: 25.
StructScore: 303.
	1:	25	0.0762195121951


Cluster 4:
Token: [string]
Total number of token occurrences: 1151.
Number of records with at least one token occurrence: 328.
StructScore: 315.
	2:	21	0.0640243902439
	3:	138	0.420731707317
	4:	150	0.457317073171
	5:	19	0.0579268292683


Cluster 5:
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 319.
	1:	9	0.0274390243902

Token: [other](_)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 321.
	1:	7	0.0213414634146


Cluster 6:
Token: [other](+)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 3.
StructScore: 325.
	2:	3	0.00914634146341


Cluster 7:
Token: [int]
Total number of token occurrences: 1355.
Number of records with at least one token occurrence: 317.
StructScore: 925.
	1:	5	0.015243902439
	2:	10	0.030487804878
	3:	51	0.155487804878
	4:	103	0.314024390244
	5:	127	0.387195121951
	6:	17	0.0518292682927
	7:	4	0.0121951219512


Cluster 8:
Token: [other](.)
Total number of token occurrences: 1242.
Number of records with at least one token occurrence: 317.
StructScore: 1238.
	1:	15	0.0457317073171
	2:	22	0.0670731707317
	3:	71	0.216463414634
	4:	102	0.310975609756
	5:	85	0.259146341463
	6:	17	0.0518292682927
	7:	5	0.015243902439


Junk Tolerance Threshold: 33
Coverage: 328
Num Tokens: 2
Struct
Coverage:328
Token count:2
[Date]	Occurrences:1
[other](:)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [Time]
Total number of token occurrences: 328.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	1:	328	1.0

Token: [string]
Total number of token occurrences: 328.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	1:	328	1.0

Token: [white]
Total number of token occurrences: 656.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	2:	328	1.0


Junk Tolerance Threshold: 33
Coverage: 328
Num Tokens: 4
Struct
Coverage:328
Token count:4
[Time]	Occurrences:1
[string]	Occurrences:1
[white]	Occurrences:2
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [white]
Total number of token occurrences: 645.
Number of records with at least one token occurrence: 328.
StructScore: 11.
	1:	11	0.0335365853659
	2:	317	0.966463414634


Cluster 1:
Token: [Time]
Total number of token occurrences: 25.
Number of records with at least one token occurrence: 25.
StructScore: 303.
	1:	25	0.0762195121951

Token: [Host]
Total number of token occurrences: 25.
Number of records with at least one token occurrence: 25.
StructScore: 303.
	1:	25	0.0762195121951


Cluster 2:
Token: [string]
Total number of token occurrences: 823.
Number of records with at least one token occurrence: 328.
StructScore: 315.
	1:	21	0.0640243902439
	2:	138	0.420731707317
	3:	150	0.457317073171
	4:	19	0.0579268292683


Cluster 3:
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 319.
	1:	9	0.0274390243902

Token: [other](_)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 321.
	1:	7	0.0213414634146


Cluster 4:
Token: [other](+)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 3.
StructScore: 325.
	2:	3	0.00914634146341


Cluster 5:
Token: [int]
Total number of token occurrences: 1355.
Number of records with at least one token occurrence: 317.
StructScore: 925.
	1:	5	0.015243902439
	2:	10	0.030487804878
	3:	51	0.155487804878
	4:	103	0.314024390244
	5:	127	0.387195121951
	6:	17	0.0518292682927
	7:	4	0.0121951219512


Cluster 6:
Token: [other](.)
Total number of token occurrences: 1242.
Number of records with at least one token occurrence: 317.
StructScore: 1238.
	1:	15	0.0457317073171
	2:	22	0.0670731707317
	3:	71	0.216463414634
	4:	102	0.310975609756
	5:	85	0.259146341463
	6:	17	0.0518292682927
	7:	5	0.015243902439


Junk Tolerance Threshold: 33
Coverage: 317
Num Tokens: 2
Struct
Coverage:317
Token count:2
[white]	Occurrences:2
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 610.
Number of records with at least one token occurrence: 317.
StructScore: 24.
	1:	24	0.0757097791798
	2:	293	0.92429022082

Token: [other](.)
Total number of token occurrences: 293.
Number of records with at least one token occurrence: 293.
StructScore: 24.
	1:	293	0.92429022082


Cluster 1:
Token: [Host]
Total number of token occurrences: 24.
Number of records with at least one token occurrence: 24.
StructScore: 293.
	1:	24	0.0757097791798


Cluster 2:
Token: [other](+)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 3.
StructScore: 314.
	2:	3	0.00946372239748


Junk Tolerance Threshold: 32
Coverage: 293
Num Tokens: 3
Struct
Coverage:293
Token count:3
[string]	Occurrences:2
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 290.
Number of records with at least one token occurrence: 290.
StructScore: 3.
	1:	290	0.98976109215


Cluster 1:
Token: [other](+)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 3.
StructScore: 290.
	2:	3	0.0102389078498


Junk Tolerance Threshold: 30
Coverage: 290
Num Tokens: 1
Struct
Coverage:290
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](+)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 3.
StructScore: 0.
	2:	3	1.0


Junk Tolerance Threshold: 1
Coverage: 3
Num Tokens: 2
Struct
Coverage:3
Token count:2
[other](+)	Occurrences:2
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Host]
Total number of token occurrences: 24.
Number of records with at least one token occurrence: 24.
StructScore: 0.
	1:	24	1.0

Token: [string]
Total number of token occurrences: 24.
Number of records with at least one token occurrence: 24.
StructScore: 0.
	1:	24	1.0


Junk Tolerance Threshold: 3
Coverage: 24
Num Tokens: 2
Struct
Coverage:24
Token count:2
[Host]	Occurrences:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [Time]
Total number of token occurrences: 25.
Number of records with at least one token occurrence: 25.
StructScore: 292.
	1:	25	0.0788643533123


Cluster 1:
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 308.
	1:	9	0.0283911671924

Token: [other](_)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 310.
	1:	7	0.0220820189274


Cluster 2:
Token: [Host]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 316.
	1:	1	0.00315457413249


Cluster 3:
Token: [string]
Total number of token occurrences: 202.
Number of records with at least one token occurrence: 177.
StructScore: 445.
	1:	152	0.479495268139
	2:	25	0.0788643533123


Cluster 4:
Token: [int]
Total number of token occurrences: 1355.
Number of records with at least one token occurrence: 317.
StructScore: 617.
	1:	5	0.0157728706625
	2:	10	0.0315457413249
	3:	51	0.160883280757
	4:	103	0.324921135647
	5:	127	0.400630914826
	6:	17	0.0536277602524
	7:	4	0.01261829653


Cluster 5:
Token: [other](.)
Total number of token occurrences: 949.
Number of records with at least one token occurrence: 313.
StructScore: 810.
	1:	32	0.10094637224
	2:	66	0.208201892744
	3:	102	0.321766561514
	4:	91	0.287066246057
	5:	17	0.0536277602524
	6:	5	0.0157728706625


Junk Tolerance Threshold: 32
Coverage: 25
Num Tokens: 1
Clusters sorted by array criteria:
Cluster 0:
Token: [int]
Total number of token occurrences: 1355.
Number of records with at least one token occurrence: 317.
StructScore: 617.
	1:	5	0.0157728706625
	2:	10	0.0315457413249
	3:	51	0.160883280757
	4:	103	0.324921135647
	5:	127	0.400630914826
	6:	17	0.0536277602524
	7:	4	0.01261829653


Cluster 1:
Token: [other](.)
Total number of token occurrences: 949.
Number of records with at least one token occurrence: 313.
StructScore: 810.
	1:	32	0.10094637224
	2:	66	0.208201892744
	3:	102	0.321766561514
	4:	91	0.287066246057
	5:	17	0.0536277602524
	6:	5	0.0157728706625


Cluster 2:
Token: [string]
Total number of token occurrences: 202.
Number of records with at least one token occurrence: 177.
StructScore: 445.
	1:	152	0.479495268139
	2:	25	0.0788643533123


Cluster 3:
Token: [Time]
Total number of token occurrences: 25.
Number of records with at least one token occurrence: 25.
StructScore: 292.
	1:	25	0.0788643533123


Cluster 4:
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 308.
	1:	9	0.0283911671924

Token: [other](_)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 310.
	1:	7	0.0220820189274


Cluster 5:
Token: [Host]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 316.
	1:	1	0.00315457413249


Possible array tokens:
[int]
Records in possible array context:317
Total:1355
Coverage:317
Width:7
Array	[int]	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 317.
Number of records with at least one token occurrence: 317.
StructScore: 0.
	1:	317	1.0


Cluster 1:
Token: [Time]
Total number of token occurrences: 25.
Number of records with at least one token occurrence: 25.
StructScore: 292.
	1:	25	0.0788643533123


Cluster 2:
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 308.
	1:	9	0.0283911671924


Cluster 3:
Token: [string]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 315.
	1:	2	0.00630914826498

Token: [Host]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 316.
	1:	1	0.00315457413249


Cluster 4:
Token: [other](.)
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 27.
StructScore: 871.
	1:	26	0.0820189274448
	2:	1	0.00315457413249


Junk Tolerance Threshold: 32
Coverage: 317
Num Tokens: 1
Struct
Coverage:317
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 282.
Number of records with at least one token occurrence: 282.
StructScore: 35.
	1:	282	0.889589905363


Cluster 1:
Token: [Time]
Total number of token occurrences: 25.
Number of records with at least one token occurrence: 25.
StructScore: 292.
	1:	25	0.0788643533123


Cluster 2:
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 308.
	1:	9	0.0283911671924


Cluster 3:
Token: [string]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 315.
	1:	2	0.00630914826498

Token: [Host]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 316.
	1:	1	0.00315457413249


Cluster 4:
Token: [other](.)
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 27.
StructScore: 871.
	1:	26	0.0820189274448
	2:	1	0.00315457413249


Junk Tolerance Threshold: 32
Coverage: 282
Num Tokens: 1
Struct
Coverage:282
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Time]
Total number of token occurrences: 25.
Number of records with at least one token occurrence: 25.
StructScore: 10.
	1:	25	0.714285714286


Cluster 1:
Token: [other](.)
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 27.
StructScore: 25.
	1:	26	0.742857142857
	2:	1	0.0285714285714


Cluster 2:
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 26.
	1:	9	0.257142857143


Cluster 3:
Token: [string]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 33.
	1:	2	0.0571428571429


Cluster 4:
Token: [Host]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 34.
	1:	1	0.0285714285714


Junk Tolerance Threshold: 4
Coverage: 25
Num Tokens: 1
Struct
Coverage:25
Token count:1
[Time]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](.)
Total number of token occurrences: 24.
Number of records with at least one token occurrence: 24.
StructScore: 1.
	1:	24	0.96


Cluster 1:
Token: [empty]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 24.
	1:	1	0.04


Junk Tolerance Threshold: 3
Coverage: 24
Num Tokens: 1
Struct
Coverage:24
Token count:1
[other](.)	Occurrences:1
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
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 1.
	1:	9	0.9


Cluster 1:
Token: [string]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 8.
	1:	2	0.2


Cluster 2:
Token: [Host]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 9.
	1:	1	0.1


Cluster 3:
Token: [other](.)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 3.
StructScore: 22.
	1:	2	0.2
	2:	1	0.1


Junk Tolerance Threshold: 1
Coverage: 9
Num Tokens: 1
Struct
Coverage:9
Token count:1
[IP]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 2.
	1:	7	0.777777777778


Cluster 1:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111


Cluster 2:
Token: [other](.)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 2.
StructScore: 22.
	1:	1	0.111111111111
	2:	1	0.111111111111


Junk Tolerance Threshold: 1
Coverage: 7
Num Tokens: 1
Struct
Coverage:7
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1.
	1:	1	0.5

Token: [other](.)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 2.
StructScore: 1.
	1:	1	0.5
	2:	1	0.5


Junk Tolerance Threshold: 1
Coverage: 1
Num Tokens: 1
Struct
Coverage:1
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](.)
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
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](.)
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
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](.)
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
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Host]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0

Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0

Token: [other](.)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0


Junk Tolerance Threshold: 1
Coverage: 1
Num Tokens: 3
Struct
Coverage:1
Token count:3
[Host]	Occurrences:1
[string]	Occurrences:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 11.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 1038.
Number of records with at least one token occurrence: 1038.
StructScore: 0.
	1:	1038	1.0


Cluster 1:
Token: [other](.)
Total number of token occurrences: 843.
Number of records with at least one token occurrence: 746.
StructScore: 973.
	1:	649	0.625240847784
	2:	97	0.0934489402697


Cluster 2:
Token: [other](_)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1037.
	1:	1	0.000963391136802


Cluster 3:
Token: [string]
Total number of token occurrences: 112.
Number of records with at least one token occurrence: 107.
StructScore: 2798.
	1:	102	0.0982658959538
	2:	5	0.00481695568401


Junk Tolerance Threshold: 104
Coverage: 1038
Num Tokens: 1
Struct
Coverage:1038
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 11.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 291.
Number of records with at least one token occurrence: 291.
StructScore: 747.
	1:	291	0.280346820809


Cluster 1:
Token: [other](.)
Total number of token occurrences: 843.
Number of records with at least one token occurrence: 746.
StructScore: 973.
	1:	649	0.625240847784
	2:	97	0.0934489402697


Cluster 2:
Token: [other](_)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1037.
	1:	1	0.000963391136802


Cluster 3:
Token: [string]
Total number of token occurrences: 112.
Number of records with at least one token occurrence: 107.
StructScore: 2798.
	1:	102	0.0982658959538
	2:	5	0.00481695568401


Junk Tolerance Threshold: 104
Coverage: 291
Num Tokens: 1
Struct
Coverage:291
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 8.
Computed clusters
Cluster 0:
Token: [other](.)
Total number of token occurrences: 843.
Number of records with at least one token occurrence: 746.
StructScore: 100.
	1:	649	0.868808567604
	2:	97	0.129852744311


Cluster 1:
Token: [other](_)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 746.
	1:	1	0.00133868808568


Cluster 2:
Token: [string]
Total number of token occurrences: 112.
Number of records with at least one token occurrence: 107.
StructScore: 1925.
	1:	102	0.136546184739
	2:	5	0.00669344042838


Junk Tolerance Threshold: 75
Coverage: 747
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [other](.)
Total number of token occurrences: 843.
Number of records with at least one token occurrence: 746.
StructScore: 100.
	1:	649	0.868808567604
	2:	97	0.129852744311


Cluster 1:
Token: [string]
Total number of token occurrences: 112.
Number of records with at least one token occurrence: 107.
StructScore: 1925.
	1:	102	0.136546184739
	2:	5	0.00669344042838


Cluster 2:
Token: [other](_)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 746.
	1:	1	0.00133868808568


Possible array tokens:
[other](.)
Records in possible array context:747
Total:843
Coverage:746
Width:2
Array	[other](.)	Occurrences:1
WARNING: ARRAY first context empty!Array context
Building histograms...
THRESHOLD for histogram equality: 8.
Computed clusters
Cluster 0:
Token: [other](.)
Total number of token occurrences: 746.
Number of records with at least one token occurrence: 746.
StructScore: 1.
	1:	746	0.998661311914


Cluster 1:
Token: [string]
Total number of token occurrences: 15.
Number of records with at least one token occurrence: 15.
StructScore: 732.
	1:	15	0.0200803212851


Cluster 2:
Token: [empty]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 746.
	1:	1	0.00133868808568


Junk Tolerance Threshold: 75
Coverage: 746
Num Tokens: 1
Struct
Coverage:746
Token count:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 8.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 731.
Number of records with at least one token occurrence: 731.
StructScore: 15.
	1:	731	0.979892761394


Cluster 1:
Token: [string]
Total number of token occurrences: 15.
Number of records with at least one token occurrence: 15.
StructScore: 731.
	1:	15	0.0201072386059


Junk Tolerance Threshold: 75
Coverage: 731
Num Tokens: 1
Struct
Coverage:731
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 15.
Number of records with at least one token occurrence: 15.
StructScore: 0.
	1:	15	1.0


Junk Tolerance Threshold: 2
Coverage: 15
Num Tokens: 1
Struct
Coverage:15
Token count:1
[string]	Occurrences:1
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
Token: [string]
Total number of token occurrences: 97.
Number of records with at least one token occurrence: 97.
StructScore: 0.
	1:	97	1.0

Token: [other](.)
Total number of token occurrences: 97.
Number of records with at least one token occurrence: 97.
StructScore: 0.
	1:	97	1.0


Junk Tolerance Threshold: 10
Coverage: 97
Num Tokens: 2
Struct
Coverage:97
Token count:2
[string]	Occurrences:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 8.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 746.
Number of records with at least one token occurrence: 746.
StructScore: 1.
	1:	746	0.998661311914


Cluster 1:
Token: [other](_)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 746.
	1:	1	0.00133868808568


Junk Tolerance Threshold: 75
Coverage: 746
Num Tokens: 1
Struct
Coverage:746
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](_)
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
[other](_)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 246.
Number of records with at least one token occurrence: 246.
StructScore: 71.
	1:	246	0.776025236593


Cluster 1:
Token: [other](_)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 311.
	1:	6	0.018927444795


Cluster 2:
Token: [string]
Total number of token occurrences: 88.
Number of records with at least one token occurrence: 71.
StructScore: 755.
	1:	54	0.170347003155
	2:	17	0.0536277602524


Cluster 3:
Token: [other](.)
Total number of token occurrences: 78.
Number of records with at least one token occurrence: 62.
StructScore: 781.
	1:	46	0.145110410095
	2:	16	0.0504731861199


Junk Tolerance Threshold: 32
Coverage: 246
Num Tokens: 1
Struct
Coverage:246
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 88.
Number of records with at least one token occurrence: 71.
StructScore: 17.
	1:	54	0.760563380282
	2:	17	0.239436619718


Cluster 1:
Token: [other](.)
Total number of token occurrences: 78.
Number of records with at least one token occurrence: 62.
StructScore: 43.
	1:	46	0.647887323944
	2:	16	0.225352112676


Cluster 2:
Token: [other](_)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 65.
	1:	6	0.0845070422535


Junk Tolerance Threshold: 8
Coverage: 71
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [string]
Total number of token occurrences: 88.
Number of records with at least one token occurrence: 71.
StructScore: 17.
	1:	54	0.760563380282
	2:	17	0.239436619718


Cluster 1:
Token: [other](.)
Total number of token occurrences: 78.
Number of records with at least one token occurrence: 62.
StructScore: 43.
	1:	46	0.647887323944
	2:	16	0.225352112676


Cluster 2:
Token: [other](_)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 65.
	1:	6	0.0845070422535


Possible array tokens:
[string]
Records in possible array context:71
Total:88
Coverage:71
Width:2
ARRAY NOT CHOSEN
Union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 4.
StructScore: 1.
	1:	3	0.75
	2:	1	0.25


Cluster 1:
Token: [other](.)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 3.
	1:	1	0.25


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [string]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 4.
StructScore: 1.
	1:	3	0.75
	2:	1	0.25


Cluster 1:
Token: [other](.)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 3.
	1:	1	0.25


Possible array tokens:
[string]
Records in possible array context:4
Total:5
Coverage:4
Width:2
ARRAY NOT CHOSEN
Union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	1:	4	1.0


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 1
Struct
Coverage:4
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 3.
StructScore: 1.
	1:	3	0.75


Cluster 1:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 3.
	1:	1	0.25

Token: [other](.)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 3.
	1:	1	0.25


Junk Tolerance Threshold: 1
Coverage: 3
Num Tokens: 1
Struct
Coverage:3
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0

Token: [other](.)
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
[string]	Occurrences:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 77.
Number of records with at least one token occurrence: 61.
StructScore: 16.
	1:	45	0.737704918033
	2:	16	0.262295081967

Token: [other](.)
Total number of token occurrences: 77.
Number of records with at least one token occurrence: 61.
StructScore: 16.
	1:	45	0.737704918033
	2:	16	0.262295081967


Junk Tolerance Threshold: 7
Coverage: 61
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [string]
Total number of token occurrences: 77.
Number of records with at least one token occurrence: 61.
StructScore: 16.
	1:	45	0.737704918033
	2:	16	0.262295081967

Token: [other](.)
Total number of token occurrences: 77.
Number of records with at least one token occurrence: 61.
StructScore: 16.
	1:	45	0.737704918033
	2:	16	0.262295081967


Possible array tokens:
[string]
Records in possible array context:61
Total:77
Coverage:61
Width:2
Possible array tokens:
[other](.)
Records in possible array context:61
Total:77
Coverage:61
Width:2
Array	[other](.)	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](.)
Total number of token occurrences: 61.
Number of records with at least one token occurrence: 61.
StructScore: 0.
	1:	61	1.0


Junk Tolerance Threshold: 7
Coverage: 61
Num Tokens: 1
Struct
Coverage:61
Token count:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 16.
StructScore: 0.
	1:	16	1.0

Token: [other](.)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 16.
StructScore: 0.
	1:	16	1.0


Junk Tolerance Threshold: 2
Coverage: 16
Num Tokens: 2
Struct
Coverage:16
Token count:2
[string]	Occurrences:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 61.
Number of records with at least one token occurrence: 61.
StructScore: 0.
	1:	61	1.0


Junk Tolerance Threshold: 7
Coverage: 61
Num Tokens: 1
Struct
Coverage:61
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 0.
	1:	6	1.0

Token: [other](_)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 0.
	1:	6	1.0


Junk Tolerance Threshold: 1
Coverage: 6
Num Tokens: 2
Struct
Coverage:6
Token count:2
[string]	Occurrences:1
[other](_)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 11.
StructScore: 0.
	1:	11	1.0

Token: [white]
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 11.
StructScore: 0.
	1:	11	1.0


Junk Tolerance Threshold: 2
Coverage: 11
Num Tokens: 2
Struct
Coverage:11
Token count:2
[string]	Occurrences:1
[white]	Occurrences:1

Before reduction:
Pstruct(Id = BTy_90 328, 1b, 0b)
	[Date](Id = BTy_0 328, 1b, 0b);
	[white](Id = BTy_1 328, 1b, 0b);
	[Time](Id = BTy_2 328, 1b, 0b);
	[white](Id = BTy_3 328, 1b, 0b);
	[string](Id = BTy_4 328, 1b, 0b);
	[other](:)(Id = BTy_6 328, 1b, 0b);
	Punion(Id = BTy_86 328, 1b, 0b)
		Pstruct(Id = BTy_85 317, 1b, 0b)
			[white](Id = BTy_7 317, 1b, 0b);
			Punion(Id = BTy_17 317, 1b, 0b)
				Pstruct(Id = BTy_16 293, 1b, 0b)
					[string](Id = BTy_8 293, 1b, 0b);
					Punion(Id = BTy_10 293, 1b, 0b)
						[empty](Id = BTy_9 290, 1b, 0b);
						Pstruct(Id = BTy_13 3, 1b, 0b)
							[other](+)(Id = BTy_11 3, 1b, 0b);
							[other](+)(Id = BTy_12 3, 1b, 0b);
						End Pstruct;
					End Punion;
					[other](.)(Id = BTy_14 293, 1b, 0b);
					[string](Id = BTy_15 293, 1b, 0b);
				End Pstruct;
				Pstruct(Id = BTy_20 24, 1b, 0b)
					[Host](Id = BTy_18 24, 1b, 0b);
					[string](Id = BTy_19 24, 1b, 0b);
				End Pstruct;
			End Punion;
			[white](Id = BTy_21 317, 1b, 0b);
			Parray(Id = BTy_22 317, 1b, 0b)([int] )
			First:
				Pstruct(Id = BTy_47 317, 1b, 0b)
					Punion(Id = BTy_24 317, 1b, 0b)
						[empty](Id = BTy_23 282, 1b, 0b);
						Pstruct(Id = BTy_29 25, 1b, 0b)
							[Time](Id = BTy_25 25, 1b, 0b);
							Punion(Id = BTy_27 25, 1b, 0b)
								[other](.)(Id = BTy_26 24, 1b, 0b);
								[empty](Id = BTy_28 1, 1b, 0b);
							End Punion;
						End Pstruct;
						Pstruct(Id = BTy_40 9, 1b, 0b)
							[IP](Id = BTy_31 9, 1b, 0b);
							Punion(Id = BTy_33 9, 1b, 0b)
								[empty](Id = BTy_32 7, 1b, 0b);
								Pstruct(Id = BTy_37 1, 1b, 0b)
									[other](.)(Id = BTy_34 1, 1b, 0b);
									[string](Id = BTy_35 1, 1b, 0b);
									[other](.)(Id = BTy_36 1, 1b, 0b);
								End Pstruct;
								[other](.)(Id = BTy_39 1, 1b, 0b);
							End Punion;
						End Pstruct;
						Pstruct(Id = BTy_45 1, 1b, 0b)
							[Host](Id = BTy_42 1, 1b, 0b);
							[string](Id = BTy_43 1, 1b, 0b);
							[other](.)(Id = BTy_44 1, 1b, 0b);
						End Pstruct;
					End Punion;
					[int](Id = BTy_46 317, 1b, 0b);
				End Pstruct;
			Body:
				Pstruct(Id = BTy_65 1038, 1b, 0b)
					Punion(Id = BTy_49 1038, 1b, 0b)
						[empty](Id = BTy_48 291, 1b, 0b);
						Parray(Id = BTy_50 747, 1b, 0b)([other](.) )
						First:
							Punion(Id = BTy_56 747, 1b, 0b)
								Pstruct(Id = BTy_55 746, 1b, 0b)
									Punion(Id = BTy_52 746, 1b, 0b)
										[empty](Id = BTy_51 731, 1b, 0b);
										[string](Id = BTy_53 15, 1b, 0b);
									End Punion;
									[other](.)(Id = BTy_54 746, 1b, 0b);
								End Pstruct;
								[empty](Id = BTy_57 1, 1b, 0b);
							End Punion;
						Body:
							Pstruct(Id = BTy_60 97, 1b, 0b)
								[string](Id = BTy_58 97, 1b, 0b);
								[other](.)(Id = BTy_59 97, 1b, 0b);
							End Pstruct;
						Tail:
							Punion(Id = BTy_62 747, 1b, 0b)
								[empty](Id = BTy_61 746, 1b, 0b);
								[other](_)(Id = BTy_63 1, 1b, 0b);
							End Punion;
						End Parray;
					End Punion;
					[int](Id = BTy_64 1038, 1b, 0b);
				End Pstruct;
			Tail:
				Punion(Id = BTy_67 317, 1b, 0b)
					[empty](Id = BTy_66 246, 1b, 0b);
					Pstruct(Id = BTy_68 4, 1b, 0b)
						[string](Id = BTy_69 4, 1b, 0b);
						Punion(Id = BTy_71 4, 1b, 0b)
							[empty](Id = BTy_70 3, 1b, 0b);
							Pstruct(Id = BTy_74 1, 1b, 0b)
								[other](.)(Id = BTy_72 1, 1b, 0b);
								[string](Id = BTy_73 1, 1b, 0b);
							End Pstruct;
						End Punion;
					End Pstruct;
					Parray(Id = BTy_75 61, 1b, 0b)([other](.) )
					First:
						[other](.)(Id = BTy_76 61, 1b, 0b);
					Body:
						Pstruct(Id = BTy_79 16, 1b, 0b)
							[string](Id = BTy_77 16, 1b, 0b);
							[other](.)(Id = BTy_78 16, 1b, 0b);
						End Pstruct;
					Tail:
						[string](Id = BTy_80 61, 1b, 0b);
					End Parray;
					Pstruct(Id = BTy_83 6, 1b, 0b)
						[other](_)(Id = BTy_81 6, 1b, 0b);
						[string](Id = BTy_82 6, 1b, 0b);
					End Pstruct;
				End Punion;
			End Parray;
		End Pstruct;
		Pstruct(Id = BTy_89 11, 1b, 0b)
			[white](Id = BTy_87 11, 1b, 0b);
			[string](Id = BTy_88 11, 1b, 0b);
		End Pstruct;
	End Punion;
End Pstruct


After final reduction:
Pstruct(Id = BTy_90 328, 1b, 0b)
	[Date](Id = BTy_0 328, 1b, 0b);
	[StringConst] " "(Id = BTy_1 328, 1b, 0b);
	[Time](Id = BTy_2 328, 1b, 0b);
	[StringConst] " "(Id = BTy_3 328, 1b, 0b);
	[Enum] {[StringConst] "Erased", [StringConst] "Installed", [StringConst] "Updated", }(Id = BTy_4 328, 1b, 0b);
	[StringConst] ":"(Id = BTy_6 328, 1b, 0b);
	Switch(BTy_4)(Id = BTy_86 328, 1b, 0b):
	case [Enum] {[StringConst] "Installed", [StringConst] "Updated", }:
		Pstruct(Id = BTy_85 317, 1b, 0b)
			[StringConst] " "(Id = BTy_7 317, 1b, 0b);
			Punion(Id = BTy_17 317, 1b, 0b)
				Pstruct(Id = BTy_16 293, 1b, 0b)
					[string](Id = BTy_8 293, 1b, 0b);
					Switch(BTy_8)(Id = BTy_10 293, 1b, 0b):
					case [StringConst] "*":
						[StringConst] ""(Id = BTy_9 290, 1b, 0b);
					case [StringConst] "libstdc":
						Pstruct(Id = BTy_13 3, 1b, 0b)
							[StringConst] "++"(Id = BTy_11 3, 1b, 0b);
						End Pstruct;
					End Switch;
					[StringConst] "."(Id = BTy_14 293, 1b, 0b);
					[Enum] {[StringConst] "i386", [StringConst] "i686", [StringConst] "x86_64", }(Id = BTy_15 293, 1b, 0b);
				End Pstruct;
				Pstruct(Id = BTy_20 24, 1b, 0b)
					[Host](Id = BTy_18 24, 1b, 0b);
					[StringConst] "arch"(Id = BTy_19 24, 1b, 0b);
				End Pstruct;
			End Punion;
			[StringConst] " "(Id = BTy_21 317, 1b, 0b);
			Punion(Id = BTy_24 317, 1b, 0b)
				[StringConst] ""(Id = BTy_23 282, 1b, 0b);
				Pstruct(Id = BTy_29 25, 1b, 0b)
					[Time](Id = BTy_25 25, 1b, 0b);
					Punion(Id = BTy_27 25, 1b, 0b)
						[StringConst] "."(Id = BTy_26 24, 1b, 0b);
						[StringConst] ""(Id = BTy_28 1, 1b, 0b);
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_40 9, 1b, 0b)
					[IP](Id = BTy_31 9, 1b, 0b);
					Punion(Id = BTy_33 9, 1b, 0b)
						[StringConst] ""(Id = BTy_32 7, 1b, 0b);
						Pstruct(Id = BTy_37 1, 1b, 0b)
							[StringConst] ".EL."(Id = BTy_34 1, 1b, 0b);
						End Pstruct;
						[StringConst] "."(Id = BTy_39 1, 1b, 0b);
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_45 1, 1b, 0b)
					[StringConst] "3.1-0.pre5."(Id = BTy_42 1, 1b, 0b);
				End Pstruct;
			End Punion;
			[Int] [~14...20020927](Id = BTy_46 317, 1b, 0b);
			RArray(Id = BTy_22 317, 1b, 0b)
				Pstruct(Id = BTy_65 1038, 1b, 0b)
					Punion(Id = BTy_49 1038, 1b, 0b)
						[StringConst] ""(Id = BTy_48 291, 1b, 0b);
						Pstruct(Id = BTy_92 747, 1b, 0b)
							Punion(Id = BTy_56 747, 1b, 0b)
								Pstruct(Id = BTy_55 746, 1b, 0b)
									Punion(Id = BTy_52 746, 1b, 0b)
										[StringConst] ""(Id = BTy_51 731, 1b, 0b);
										[string](Id = BTy_53 15, 1b, 0b);
									End Punion;
									[StringConst] "."(Id = BTy_54 746, 1b, 0b);
								End Pstruct;
								[StringConst] ""(Id = BTy_57 1, 1b, 0b);
							End Punion;
							RArray(Id = BTy_50 747, 1b, 0b)
								Pstruct(Id = BTy_60 97, 1b, 0b)
									[string](Id = BTy_58 97, 1b, 0b);
									[StringConst] "."(Id = BTy_59 97, 1b, 0b);
								End Pstruct;
							End RArray;
							Punion(Id = BTy_62 747, 1b, 0b)
								[StringConst] ""(Id = BTy_61 746, 1b, 0b);
								[StringConst] "_"(Id = BTy_63 1, 1b, 0b);
							End Punion;
						End Pstruct;
					End Punion;
					[Int] [~44...9004](Id = BTy_64 1038, 1b, 0b);
				End Pstruct;
			End RArray;
			Punion(Id = BTy_67 317, 1b, 0b)
				[StringConst] ""(Id = BTy_66 246, 1b, 0b);
				Pstruct(Id = BTy_68 4, 1b, 0b)
					[string](Id = BTy_69 4, 1b, 0b);
					Punion(Id = BTy_71 4, 1b, 0b)
						[StringConst] ""(Id = BTy_70 3, 1b, 0b);
						Pstruct(Id = BTy_74 1, 1b, 0b)
							[StringConst] ".EL4"(Id = BTy_72 1, 1b, 0b);
						End Pstruct;
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_93 61, 1b, 0b)
					[StringConst] "."(Id = BTy_76 61, 1b, 0b);
					RArray(Id = BTy_75 61, 1b, 0b)
						Separator: [StringConst] "."
						[string](Id = BTy_77 77, 1b, 0b);
					End RArray;
				End Pstruct;
				Pstruct(Id = BTy_83 6, 1b, 0b)
					[StringConst] "_"(Id = BTy_81 6, 1b, 0b);
					[Enum] {[StringConst] "EL4", [StringConst] "nonptl", }(Id = BTy_82 6, 1b, 0b);
				End Pstruct;
			End Punion;
		End Pstruct;
	case [StringConst] "Erased":
		Pstruct(Id = BTy_89 11, 1b, 0b)
			[StringConst] " "(Id = BTy_87 11, 1b, 0b);
			[string](Id = BTy_88 11, 1b, 0b);
		End Pstruct;
	End Switch;
End Pstruct

Complexity of inferred type:
	numAlt = 10  numTBD = 0  numBtm = 0
Outputing partitions to directory: gen/

Completed data/yum.txt
Overall type complexity = 62.585b
Overall data complexity = 2769.510b

