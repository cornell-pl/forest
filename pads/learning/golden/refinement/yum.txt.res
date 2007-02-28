Source file to process: ../golden/data/yum.txt
Output directory: gen/
Max depth to explore: 50
Print line numbers in output contexts: false
Print ids and output type tokens: true
Histogram comparison tolerance (percentage): 0.01
Struct determination tolerance (percentage): 0.1
Noise level threshold (percentage): 0.0
Minimum width threshold for array: 2
Junk threshold (percentage): 0.1
Starting on file ../golden/data/yum.txt
328 records.
Histogram of number of tokens per record:
	10:	11
	15:	4
	17:	29
	19:	61
	20:	1
	21:	107
	22:	1
	23:	89
	24:	4
	25:	16
	27:	5

Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: (:)
Total number of token occurrences: 328.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	1:	328	1.0


Cluster 1:
Token: [white space]
Total number of token occurrences: 1629.
Number of records with at least one token occurrence: 328.
StructScore: 11.
	4:	11	0.0335365853659
	5:	317	0.966463414634


Cluster 2:
Token: [Time]
Total number of token occurrences: 353.
Number of records with at least one token occurrence: 328.
StructScore: 25.
	1:	303	0.923780487805
	2:	25	0.0762195121951


Cluster 3:
Token: [string]
Total number of token occurrences: 1503.
Number of records with at least one token occurrence: 328.
StructScore: 281.
	3:	11	0.0335365853659
	4:	140	0.426829268293
	5:	152	0.463414634146
	6:	25	0.0762195121951


Cluster 4:
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 319.
	1:	9	0.0274390243902

Token: (_)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 321.
	1:	7	0.0213414634146


Cluster 5:
Token: (+)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 3.
StructScore: 325.
	2:	3	0.00914634146341


Cluster 6:
Token: [int]
Total number of token occurrences: 1686.
Number of records with at least one token occurrence: 328.
StructScore: 820.
	1:	11	0.0335365853659
	2:	5	0.015243902439
	3:	9	0.0274390243902
	4:	51	0.155487804878
	5:	103	0.314024390244
	6:	128	0.390243902439
	7:	17	0.0518292682927
	8:	4	0.0121951219512


Cluster 7:
Token: (.)
Total number of token occurrences: 1268.
Number of records with at least one token occurrence: 317.
StructScore: 1116.
	1:	4	0.0121951219512
	2:	32	0.0975609756098
	3:	65	0.198170731707
	4:	102	0.310975609756
	5:	92	0.280487804878
	6:	17	0.0518292682927
	7:	5	0.015243902439


Junk Tolerance Threshold: 33
Coverage: 328
Num Tokens: 1
Struct
Coverage:328
Token count:1
(:)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [Time]
Total number of token occurrences: 328.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	1:	328	1.0

Token: [int]
Total number of token occurrences: 328.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	1:	328	1.0

Token: [string]
Total number of token occurrences: 656.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	2:	328	1.0

Token: [white space]
Total number of token occurrences: 984.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	3:	328	1.0


Junk Tolerance Threshold: 33
Coverage: 328
Num Tokens: 7
Struct
Coverage:328
Token count:7
[Time]	Occurrences:1
[int]	Occurrences:1
[string]	Occurrences:2
[white space]	Occurrences:3
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [white space]
Total number of token occurrences: 645.
Number of records with at least one token occurrence: 328.
StructScore: 11.
	1:	11	0.0335365853659
	2:	317	0.966463414634


Cluster 1:
Token: [string]
Total number of token occurrences: 847.
Number of records with at least one token occurrence: 328.
StructScore: 281.
	1:	11	0.0335365853659
	2:	140	0.426829268293
	3:	152	0.463414634146
	4:	25	0.0762195121951


Cluster 2:
Token: [Time]
Total number of token occurrences: 25.
Number of records with at least one token occurrence: 25.
StructScore: 303.
	1:	25	0.0762195121951


Cluster 3:
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 319.
	1:	9	0.0274390243902

Token: (_)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 321.
	1:	7	0.0213414634146


Cluster 4:
Token: (+)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 3.
StructScore: 325.
	2:	3	0.00914634146341


Cluster 5:
Token: [int]
Total number of token occurrences: 1358.
Number of records with at least one token occurrence: 317.
StructScore: 915.
	1:	5	0.015243902439
	2:	9	0.0274390243902
	3:	51	0.155487804878
	4:	103	0.314024390244
	5:	128	0.390243902439
	6:	17	0.0518292682927
	7:	4	0.0121951219512


Cluster 6:
Token: (.)
Total number of token occurrences: 1268.
Number of records with at least one token occurrence: 317.
StructScore: 1116.
	1:	4	0.0121951219512
	2:	32	0.0975609756098
	3:	65	0.198170731707
	4:	102	0.310975609756
	5:	92	0.280487804878
	6:	17	0.0518292682927
	7:	5	0.015243902439


Junk Tolerance Threshold: 33
Coverage: 317
Num Tokens: 2
Struct
Coverage:317
Token count:2
[white space]	Occurrences:2
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 634.
Number of records with at least one token occurrence: 317.
StructScore: 0.
	2:	317	1.0

Token: (.)
Total number of token occurrences: 317.
Number of records with at least one token occurrence: 317.
StructScore: 0.
	1:	317	1.0


Cluster 1:
Token: (+)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 3.
StructScore: 314.
	2:	3	0.00946372239748


Junk Tolerance Threshold: 32
Coverage: 317
Num Tokens: 3
Struct
Coverage:317
Token count:3
[string]	Occurrences:2
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 4.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 314.
Number of records with at least one token occurrence: 314.
StructScore: 3.
	1:	314	0.990536277603


Cluster 1:
Token: (+)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 3.
StructScore: 314.
	2:	3	0.00946372239748


Junk Tolerance Threshold: 32
Coverage: 314
Num Tokens: 1
Struct
Coverage:314
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (+)
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
(+)	Occurrences:2
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

Token: (_)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 310.
	1:	7	0.0220820189274


Cluster 2:
Token: [string]
Total number of token occurrences: 202.
Number of records with at least one token occurrence: 177.
StructScore: 445.
	1:	152	0.479495268139
	2:	25	0.0788643533123


Cluster 3:
Token: [int]
Total number of token occurrences: 1358.
Number of records with at least one token occurrence: 317.
StructScore: 607.
	1:	5	0.0157728706625
	2:	9	0.0283911671924
	3:	51	0.160883280757
	4:	103	0.324921135647
	5:	128	0.403785488959
	6:	17	0.0536277602524
	7:	4	0.01261829653


Cluster 4:
Token: (.)
Total number of token occurrences: 951.
Number of records with at least one token occurrence: 313.
StructScore: 808.
	1:	32	0.10094637224
	2:	65	0.205047318612
	3:	102	0.321766561514
	4:	92	0.290220820189
	5:	17	0.0536277602524
	6:	5	0.0157728706625


Junk Tolerance Threshold: 32
Coverage: 25
Num Tokens: 1
Clusters sorted by array criteria:
Cluster 0:
Token: [int]
Total number of token occurrences: 1358.
Number of records with at least one token occurrence: 317.
StructScore: 607.
	1:	5	0.0157728706625
	2:	9	0.0283911671924
	3:	51	0.160883280757
	4:	103	0.324921135647
	5:	128	0.403785488959
	6:	17	0.0536277602524
	7:	4	0.01261829653


Cluster 1:
Token: (.)
Total number of token occurrences: 951.
Number of records with at least one token occurrence: 313.
StructScore: 808.
	1:	32	0.10094637224
	2:	65	0.205047318612
	3:	102	0.321766561514
	4:	92	0.290220820189
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

Token: (_)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 310.
	1:	7	0.0220820189274


Possible array tokens:
[int]
Records in possible array context:317
Total:1358
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
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 316.
	1:	1	0.00315457413249


Cluster 4:
Token: (.)
Total number of token occurrences: 27.
Number of records with at least one token occurrence: 26.
StructScore: 874.
	1:	25	0.0788643533123
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
Total number of token occurrences: 283.
Number of records with at least one token occurrence: 283.
StructScore: 34.
	1:	283	0.892744479495


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
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 316.
	1:	1	0.00315457413249


Cluster 4:
Token: (.)
Total number of token occurrences: 27.
Number of records with at least one token occurrence: 26.
StructScore: 874.
	1:	25	0.0788643533123
	2:	1	0.00315457413249


Junk Tolerance Threshold: 32
Coverage: 283
Num Tokens: 1
Struct
Coverage:283
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Time]
Total number of token occurrences: 25.
Number of records with at least one token occurrence: 25.
StructScore: 9.
	1:	25	0.735294117647


Cluster 1:
Token: [IP]
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 25.
	1:	9	0.264705882353

Token: (.)
Total number of token occurrences: 27.
Number of records with at least one token occurrence: 26.
StructScore: 25.
	1:	25	0.735294117647
	2:	1	0.0294117647059


Cluster 2:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 33.
	1:	1	0.0294117647059


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
Token: (.)
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
(.)	Occurrences:1
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
StructScore: 0.
	1:	9	1.0


Cluster 1:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111


Cluster 2:
Token: (.)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 2.
StructScore: 22.
	1:	1	0.111111111111
	2:	1	0.111111111111


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
Token: (.)
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

Token: (.)
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
Token: (.)
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
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (.)
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
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (.)
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
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 11.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 1041.
Number of records with at least one token occurrence: 1041.
StructScore: 0.
	1:	1041	1.0


Cluster 1:
Token: (.)
Total number of token occurrences: 846.
Number of records with at least one token occurrence: 748.
StructScore: 977.
	1:	650	0.624399615754
	2:	98	0.0941402497598


Cluster 2:
Token: (_)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1040.
	1:	1	0.000960614793468


Cluster 3:
Token: [string]
Total number of token occurrences: 113.
Number of records with at least one token occurrence: 108.
StructScore: 2804.
	1:	103	0.0989433237272
	2:	5	0.00480307396734


Junk Tolerance Threshold: 105
Coverage: 1041
Num Tokens: 1
Struct
Coverage:1041
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 11.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 292.
Number of records with at least one token occurrence: 292.
StructScore: 749.
	1:	292	0.280499519693


Cluster 1:
Token: (.)
Total number of token occurrences: 846.
Number of records with at least one token occurrence: 748.
StructScore: 977.
	1:	650	0.624399615754
	2:	98	0.0941402497598


Cluster 2:
Token: (_)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1040.
	1:	1	0.000960614793468


Cluster 3:
Token: [string]
Total number of token occurrences: 113.
Number of records with at least one token occurrence: 108.
StructScore: 2804.
	1:	103	0.0989433237272
	2:	5	0.00480307396734


Junk Tolerance Threshold: 105
Coverage: 292
Num Tokens: 1
Struct
Coverage:292
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 8.
Computed clusters
Cluster 0:
Token: (.)
Total number of token occurrences: 846.
Number of records with at least one token occurrence: 748.
StructScore: 101.
	1:	650	0.86782376502
	2:	98	0.130841121495


Cluster 1:
Token: (_)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 748.
	1:	1	0.00133511348465


Cluster 2:
Token: [string]
Total number of token occurrences: 113.
Number of records with at least one token occurrence: 108.
StructScore: 1928.
	1:	103	0.137516688919
	2:	5	0.00667556742323


Junk Tolerance Threshold: 75
Coverage: 749
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: (.)
Total number of token occurrences: 846.
Number of records with at least one token occurrence: 748.
StructScore: 101.
	1:	650	0.86782376502
	2:	98	0.130841121495


Cluster 1:
Token: [string]
Total number of token occurrences: 113.
Number of records with at least one token occurrence: 108.
StructScore: 1928.
	1:	103	0.137516688919
	2:	5	0.00667556742323


Cluster 2:
Token: (_)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 748.
	1:	1	0.00133511348465


Possible array tokens:
(.)
Records in possible array context:749
Total:846
Coverage:748
Width:2
Array	(.)	Occurrences:1
WARNING: ARRAY first context empty!Array context
Building histograms...
THRESHOLD for histogram equality: 8.
Computed clusters
Cluster 0:
Token: (.)
Total number of token occurrences: 748.
Number of records with at least one token occurrence: 748.
StructScore: 1.
	1:	748	0.998664886515


Cluster 1:
Token: [string]
Total number of token occurrences: 15.
Number of records with at least one token occurrence: 15.
StructScore: 734.
	1:	15	0.0200267022697


Cluster 2:
Token: [empty]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 748.
	1:	1	0.00133511348465


Junk Tolerance Threshold: 75
Coverage: 748
Num Tokens: 1
Struct
Coverage:748
Token count:1
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 8.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 733.
Number of records with at least one token occurrence: 733.
StructScore: 15.
	1:	733	0.979946524064


Cluster 1:
Token: [string]
Total number of token occurrences: 15.
Number of records with at least one token occurrence: 15.
StructScore: 733.
	1:	15	0.0200534759358


Junk Tolerance Threshold: 75
Coverage: 733
Num Tokens: 1
Struct
Coverage:733
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
Total number of token occurrences: 98.
Number of records with at least one token occurrence: 98.
StructScore: 0.
	1:	98	1.0

Token: (.)
Total number of token occurrences: 98.
Number of records with at least one token occurrence: 98.
StructScore: 0.
	1:	98	1.0


Junk Tolerance Threshold: 10
Coverage: 98
Num Tokens: 2
Struct
Coverage:98
Token count:2
[string]	Occurrences:1
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 8.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 748.
Number of records with at least one token occurrence: 748.
StructScore: 1.
	1:	748	0.998664886515


Cluster 1:
Token: (_)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 748.
	1:	1	0.00133511348465


Junk Tolerance Threshold: 75
Coverage: 748
Num Tokens: 1
Struct
Coverage:748
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (_)
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
(_)	Occurrences:1
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
Token: (_)
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
Token: (.)
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
Token: (.)
Total number of token occurrences: 78.
Number of records with at least one token occurrence: 62.
StructScore: 43.
	1:	46	0.647887323944
	2:	16	0.225352112676


Cluster 2:
Token: (_)
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
Token: (.)
Total number of token occurrences: 78.
Number of records with at least one token occurrence: 62.
StructScore: 43.
	1:	46	0.647887323944
	2:	16	0.225352112676


Cluster 2:
Token: (_)
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
Token: (.)
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
Token: (.)
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

Token: (.)
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

Token: (.)
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
(.)	Occurrences:1
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

Token: (.)
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

Token: (.)
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
(.)
Records in possible array context:61
Total:77
Coverage:61
Width:2
Array	(.)	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (.)
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
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 16.
StructScore: 0.
	1:	16	1.0

Token: (.)
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
(.)	Occurrences:1
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

Token: (_)
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
(_)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 11.
StructScore: 0.
	1:	11	1.0

Token: [white space]
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
[white space]	Occurrences:1

Before reduction:
Pstruct(Id = BTy_83 328)
	[string](Id = BTy_0 328);
	[white space](Id = BTy_1 328);
	[int](Id = BTy_2 328);
	[white space](Id = BTy_3 328);
	[Time](Id = BTy_4 328);
	[white space](Id = BTy_5 328);
	[string](Id = BTy_6 328);
	(:)(Id = BTy_8 328);
	Punion(Id = BTy_79 328)
		Pstruct(Id = BTy_78 317)
			[white space](Id = BTy_9 317);
			[string](Id = BTy_10 317);
			Punion(Id = BTy_12 317)
				[empty](Id = BTy_11 314);
				Pstruct(Id = BTy_15 3)
					(+)(Id = BTy_13 3);
					(+)(Id = BTy_14 3);
				End Pstruct;
			End Punion;
			(.)(Id = BTy_16 317);
			[string](Id = BTy_17 317);
			[white space](Id = BTy_19 317);
			Parray(Id = BTy_20 317)([int] )
			First:
				Pstruct(Id = BTy_40 317)
					Punion(Id = BTy_22 317)
						[empty](Id = BTy_21 283);
						Pstruct(Id = BTy_27 25)
							[Time](Id = BTy_23 25);
							Punion(Id = BTy_25 25)
								(.)(Id = BTy_24 24);
								[empty](Id = BTy_26 1);
							End Punion;
						End Pstruct;
						Pstruct(Id = BTy_38 9)
							[IP](Id = BTy_29 9);
							Punion(Id = BTy_31 9)
								[empty](Id = BTy_30 7);
								Pstruct(Id = BTy_35 1)
									(.)(Id = BTy_32 1);
									[string](Id = BTy_33 1);
									(.)(Id = BTy_34 1);
								End Pstruct;
								(.)(Id = BTy_37 1);
							End Punion;
						End Pstruct;
					End Punion;
					[int](Id = BTy_39 317);
				End Pstruct;
			Body:
				Pstruct(Id = BTy_58 1041)
					Punion(Id = BTy_42 1041)
						[empty](Id = BTy_41 292);
						Parray(Id = BTy_43 749)((.) )
						First:
							Punion(Id = BTy_49 749)
								Pstruct(Id = BTy_48 748)
									Punion(Id = BTy_45 748)
										[empty](Id = BTy_44 733);
										[string](Id = BTy_46 15);
									End Punion;
									(.)(Id = BTy_47 748);
								End Pstruct;
								[empty](Id = BTy_50 1);
							End Punion;
						Body:
							Pstruct(Id = BTy_53 98)
								[string](Id = BTy_51 98);
								(.)(Id = BTy_52 98);
							End Pstruct;
						Tail:
							Punion(Id = BTy_55 749)
								[empty](Id = BTy_54 748);
								(_)(Id = BTy_56 1);
							End Punion;
						End Parray;
					End Punion;
					[int](Id = BTy_57 1041);
				End Pstruct;
			Tail:
				Punion(Id = BTy_60 317)
					[empty](Id = BTy_59 246);
					Pstruct(Id = BTy_61 4)
						[string](Id = BTy_62 4);
						Punion(Id = BTy_64 4)
							[empty](Id = BTy_63 3);
							Pstruct(Id = BTy_67 1)
								(.)(Id = BTy_65 1);
								[string](Id = BTy_66 1);
							End Pstruct;
						End Punion;
					End Pstruct;
					Parray(Id = BTy_68 61)((.) )
					First:
						(.)(Id = BTy_69 61);
					Body:
						Pstruct(Id = BTy_72 16)
							[string](Id = BTy_70 16);
							(.)(Id = BTy_71 16);
						End Pstruct;
					Tail:
						[string](Id = BTy_73 61);
					End Parray;
					Pstruct(Id = BTy_76 6)
						(_)(Id = BTy_74 6);
						[string](Id = BTy_75 6);
					End Pstruct;
				End Punion;
			End Parray;
		End Pstruct;
		Pstruct(Id = BTy_82 11)
			[white space](Id = BTy_80 11);
			[string](Id = BTy_81 11);
		End Pstruct;
	End Punion;
End Pstruct

BTy_0	BTy_1	BTy_2	BTy_3	BTy_4	BTy_5	BTy_6	BTy_8	BTy_9	BTy_10	BTy_11	BTy_13	BTy_14	BTy_12	BTy_16	BTy_17	BTy_19	BTy_20	BTy_21	BTy_23	BTy_24	BTy_26	BTy_25	BTy_29	BTy_30	BTy_32	BTy_33	BTy_34	BTy_37	BTy_31	BTy_22	BTy_39	BTy_41	BTy_43	BTy_44	BTy_46	BTy_45	BTy_47	BTy_50	BTy_49	BTy_51	BTy_52	BTy_54	BTy_56	BTy_55	BTy_42	BTy_57	BTy_59	BTy_62	BTy_63	BTy_65	BTy_66	BTy_64	BTy_68	BTy_69	BTy_70	BTy_71	BTy_73	BTy_74	BTy_75	BTy_60	BTy_80	BTy_81	BTy_79	
Dec        [ ]        10         [ ]        04:07:50   [ ]        Updated    :          [ ]        pam        []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          77         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:07:51   [ ]        Updated    :          [ ]        openssl    []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          NONE       1          []         a-43       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:07:51   [ ]        Updated    :          [ ]        openldap   []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:07:52   [ ]        Updated    :          [ ]        wget       []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          10         NONE       E          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          NONE       NONE       1          
Dec        [ ]        10         [ ]        04:07:52   [ ]        Updated    :          [ ]        util-linux []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          NONE       1          []         a-16       1          .          NONE       1          EL4        .          NONE       NONE       NONE       2          12         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:07:58   [ ]        Installed  :          [ ]        kernel     []         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          6          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL         NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        10         [ ]        04:07:58   [ ]        Updated    :          [ ]        shadow-utils[]         NONE       NONE       1          .          x86_64     [ ]        7          NONE       2:4        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       RHEL4      NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        10         [ ]        04:07:58   [ ]        Updated    :          [ ]        nss_ldap   []         NONE       NONE       1          .          x86_64     [ ]        2          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          226        []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:16   [ ]        Installed  :          [ ]        perl-DBI   []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          40         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:16   [ ]        Installed  :          [ ]        zlib       []         NONE       NONE       1          .          i386       [ ]        3          NONE       NONE       NONE       NONE       NONE       1.2.1.2    NONE       NONE       NONE       NONE       NONE       NONE       3          ~1         NONE       1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:17   [ ]        Installed  :          [ ]        postgresql-libs[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:17   [ ]        Installed  :          [ ]        postgresql []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:18   [ ]        Installed  :          [ ]        e2fsprogs  []         NONE       NONE       1          .          i386       [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          35         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:18   [ ]        Installed  :          [ ]        krb5-libs  []         NONE       NONE       1          .          i386       [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:18   [ ]        Installed  :          [ ]        openssl    []         NONE       NONE       1          .          i686       [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          NONE       1          []         a-43       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:18   [ ]        Installed  :          [ ]        postgresql-libs[]         NONE       NONE       1          .          i386       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:19   [ ]        Installed  :          [ ]        mysql      []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:20   [ ]        Installed  :          [ ]        postgresql-server[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:20   [ ]        Installed  :          [ ]        libgcc     []         NONE       NONE       1          .          i386       [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:20   [ ]        Installed  :          [ ]        xorg-x11-font-utils[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:22   [ ]        Installed  :          [ ]        ncurses    []         NONE       NONE       1          .          i386       [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          5          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:22   [ ]        Installed  :          [ ]        mysqlclient10[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          23         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:22   [ ]        Installed  :          [ ]        tcl        []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          8          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:22   [ ]        Installed  :          [ ]        perl-DBD-MySQL[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          9004       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:22   [ ]        Installed  :          [ ]        libstdc    NONE       +          +          2          .          i386       [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:23   [ ]        Installed  :          [ ]        e2fsprogs-devel[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          35         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:23   [ ]        Installed  :          [ ]        krb5-devel []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:23   [ ]        Installed  :          [ ]        zlib-devel []         NONE       NONE       1          .          x86_64     [ ]        3          NONE       NONE       NONE       NONE       NONE       1.2.1.2    NONE       NONE       NONE       NONE       NONE       NONE       3          ~1         NONE       1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:24   [ ]        Installed  :          [ ]        openssl-devel[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          NONE       1          []         a-43       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:26   [ ]        Installed  :          [ ]        emacs-common[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          21         []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:26   [ ]        Installed  :          [ ]        libungif   []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          el4        .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:26   [ ]        Installed  :          [ ]        Xaw3d      []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          5          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:26   [ ]        Installed  :          [ ]        libtool-libs[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          5          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:26   [ ]        Installed  :          [ ]        unixODBC   []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:26   [ ]        Installed  :          [ ]        mx         []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:26   [ ]        Installed  :          [ ]        ttmkfdir   []         NONE       NONE       1          .          x86_64     [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL         NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:26   [ ]        Installed  :          [ ]        xorg-x11-xfs[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:26   [ ]        Installed  :          [ ]        chkfontpath[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          10         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:27   [ ]        Installed  :          [ ]        fonts-xorg-75dpi[]         NONE       NONE       1          .          noarch     [ ]        3          NONE       NONE       NONE       NONE       NONE       6.8.1.1    NONE       NONE       NONE       NONE       NONE       NONE       3          ~1         NONE       2          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       2          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:27   [ ]        Installed  :          [ ]        mysqlclient10-devel[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          23         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:28   [ ]        Installed  :          [ ]        mysqlclient10[]         NONE       NONE       1          .          i386       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          23         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:28   [ ]        Installed  :          [ ]        postgresql-python[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:28   [ ]        Installed  :          [ ]        mysql-bench[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:28   [ ]        Installed  :          [ ]        postgresql-tcl[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:29   [ ]        Installed  :          [ ]        mysql      []         NONE       NONE       1          .          i386       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:29   [ ]        Installed  :          [ ]        postgresql-test[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:29   [ ]        Installed  :          [ ]        postgresql-contrib[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:29   [ ]        Installed  :          [ ]        screen     []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:30   [ ]        Installed  :          [ ]        postgresql-docs[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:30   [ ]        Installed  :          [ ]        emacs      []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          21         []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:31   [ ]        Installed  :          [ ]        postgresql-devel[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:31   [ ]        Installed  :          [ ]        postgresql-pl[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:31   [ ]        Installed  :          [ ]        postgresql-odbc[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:31   [ ]        Installed  :          [ ]        mysql-devel[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:31   [ ]        Installed  :          [ ]        postgresql-jdbc[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:11:32   [ ]        Installed  :          [ ]        mysql-server[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:18   [ ]        Installed  :          [ ]        apr        []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:18   [ ]        Installed  :          [ ]        apr-util   []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:18   [ ]        Installed  :          [ ]        perl-URI   []         NONE       NONE       1          .          noarch     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          30         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:18   [ ]        Installed  :          [ ]        neon       []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          24         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:19   [ ]        Installed  :          [ ]        cpp        []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:19   [ ]        Installed  :          [ ]        umb-scheme []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:19   [ ]        Installed  :          [ ]        guile      []         NONE       NONE       1          .          x86_64     [ ]        5          NONE       5:1        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          6          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:20   [ ]        Installed  :          [ ]        swig       []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:21   [ ]        Installed  :          [ ]        glibc-kernheaders[]         NONE       NONE       1          .          x86_64     [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL         NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:22   [ ]        Installed  :          [ ]        glibc-headers[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:22   [ ]        Installed  :          [ ]        glibc-devel[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:23   [ ]        Installed  :          [ ]        gcc        []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:12:24   [ ]        Installed  :          [ ]        subversion []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       ent        NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        10         [ ]        04:20:43   [ ]        Installed  :          [ ]        lynx       []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        10         [ ]        04:21:19   [ ]        Installed  :          [ ]        nmap       []         NONE       NONE       1          .          x86_64     [ ]        4          NONE       2:3        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          70         []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        00:46:30   [ ]        Installed  :          [ ]        bind       []         NONE       NONE       1          .          x86_64     [ ]        5          NONE       20:9       .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        00:46:31   [ ]        Installed  :          [ ]        bind-devel []         NONE       NONE       1          .          x86_64     [ ]        5          NONE       20:9       .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        00:46:32   [ ]        Installed  :          [ ]        bind-libs  []         NONE       NONE       1          .          i386       [ ]        5          NONE       20:9       .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        00:46:32   [ ]        Installed  :          [ ]        bind-chroot[]         NONE       NONE       1          .          x86_64     [ ]        5          NONE       20:9       .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:04:52   [ ]        Installed  :          [ ]        libidn     []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          5          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:04:53   [ ]        Installed  :          [ ]        curl       []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          12         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       rhel4      NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        13         [ ]        18:04:54   [ ]        Installed  :          [ ]        httpd      []         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       3          .          ent        .          centos4    NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        13         [ ]        18:04:55   [ ]        Installed  :          [ ]        php        []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:04:55   [ ]        Installed  :          [ ]        php-mysql  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:04:55   [ ]        Installed  :          [ ]        php-pgsql  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:04:55   [ ]        Installed  :          [ ]        httpd-suexec[]         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       3          .          ent        .          centos4    NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        13         [ ]        18:04:56   [ ]        Installed  :          [ ]        php-pear   []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:04:56   [ ]        Installed  :          [ ]        php-mbstring[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:05:20   [ ]        Installed  :          [ ]        apr-devel  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:05:22   [ ]        Installed  :          [ ]        db4-devel  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:05:22   [ ]        Installed  :          [ ]        cyrus-sasl-devel[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        13         [ ]        18:05:23   [ ]        Installed  :          [ ]        openldap-devel[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:05:23   [ ]        Installed  :          [ ]        expat-devel[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          95         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:05:23   [ ]        Installed  :          [ ]        apr-util-devel[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:05:23   [ ]        Installed  :          [ ]        pcre-devel []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          5          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       RHEL4      NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        13         [ ]        18:05:24   [ ]        Installed  :          [ ]        httpd-manual[]         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       3          .          ent        .          centos4    NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        13         [ ]        18:05:24   [ ]        Installed  :          [ ]        httpd-devel[]         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       3          .          ent        .          centos4    NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        13         [ ]        18:05:36   [ ]        Installed  :          [ ]        postfix    []         NONE       NONE       1          .          x86_64     [ ]        8          NONE       2:2        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          5          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       RHEL4      NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        13         [ ]        18:24:21   [ ]        Installed  :          [ ]        gd         []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        18:24:21   [ ]        Installed  :          [ ]        webalizer  []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       _          2          1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        13         [ ]        19:25:14   [ ]        Installed  :          [ ]        mod_dav_svn[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       ent        NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        13         [ ]        22:07:23   [ ]        Installed  :          [ ]        dovecot    []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          99         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        15         [ ]        06:06:33   [ ]        Installed  :          [ ]        ImageMagick[]         NONE       NONE       1          .          x86_64     [ ]        2          NONE       NONE       NONE       NONE       NONE       6.0.7.1    NONE       NONE       NONE       NONE       NONE       NONE       3          ~12        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        15         [ ]        22:06:46   [ ]        Installed  :          [ ]        php-gd     []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:48:51   [ ]        Installed  :          [ ]        glibc-devel[]         NONE       NONE       1          .          i386       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:48:52   [ ]        Installed  :          [ ]        glibc-profile[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:48:54   [ ]        Installed  :          [ ]        glibc-utils[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:34   [ ]        Installed  :          [ ]        ruby-libs  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:35   [ ]        Installed  :          [ ]        db4        []         NONE       NONE       1          .          i386       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:36   [ ]        Installed  :          [ ]        readline   []         NONE       NONE       1          .          i386       [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:37   [ ]        Installed  :          [ ]        tk         []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          8          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:37   [ ]        Installed  :          [ ]        libtermcap []         NONE       NONE       1          .          i386       [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:37   [ ]        Installed  :          [ ]        gdbm       []         NONE       NONE       1          .          i386       [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:37   [ ]        Installed  :          [ ]        ruby-devel []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:38   [ ]        Installed  :          [ ]        ruby-libs  []         NONE       NONE       1          .          i386       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:39   [ ]        Installed  :          [ ]        ruby       []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:41   [ ]        Installed  :          [ ]        ruby-docs  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:42   [ ]        Installed  :          [ ]        ruby-tcltk []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:49:42   [ ]        Installed  :          [ ]        ruby-mode  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:50:22   [ ]        Installed  :          [ ]        tix        []         NONE       NONE       1          .          x86_64     [ ]        5          NONE       1:8        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:50:22   [ ]        Installed  :          [ ]        tkinter    []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:50:23   [ ]        Installed  :          [ ]        python-devel[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:50:23   [ ]        Installed  :          [ ]        python-ldap[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:50:25   [ ]        Installed  :          [ ]        python-docs[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        20         [ ]        03:50:25   [ ]        Installed  :          [ ]        python-tools[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        24         [ ]        00:44:34   [ ]        Updated    :          [ ]        cups-libs  []         NONE       NONE       1          .          x86_64     [ ]        7          NONE       1:1        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          1          []         1          []         NONE       1          .          NONE       1          rc1        .          NONE       NONE       NONE       1          22         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        24         [ ]        00:44:39   [ ]        Updated    :          [ ]        perl       []         NONE       NONE       1          .          x86_64     [ ]        7          NONE       3:5        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          8          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          5          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       RHEL4      NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        24         [ ]        00:44:39   [ ]        Updated    :          [ ]        curl       []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          12         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       rhel4      NONE       NONE       3          NONE       NONE       1          
Dec        [ ]        24         [ ]        00:44:41   [ ]        Updated    :          [ ]        udev       []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          39         []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        24         [ ]        00:44:43   [ ]        Updated    :          [ ]        cups       []         NONE       NONE       1          .          x86_64     [ ]        7          NONE       1:1        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          1          []         1          []         NONE       1          .          NONE       1          rc1        .          NONE       NONE       NONE       1          22         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        24         [ ]        23:35:07   [ ]        Installed  :          [ ]        lm_sensors []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dec        [ ]        24         [ ]        23:35:07   [ ]        Installed  :          [ ]        lm_sensors []         NONE       NONE       1          .          i386       [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jan        [ ]        4          [ ]        04:07:41   [ ]        Updated    :          [ ]        libtool-libs[]         NONE       NONE       1          .          x86_64     [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          5          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       c4         NONE       NONE       3          NONE       NONE       1          
Jan        [ ]        28         [ ]        04:21:39   [ ]        Installed  :          [ ]        mod_python []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:10:35   [ ]        Updated    :          [ ]        yum        []         NONE       NONE       1          .          noarch     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       centos4    NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:25   [ ]        Updated    :          [ ]        libgcc     []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:25   [ ]        Updated    :          [ ]        libgcc     []         NONE       NONE       1          .          i386       [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:25   [ ]        Updated    :          [ ]        hwdata     []         NONE       NONE       1          .          noarch     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          NONE       1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          146        NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL-1       NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:26   [ ]        Updated    :          [ ]        tzdata     []         NONE       NONE       1          .          noarch     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2006       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       a-2        NONE       .          EL4        2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:37   [ ]        Updated    :          [ ]        glibc-common[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:43   [ ]        Updated    :          [ ]        glibc      []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:43   [ ]        Updated    :          [ ]        chkconfig  []         NONE       NONE       1          .          x86_64     [ ]        2          NONE       NONE       NONE       NONE       NONE       1.3.13.3   NONE       NONE       NONE       NONE       NONE       NONE       3          ~2         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:44   [ ]        Updated    :          [ ]        e2fsprogs  []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          35         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:44   [ ]        Updated    :          [ ]        popt       []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       _          nonptl     4          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:44   [ ]        Updated    :          [ ]        krb5-libs  []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:45   [ ]        Updated    :          [ ]        openssl    []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          NONE       1          []         a-43       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:45   [ ]        Updated    :          [ ]        bzip2-libs []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:48   [ ]        Updated    :          [ ]        python     []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:50   [ ]        Updated    :          [ ]        glibc      []         NONE       NONE       1          .          i686       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:50   [ ]        Updated    :          [ ]        libpng     []         NONE       NONE       1          .          x86_64     [ ]        6          NONE       2:1        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          2          []         1          []         NONE       1          .          NONE       1          el4        .          NONE       NONE       NONE       1          7          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:50   [ ]        Updated    :          [ ]        audit-libs []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:51   [ ]        Updated    :          [ ]        shadow-utils[]         NONE       NONE       1          .          x86_64     [ ]        7          NONE       2:4        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       RHEL4      NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:51   [ ]        Updated    :          [ ]        pam        []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          77         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:52   [ ]        Updated    :          [ ]        newt       []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          51         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       rhel4      NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:52   [ ]        Updated    :          [ ]        e2fsprogs  []         NONE       NONE       1          .          i386       [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          35         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:52   [ ]        Updated    :          [ ]        rpm-libs   []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       _          nonptl     4          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:53   [ ]        Updated    :          [ ]        procps     []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:53   [ ]        Updated    :          [ ]        file       []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          10         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:55   [ ]        Updated    :          [ ]        glibc-headers[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:56   [ ]        Updated    :          [ ]        rpm        []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       _          nonptl     4          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:56   [ ]        Updated    :          [ ]        rpm-python []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       _          nonptl     4          NONE       NONE       1          
May        [ ]        2          [ ]        06:19:57   [ ]        Updated    :          [ ]        krb5-libs  []         NONE       NONE       1          .          i386       [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:57   [ ]        Updated    :          [ ]        openssl    []         NONE       NONE       1          .          i686       [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          NONE       1          []         a-43       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:57   [ ]        Updated    :          [ ]        module-init-tools[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          pre5       .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:58   [ ]        Updated    :          [ ]        OpenIPMI-libs[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         E          1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:19:59   [ ]        Updated    :          [ ]        binutils   []         NONE       NONE       1          .          x86_64     [ ]        4          NONE       NONE       NONE       NONE       NONE       2.15.92.0  NONE       NONE       NONE       NONE       .          3          3          2          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          ~18        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:00   [ ]        Updated    :          [ ]        device-mapper[]         NONE       NONE       1          .          x86_64     [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       RHEL4      NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:01   [ ]        Updated    :          [ ]        apr        []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          c4         .          NONE       NONE       NONE       1          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:03   [ ]        Updated    :          [ ]        fontconfig []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       centos4    NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:04   [ ]        Updated    :          [ ]        sendmail   []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          8          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          13         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:05   [ ]        Updated    :          [ ]        glibc-devel[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:05   [ ]        Updated    :          [ ]        policycoreutils[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          18         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:05   [ ]        Updated    :          [ ]        MAKEDEV    []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          15         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:05   [ ]        Updated    :          [ ]        gd         []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         E          1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:05   [ ]        Updated    :          [ ]        cups-libs  []         NONE       NONE       1          .          x86_64     [ ]        7          NONE       1:1        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          1          []         1          []         NONE       1          .          NONE       1          rc1        .          NONE       NONE       NONE       1          22         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:06   [ ]        Updated    :          [ ]        rhnlib     []         NONE       NONE       1          .          noarch     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          p23        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:06   [ ]        Updated    :          [ ]        gnupg      []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:07   [ ]        Updated    :          [ ]        e2fsprogs-devel[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          35         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:07   [ ]        Updated    :          [ ]        krb5-devel []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:08   [ ]        Updated    :          [ ]        libstdc    NONE       +          +          2          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:08   [ ]        Updated    :          [ ]        iputils    []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          20020927   []         2          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          ~18        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:08   [ ]        Updated    :          [ ]        centos-release[]         NONE       NONE       1          .          x86_64     [ ]        3          NONE       6:4        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          ~3         NONE       1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:09   [ ]        Updated    :          [ ]        libuser    []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          el4        .          NONE       NONE       NONE       1          52         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:09   [ ]        Updated    :          [ ]        audit      []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:10   [ ]        Updated    :          [ ]        util-linux []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          NONE       1          []         a-16       1          .          NONE       1          EL4        .          NONE       NONE       NONE       2          12         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:10   [ ]        Updated    :          [ ]        device-mapper[]         NONE       NONE       1          .          i386       [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       RHEL4      NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:10   [ ]        Updated    :          [ ]        numactl    []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          6          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:10   [ ]        Installed  :          [ ]        keyutils-libs[]         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:11   [ ]        Updated    :          [ ]        unixODBC   []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:12   [ ]        Updated    :          [ ]        cpp        []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:13   [ ]        Updated    :          [ ]        gcc        []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:14   [ ]        Updated    :          [ ]        apr-devel  []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          c4         .          NONE       NONE       NONE       1          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:14   [ ]        Updated    :          [ ]        libtool-libs[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          5          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:14   [ ]        Updated    :          [ ]        keyutils   []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:15   [ ]        Updated    :          [ ]        openssl-devel[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          NONE       1          []         a-43       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:17   [ ]        Updated    :          [ ]        up2date    []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       centos4    NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:17   [ ]        Updated    :          [ ]        glibc-utils[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:18   [ ]        Updated    :          [ ]        selinux-policy-targeted[]         NONE       NONE       1          .          noarch     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          17         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:18   [ ]        Updated    :          [ ]        mdadm      []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          6          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:19   [ ]        Updated    :          [ ]        OpenIPMI   []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         E          1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:20   [ ]        Updated    :          [ ]        glibc-devel[]         NONE       NONE       1          .          i386       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:20   [ ]        Updated    :          [ ]        autofs     []         NONE       NONE       1          .          x86_64     [ ]        5          NONE       1:4        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:20   [ ]        Updated    :          [ ]        ntsysv     []         NONE       NONE       1          .          x86_64     [ ]        2          NONE       NONE       NONE       NONE       NONE       1.3.13.3   NONE       NONE       NONE       NONE       NONE       NONE       3          ~2         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:20   [ ]        Updated    :          [ ]        openssh-server[]         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          NONE       1          []         p1-8       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       2          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:21   [ ]        Updated    :          [ ]        nscd       []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:21   [ ]        Updated    :          [ ]        php-gd     []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:21   [ ]        Updated    :          [ ]        libstdc    NONE       +          +          2          .          i386       [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:21   [ ]        Updated    :          [ ]        python-tools[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:22   [ ]        Updated    :          [ ]        diskdumputils[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:23   [ ]        Updated    :          [ ]        python-devel[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:25   [ ]        Updated    :          [ ]        python-docs[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:25   [ ]        Updated    :          [ ]        bzip2      []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:25   [ ]        Updated    :          [ ]        openssh-clients[]         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          NONE       1          []         p1-8       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       2          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:26   [ ]        Updated    :          [ ]        krb5-workstation[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:26   [ ]        Updated    :          [ ]        ypbind     []         NONE       NONE       1          .          x86_64     [ ]        5          NONE       3:1        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          17         []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:26   [ ]        Updated    :          [ ]        php-mbstring[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:26   [ ]        Updated    :          [ ]        php-pgsql  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:27   [ ]        Updated    :          [ ]        tar        []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          14         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       RHEL4      NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:27   [ ]        Updated    :          [ ]        httpd-suexec[]         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       3          .          ent        .          centos4    NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:27   [ ]        Updated    :          [ ]        crash      []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:27   [ ]        Updated    :          [ ]        php-mysql  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:28   [ ]        Updated    :          [ ]        psacct     []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       rhel4      NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:29   [ ]        Updated    :          [ ]        httpd-manual[]         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       3          .          ent        .          centos4    NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:29   [ ]        Updated    :          [ ]        centos-yumconf[]         NONE       NONE       1          .          noarch     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:31   [ ]        Updated    :          [ ]        comps      []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          NONE       1          []         CENTOS-0   1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:32   [ ]        Updated    :          [ ]        php-pear   []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:40   [ ]        Updated    :          [ ]        rpmdb-CentOS[]         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:41   [ ]        Updated    :          [ ]        glibc-profile[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:42   [ ]        Updated    :          [ ]        redhat-logos[]         NONE       NONE       1          .          noarch     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          centos4    .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:43   [ ]        Updated    :          [ ]        xorg-x11-libs[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:45   [ ]        Updated    :          [ ]        httpd      []         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       3          .          ent        .          centos4    NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:45   [ ]        Updated    :          [ ]        xorg-x11-font-utils[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:45   [ ]        Updated    :          [ ]        lvm2       []         NONE       NONE       1          .          x86_64     [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       RHEL4      NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:48   [ ]        Updated    :          [ ]        udev       []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          39         []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:48   [ ]        Updated    :          [ ]        xorg-x11-Mesa-libGL[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:49   [ ]        Updated    :          [ ]        hal        []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:50   [ ]        Updated    :          [ ]        ImageMagick[]         NONE       NONE       1          .          x86_64     [ ]        4          NONE       NONE       NONE       NONE       NONE       6.0.7.1    NONE       NONE       NONE       NONE       NONE       NONE       3          ~14        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       c4         NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:50   [ ]        Updated    :          [ ]        tkinter    []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:20:52   [ ]        Updated    :          [ ]        fonts-xorg-75dpi[]         NONE       NONE       1          .          noarch     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL         NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:52   [ ]        Updated    :          [ ]        httpd-devel[]         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       3          .          ent        .          centos4    NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:20:53   [ ]        Updated    :          [ ]        initscripts[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          NONE       1          []         NONE       1          .          NONE       1          EL-1       .          NONE       NONE       NONE       2          93         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       centos4    NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:21:04   [ ]        Installed  :          [ ]        kernel     []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          6          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL         NONE       NONE       3          NONE       NONE       1          
May        [ ]        2          [ ]        06:21:04   [ ]        Updated    :          [ ]        php        []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:21:04   [ ]        Updated    :          [ ]        openssh    []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          NONE       1          []         p1-8       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       2          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:21:05   [ ]        Updated    :          [ ]        pcmcia-cs  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:21:05   [ ]        Updated    :          [ ]        ipsec-tools[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          rhel4      .          NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:21:06   [ ]        Updated    :          [ ]        system-config-network-tui[]         NONE       NONE       1          .          noarch     [ ]        7          NONE       NONE       NONE       NONE       NONE       1.3.22.0   NONE       .          EL         .          NONE       2          3          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:21:07   [ ]        Updated    :          [ ]        dhcpv6_client[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          10         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       _          EL4        4          NONE       NONE       1          
May        [ ]        2          [ ]        06:21:09   [ ]        Updated    :          [ ]        cups       []         NONE       NONE       1          .          x86_64     [ ]        7          NONE       1:1        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          1          []         1          []         NONE       1          .          NONE       1          rc1        .          NONE       NONE       NONE       1          22         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:21:09   [ ]        Updated    :          [ ]        netdump    []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          7          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:21:10   [ ]        Updated    :          [ ]        kernel-utils[]         NONE       NONE       1          .          x86_64     [ ]        6          NONE       1:2        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:21:11   [ ]        Updated    :          [ ]        xorg-x11-xfs[]         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        2          [ ]        06:21:13   [ ]        Updated    :          [ ]        dhclient   []         NONE       NONE       1          .          x86_64     [ ]        7          NONE       7:3        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
May        [ ]        5          [ ]        23:35:46   [ ]        Updated    :          [ ]        xorg-x11-libs[]         NONE       NONE       1          .          x86_64     [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        5          [ ]        23:35:47   [ ]        Updated    :          [ ]        xorg-x11-font-utils[]         NONE       NONE       1          .          x86_64     [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        5          [ ]        23:35:47   [ ]        Updated    :          [ ]        xorg-x11-Mesa-libGL[]         NONE       NONE       1          .          x86_64     [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        5          [ ]        23:35:48   [ ]        Updated    :          [ ]        xorg-x11-xfs[]         NONE       NONE       1          .          x86_64     [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          6          []         1          []         NONE       1          .          NONE       1          EL         .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        9          [ ]        11:13:38   [ ]        Updated    :          [ ]        ruby-libs  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        9          [ ]        11:14:54   [ ]        Updated    :          [ ]        ruby       []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        9          [ ]        11:16:12   [ ]        Updated    :          [ ]        libtiff    []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          6          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        9          [ ]        11:16:34   [ ]        Updated    :          [ ]        ruby-libs  []         NONE       NONE       1          .          i386       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        9          [ ]        11:16:34   [ ]        Updated    :          [ ]        ruby-devel []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        9          [ ]        11:16:36   [ ]        Updated    :          [ ]        ruby-docs  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        9          [ ]        11:16:37   [ ]        Updated    :          [ ]        ruby-tcltk []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        9          [ ]        11:16:37   [ ]        Updated    :          [ ]        ruby-mode  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          EL4        .          NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        9          [ ]        16:49:56   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        ruby-tcltk 2          
May        [ ]        9          [ ]        16:49:58   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        ruby-devel 2          
May        [ ]        9          [ ]        16:49:58   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        ruby-libs  2          
May        [ ]        9          [ ]        16:50:04   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        ruby-docs  2          
May        [ ]        9          [ ]        16:50:05   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        ruby-libs  2          
May        [ ]        9          [ ]        16:50:06   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        ruby-mode  2          
May        [ ]        9          [ ]        16:50:06   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        ruby       2          
May        [ ]        12         [ ]        12:20:57   [ ]        Installed  :          [ ]        fedora-usermgmt-setup[]         NONE       NONE       1          .          noarch     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        12         [ ]        12:20:57   [ ]        Installed  :          [ ]        perl-DateManip[]         NONE       NONE       1          .          noarch     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          5          NONE       1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          42         NONE       a-3        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          NONE       NONE       1          
May        [ ]        12         [ ]        12:20:58   [ ]        Installed  :          [ ]        rrdtool    []         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       3          .          el4        .          kb         NONE       NONE       3          NONE       NONE       1          
May        [ ]        12         [ ]        12:20:58   [ ]        Installed  :          [ ]        perl-Time-HiRes[]         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          55         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        12         [ ]        12:20:58   [ ]        Installed  :          [ ]        perl-HTML-Template[]         NONE       NONE       1          .          noarch     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          7          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        12         [ ]        12:20:58   [ ]        Installed  :          [ ]        perl-IO-Multiplex[]         NONE       NONE       1          .          noarch     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        12         [ ]        12:20:59   [ ]        Installed  :          [ ]        perl-Net-Server[]         NONE       NONE       1          .          noarch     [ ]        7          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          90         NONE       NONE       NONE       NONE       NONE       NONE       3          .          el4        .          kb         NONE       NONE       3          NONE       NONE       1          
May        [ ]        12         [ ]        12:21:00   [ ]        Installed  :          [ ]        fedora-usermgmt-shadow-utils[]         NONE       NONE       1          .          noarch     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        12         [ ]        12:21:00   [ ]        Installed  :          [ ]        fedora-usermgmt[]         NONE       NONE       1          .          noarch     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          8          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        12         [ ]        12:21:01   [ ]        Installed  :          [ ]        munin      []         NONE       NONE       1          .          noarch     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       3          .          el4        .          kb         NONE       NONE       3          NONE       NONE       1          
May        [ ]        12         [ ]        12:39:30   [ ]        Installed  :          [ ]        perl-HTML-Tagset[]         NONE       NONE       1          .          noarch     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        12         [ ]        12:39:30   [ ]        Installed  :          [ ]        perl-HTML-Parser[]         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          35         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        12         [ ]        12:39:31   [ ]        Installed  :          [ ]        perl-libwww-perl[]         NONE       NONE       1          .          noarch     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          5          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          79         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        12         [ ]        12:39:32   [ ]        Installed  :          [ ]        sysstat    []         NONE       NONE       1          .          x86_64     [ ]        6          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          5          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          0          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       rhel4      NONE       NONE       3          NONE       NONE       1          
May        [ ]        12         [ ]        12:39:39   [ ]        Installed  :          [ ]        munin-node []         NONE       NONE       1          .          noarch     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       3          .          el4        .          kb         NONE       NONE       3          NONE       NONE       1          
May        [ ]        12         [ ]        13:08:45   [ ]        Installed  :          [ ]        flex       []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          NONE       1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       2          5          NONE       a-33       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          NONE       NONE       1          
May        [ ]        12         [ ]        13:12:06   [ ]        Installed  :          [ ]        ocaml      []         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          9          NONE       NONE       NONE       NONE       NONE       NONE       3          .          el4        .          kb         NONE       NONE       3          NONE       NONE       1          
May        [ ]        12         [ ]        13:17:16   [ ]        Installed  :          [ ]        byacc      []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          9          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:43   [ ]        Updated    :          [ ]        postgresql-libs[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:44   [ ]        Updated    :          [ ]        postgresql-libs[]         NONE       NONE       1          .          i386       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:46   [ ]        Updated    :          [ ]        postgresql []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:49   [ ]        Updated    :          [ ]        postgresql-server[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:49   [ ]        Updated    :          [ ]        postgresql-tcl[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:49   [ ]        Updated    :          [ ]        postgresql-jdbc[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:51   [ ]        Updated    :          [ ]        postgresql-devel[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:53   [ ]        Updated    :          [ ]        postgresql-docs[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:53   [ ]        Updated    :          [ ]        postgresql-python[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:55   [ ]        Updated    :          [ ]        postgresql-test[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:31:55   [ ]        Updated    :          [ ]        postgresql-pl[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
May        [ ]        29         [ ]        00:32:05   [ ]        Installed  :          [ ]        kernel     []         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          6          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL         NONE       NONE       3          NONE       NONE       1          
May        [ ]        29         [ ]        00:32:05   [ ]        Updated    :          [ ]        postgresql-contrib[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          7          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          4          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        5          [ ]        03:25:25   [ ]        Updated    :          [ ]        mysql      []         NONE       NONE       1          .          i386       [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        5          [ ]        03:25:27   [ ]        Updated    :          [ ]        mysql      []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        5          [ ]        03:25:29   [ ]        Updated    :          [ ]        mysql-server[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        5          [ ]        03:25:30   [ ]        Updated    :          [ ]        mysql-devel[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        5          [ ]        03:25:31   [ ]        Updated    :          [ ]        sendmail   []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          8          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          13         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        5          [ ]        03:25:32   [ ]        Updated    :          [ ]        mysql-bench[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          RHEL4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        12         [ ]        15:29:40   [ ]        Updated    :          [ ]        ocaml      []         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          3          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          9          NONE       NONE       NONE       NONE       NONE       NONE       3          .          el4        .          kb         NONE       NONE       3          NONE       NONE       1          
Jul        [ ]        12         [ ]        15:29:41   [ ]        Updated    :          [ ]        munin      []         NONE       NONE       1          .          noarch     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       3          .          el4        .          kb         NONE       NONE       3          NONE       NONE       1          
Jul        [ ]        12         [ ]        15:29:50   [ ]        Installed  :          [ ]        kernel     []         NONE       NONE       1          .          x86_64     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          6          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL         NONE       NONE       3          NONE       NONE       1          
Jul        [ ]        12         [ ]        15:29:56   [ ]        Updated    :          [ ]        munin-node []         NONE       NONE       1          .          noarch     [ ]        8          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       3          .          el4        .          kb         NONE       NONE       3          NONE       NONE       1          
Jul        [ ]        16         [ ]        03:59:20   [ ]        Installed  :          [ ]        mcelog     []         NONE       NONE       1          .          x86_64     [ ]        7          NONE       1:0        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL         NONE       NONE       3          NONE       NONE       1          
Jul        [ ]        16         [ ]        04:07:49   [ ]        Installed  :          [ ]        dhcp       []         NONE       NONE       1          .          x86_64     [ ]        7          NONE       7:3        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
Jul        [ ]        16         [ ]        04:07:50   [ ]        Installed  :          [ ]        dhcpv6     []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          10         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       _          EL4        4          NONE       NONE       1          
Jul        [ ]        16         [ ]        04:07:50   [ ]        Installed  :          [ ]        dhcp-devel []         NONE       NONE       1          .          x86_64     [ ]        7          NONE       7:3        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
Jul        [ ]        16         [ ]        04:20:27   [ ]        Installed  :          [ ]        tftp       []         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          39         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        16         [ ]        04:20:27   [ ]        Installed  :          [ ]        tftp-server[]         NONE       NONE       1          .          x86_64     [ ]        3          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          0          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          39         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        16         [ ]        12:37:13   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        dhcp-devel 2          
Jul        [ ]        16         [ ]        12:37:13   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        dhcpv6_client2          
Jul        [ ]        16         [ ]        12:37:13   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        dhcpv6     2          
Jul        [ ]        16         [ ]        12:37:14   [ ]        Erased     :          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       [ ]        dhcp       2          
Jul        [ ]        21         [ ]        02:52:06   [ ]        Updated    :          [ ]        freetype   []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          2          []         1          []         NONE       1          .          NONE       1          rhel4      .          NONE       NONE       NONE       1          1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        21         [ ]        02:52:06   [ ]        Updated    :          [ ]        php-gd     []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        21         [ ]        02:52:07   [ ]        Updated    :          [ ]        vixie-cron []         NONE       NONE       1          .          x86_64     [ ]        6          NONE       4:4        .          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       NONE       2          1          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          ~44        NONE       NONE       NONE       NONE       NONE       NONE       2          .          NONE       NONE       EL4        NONE       NONE       3          NONE       NONE       1          
Jul        [ ]        21         [ ]        02:52:07   [ ]        Updated    :          [ ]        php-mysql  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        21         [ ]        02:52:07   [ ]        Updated    :          [ ]        php-pgsql  []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        21         [ ]        02:52:08   [ ]        Updated    :          [ ]        php-pear   []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        21         [ ]        02:52:08   [ ]        Updated    :          [ ]        php-mbstring[]         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        21         [ ]        02:52:11   [ ]        Updated    :          [ ]        gnupg      []         NONE       NONE       1          .          x86_64     [ ]        4          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          1          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          2          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Jul        [ ]        21         [ ]        02:52:13   [ ]        Updated    :          [ ]        php        []         NONE       NONE       1          .          x86_64     [ ]        5          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          4          []         1          []         NONE       1          .          NONE       1          NONE       NONE       NONE       NONE       NONE       1          3          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          
Dependencies (136):
{} -> BTy_1
{} -> BTy_3
{} -> BTy_5
{} -> BTy_8
{} -> BTy_26
{} -> BTy_30
{} -> BTy_50
{} -> BTy_54
{} -> BTy_59
{} -> BTy_63
{BTy_4,} -> BTy_9
{BTy_4,} -> BTy_16
{BTy_4,} -> BTy_19
{BTy_4,} -> BTy_32
{BTy_4,} -> BTy_33
{BTy_4,} -> BTy_34
{BTy_4,} -> BTy_37
{BTy_4,} -> BTy_31
{BTy_4,} -> BTy_65
{BTy_4,} -> BTy_66
{BTy_4,} -> BTy_64
{BTy_4,} -> BTy_80
{BTy_4,} -> BTy_79
{BTy_6,} -> BTy_9
{BTy_6,} -> BTy_16
{BTy_6,} -> BTy_19
{BTy_6,} -> BTy_80
{BTy_6,} -> BTy_79
{BTy_10,} -> BTy_9
{BTy_12,} -> BTy_9
{BTy_16,} -> BTy_9
{BTy_9,} -> BTy_16
{BTy_17,} -> BTy_9
{BTy_19,} -> BTy_9
{BTy_9,} -> BTy_19
{BTy_20,} -> BTy_9
{BTy_22,} -> BTy_9
{BTy_39,} -> BTy_9
{BTy_80,} -> BTy_9
{BTy_9,} -> BTy_80
{BTy_81,} -> BTy_9
{BTy_10,} -> BTy_11
{BTy_10,} -> BTy_13
{BTy_10,} -> BTy_14
{BTy_10,} -> BTy_12
{BTy_10,} -> BTy_16
{BTy_10,} -> BTy_19
{BTy_10,} -> BTy_23
{BTy_10,} -> BTy_24
{BTy_10,} -> BTy_25
{BTy_10,} -> BTy_32
{BTy_10,} -> BTy_33
{BTy_10,} -> BTy_34
{BTy_10,} -> BTy_37
{BTy_10,} -> BTy_31
{BTy_10,} -> BTy_44
{BTy_10,} -> BTy_45
{BTy_10,} -> BTy_47
{BTy_10,} -> BTy_49
{BTy_10,} -> BTy_56
{BTy_10,} -> BTy_55
{BTy_10,} -> BTy_62
{BTy_10,} -> BTy_65
{BTy_10,} -> BTy_66
{BTy_10,} -> BTy_64
{BTy_10,} -> BTy_70
{BTy_10,} -> BTy_71
{BTy_10,} -> BTy_74
{BTy_10,} -> BTy_75
{BTy_10,} -> BTy_80
{BTy_14,} -> BTy_13
{BTy_13,} -> BTy_14
{BTy_12,} -> BTy_16
{BTy_12,} -> BTy_19
{BTy_12,} -> BTy_80
{BTy_12,} -> BTy_79
{BTy_17,} -> BTy_16
{BTy_19,} -> BTy_16
{BTy_16,} -> BTy_19
{BTy_20,} -> BTy_16
{BTy_22,} -> BTy_16
{BTy_39,} -> BTy_16
{BTy_80,} -> BTy_16
{BTy_16,} -> BTy_80
{BTy_81,} -> BTy_16
{BTy_17,} -> BTy_19
{BTy_17,} -> BTy_80
{BTy_20,} -> BTy_19
{BTy_22,} -> BTy_19
{BTy_39,} -> BTy_19
{BTy_80,} -> BTy_19
{BTy_19,} -> BTy_80
{BTy_81,} -> BTy_19
{BTy_20,} -> BTy_80
{BTy_20,} -> BTy_79
{BTy_23,} -> BTy_24
{BTy_23,} -> BTy_25
{BTy_29,} -> BTy_32
{BTy_29,} -> BTy_33
{BTy_29,} -> BTy_34
{BTy_29,} -> BTy_37
{BTy_29,} -> BTy_31
{BTy_33,} -> BTy_32
{BTy_32,} -> BTy_33
{BTy_34,} -> BTy_32
{BTy_32,} -> BTy_34
{BTy_34,} -> BTy_33
{BTy_33,} -> BTy_34
{BTy_22,} -> BTy_80
{BTy_22,} -> BTy_79
{BTy_39,} -> BTy_65
{BTy_39,} -> BTy_66
{BTy_39,} -> BTy_64
{BTy_39,} -> BTy_80
{BTy_43,} -> BTy_44
{BTy_43,} -> BTy_45
{BTy_43,} -> BTy_47
{BTy_43,} -> BTy_49
{BTy_47,} -> BTy_44
{BTy_44,} -> BTy_47
{BTy_47,} -> BTy_45
{BTy_45,} -> BTy_47
{BTy_49,} -> BTy_45
{BTy_45,} -> BTy_49
{BTy_51,} -> BTy_52
{BTy_62,} -> BTy_65
{BTy_62,} -> BTy_66
{BTy_62,} -> BTy_64
{BTy_66,} -> BTy_65
{BTy_65,} -> BTy_66
{BTy_68,} -> BTy_69
{BTy_68,} -> BTy_71
{BTy_73,} -> BTy_69
{BTy_70,} -> BTy_71
{BTy_75,} -> BTy_74
{BTy_81,} -> BTy_80

After reduction:
Pstruct(Id = BTy_83 328)
	[string](Id = BTy_0 328);
	" "(Id = BTy_1 328);
	[int](Id = BTy_2 328);
	" "(Id = BTy_3 328);
	[Time](Id = BTy_4 328);
	" "(Id = BTy_5 328);
	[string](Id = BTy_6 328);
	": "(Id = BTy_8 328);
	Switch(BTy_4):
	case "*":
		Pstruct(Id = BTy_78 317)
			[string](Id = BTy_10 317);
			Switch(BTy_10):
			case "*":
				""(Id = BTy_11 314);
			case "libstdc":
				"++"(Id = BTy_13 3);
			End Switch;
			"."(Id = BTy_16 317);
			[string](Id = BTy_17 317);
			" "(Id = BTy_19 317);
			Punion(Id = BTy_22 317)
				""(Id = BTy_21 283);
				Pstruct(Id = BTy_27 25)
					[Time](Id = BTy_23 25);
					Punion(Id = BTy_25 25)
						"."(Id = BTy_24 24);
						""(Id = BTy_26 1);
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_38 9)
					[IP](Id = BTy_29 9);
					Punion(Id = BTy_31 9)
						""(Id = BTy_30 7);
						".EL."(Id = BTy_32 1);
						"."(Id = BTy_37 1);
					End Punion;
				End Pstruct;
			End Punion;
			[int](Id = BTy_39 317);
			RArray(Id = BTy_20 317)
				Pstruct(Id = BTy_58 1041)
					Punion(Id = BTy_42 1041)
						""(Id = BTy_41 292);
						Pstruct(Id = BTy_141 749)
							Punion(Id = BTy_49 749)
								Pstruct(Id = BTy_48 748)
									Punion(Id = BTy_45 748)
										""(Id = BTy_44 733);
										[string](Id = BTy_46 15);
									End Punion;
									"."(Id = BTy_47 748);
								End Pstruct;
								""(Id = BTy_50 1);
							End Punion;
							RArray(Id = BTy_43 749)
								Pstruct(Id = BTy_53 98)
									[string](Id = BTy_51 98);
									"."(Id = BTy_52 98);
								End Pstruct;
							End RArray;
							Punion(Id = BTy_55 749)
								""(Id = BTy_54 748);
								"_"(Id = BTy_56 1);
							End Punion;
						End Pstruct;
					End Punion;
					[int](Id = BTy_57 1041);
				End Pstruct;
			End RArray;
			Punion(Id = BTy_60 317)
				""(Id = BTy_59 246);
				Pstruct(Id = BTy_61 4)
					[string](Id = BTy_62 4);
					Punion(Id = BTy_64 4)
						""(Id = BTy_63 3);
						".EL4"(Id = BTy_65 1);
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_158 61)
					"."(Id = BTy_69 61);
					RArray(Id = BTy_68 61)
						Separator: "."
						[string](Id = BTy_70 16);
					End RArray;
				End Pstruct;
				Pstruct(Id = BTy_76 6)
					"_"(Id = BTy_74 6);
					[string](Id = BTy_75 6);
				End Pstruct;
			End Punion;
		End Pstruct;
	case "12:37:13":
		[string](Id = BTy_81 11);
	End Switch;
End Pstruct

Complexity of inferred type:
	numAlt = 10  numTBD = 0  numBtm = 0
Outputing partitions to directory: gen/
