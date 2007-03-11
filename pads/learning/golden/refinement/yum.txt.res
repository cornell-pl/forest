Source file to process: data/yum.txt
Output directory: gen/
Max depth to explore: 50
Print line numbers in output contexts: false
Print ids and output type tokens: true
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
	15:	29
	17:	61
	18:	1
	19:	107
	20:	1
	21:	89
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

Token: (:)
Total number of token occurrences: 328.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	1:	328	1.0


Cluster 1:
Token: [white space]
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
Token: [string]
Total number of token occurrences: 1175.
Number of records with at least one token occurrence: 328.
StructScore: 281.
	2:	11	0.0335365853659
	3:	140	0.426829268293
	4:	152	0.463414634146
	5:	25	0.0762195121951


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
Num Tokens: 2
Struct
Coverage:328
Token count:2
[Date]	Occurrences:1
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

Token: [string]
Total number of token occurrences: 328.
Number of records with at least one token occurrence: 328.
StructScore: 0.
	1:	328	1.0

Token: [white space]
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
[white space]	Occurrences:2
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
Pstruct(Id = BTy_81 328, 0b, 0b)
	[Date](Id = BTy_0 328, 0b, 0b);
	[white space](Id = BTy_1 328, 0b, 0b);
	[Time](Id = BTy_2 328, 0b, 0b);
	[white space](Id = BTy_3 328, 0b, 0b);
	[string](Id = BTy_4 328, 0b, 0b);
	(:)(Id = BTy_6 328, 0b, 0b);
	Punion(Id = BTy_77 328, 0b, 0b)
		Pstruct(Id = BTy_76 317, 0b, 0b)
			[white space](Id = BTy_7 317, 0b, 0b);
			[string](Id = BTy_8 317, 0b, 0b);
			Punion(Id = BTy_10 317, 0b, 0b)
				[empty](Id = BTy_9 314, 0b, 0b);
				Pstruct(Id = BTy_13 3, 0b, 0b)
					(+)(Id = BTy_11 3, 0b, 0b);
					(+)(Id = BTy_12 3, 0b, 0b);
				End Pstruct;
			End Punion;
			(.)(Id = BTy_14 317, 0b, 0b);
			[string](Id = BTy_15 317, 0b, 0b);
			[white space](Id = BTy_17 317, 0b, 0b);
			Parray(Id = BTy_18 317, 0b, 0b)([int] )
			First:
				Pstruct(Id = BTy_38 317, 0b, 0b)
					Punion(Id = BTy_20 317, 0b, 0b)
						[empty](Id = BTy_19 283, 0b, 0b);
						Pstruct(Id = BTy_25 25, 0b, 0b)
							[Time](Id = BTy_21 25, 0b, 0b);
							Punion(Id = BTy_23 25, 0b, 0b)
								(.)(Id = BTy_22 24, 0b, 0b);
								[empty](Id = BTy_24 1, 0b, 0b);
							End Punion;
						End Pstruct;
						Pstruct(Id = BTy_36 9, 0b, 0b)
							[IP](Id = BTy_27 9, 0b, 0b);
							Punion(Id = BTy_29 9, 0b, 0b)
								[empty](Id = BTy_28 7, 0b, 0b);
								Pstruct(Id = BTy_33 1, 0b, 0b)
									(.)(Id = BTy_30 1, 0b, 0b);
									[string](Id = BTy_31 1, 0b, 0b);
									(.)(Id = BTy_32 1, 0b, 0b);
								End Pstruct;
								(.)(Id = BTy_35 1, 0b, 0b);
							End Punion;
						End Pstruct;
					End Punion;
					[int](Id = BTy_37 317, 0b, 0b);
				End Pstruct;
			Body:
				Pstruct(Id = BTy_56 1041, 0b, 0b)
					Punion(Id = BTy_40 1041, 0b, 0b)
						[empty](Id = BTy_39 292, 0b, 0b);
						Parray(Id = BTy_41 749, 0b, 0b)((.) )
						First:
							Punion(Id = BTy_47 749, 0b, 0b)
								Pstruct(Id = BTy_46 748, 0b, 0b)
									Punion(Id = BTy_43 748, 0b, 0b)
										[empty](Id = BTy_42 733, 0b, 0b);
										[string](Id = BTy_44 15, 0b, 0b);
									End Punion;
									(.)(Id = BTy_45 748, 0b, 0b);
								End Pstruct;
								[empty](Id = BTy_48 1, 0b, 0b);
							End Punion;
						Body:
							Pstruct(Id = BTy_51 98, 0b, 0b)
								[string](Id = BTy_49 98, 0b, 0b);
								(.)(Id = BTy_50 98, 0b, 0b);
							End Pstruct;
						Tail:
							Punion(Id = BTy_53 749, 0b, 0b)
								[empty](Id = BTy_52 748, 0b, 0b);
								(_)(Id = BTy_54 1, 0b, 0b);
							End Punion;
						End Parray;
					End Punion;
					[int](Id = BTy_55 1041, 0b, 0b);
				End Pstruct;
			Tail:
				Punion(Id = BTy_58 317, 0b, 0b)
					[empty](Id = BTy_57 246, 0b, 0b);
					Pstruct(Id = BTy_59 4, 0b, 0b)
						[string](Id = BTy_60 4, 0b, 0b);
						Punion(Id = BTy_62 4, 0b, 0b)
							[empty](Id = BTy_61 3, 0b, 0b);
							Pstruct(Id = BTy_65 1, 0b, 0b)
								(.)(Id = BTy_63 1, 0b, 0b);
								[string](Id = BTy_64 1, 0b, 0b);
							End Pstruct;
						End Punion;
					End Pstruct;
					Parray(Id = BTy_66 61, 0b, 0b)((.) )
					First:
						(.)(Id = BTy_67 61, 0b, 0b);
					Body:
						Pstruct(Id = BTy_70 16, 0b, 0b)
							[string](Id = BTy_68 16, 0b, 0b);
							(.)(Id = BTy_69 16, 0b, 0b);
						End Pstruct;
					Tail:
						[string](Id = BTy_71 61, 0b, 0b);
					End Parray;
					Pstruct(Id = BTy_74 6, 0b, 0b)
						(_)(Id = BTy_72 6, 0b, 0b);
						[string](Id = BTy_73 6, 0b, 0b);
					End Pstruct;
				End Punion;
			End Parray;
		End Pstruct;
		Pstruct(Id = BTy_80 11, 0b, 0b)
			[white space](Id = BTy_78 11, 0b, 0b);
			[string](Id = BTy_79 11, 0b, 0b);
		End Pstruct;
	End Punion;
End Pstruct


After final reduction:
Pstruct(Id = BTy_81 328, 0b, 0b)
	[Date](Id = BTy_0 328, 0b, 0b);
	" "(Id = BTy_1 328, 0b, 0b);
	[Time](Id = BTy_2 328, 0b, 0b);
	" "(Id = BTy_3 328, 0b, 0b);
	{"Erased", "Installed", "Updated", }(Id = BTy_4 328, 0b, 0b);
	": "(Id = BTy_6 328, 0b, 0b);
	[string](Id = BTy_79 328, 0b, 0b);
	Switch(BTy_4)(Id = BTy_77 328, 0b, 0b):
	case {"Installed", "Updated", }:
		Pstruct(Id = BTy_76 317, 0b, 0b)
			Punion(Id = BTy_10 317, 0b, 0b)
				""(Id = BTy_9 314, 0b, 0b);
				Pstruct(Id = BTy_13 3, 0b, 0b)
					"++"(Id = BTy_11 3, 0b, 0b);
				End Pstruct;
			End Punion;
			"."(Id = BTy_14 317, 0b, 0b);
			[string](Id = BTy_15 317, 0b, 0b);
			" "(Id = BTy_17 317, 0b, 0b);
			Punion(Id = BTy_20 317, 0b, 0b)
				""(Id = BTy_19 283, 0b, 0b);
				Pstruct(Id = BTy_25 25, 0b, 0b)
					[Time](Id = BTy_21 25, 0b, 0b);
					Punion(Id = BTy_23 25, 0b, 0b)
						"."(Id = BTy_22 24, 0b, 0b);
						""(Id = BTy_24 1, 0b, 0b);
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_36 9, 0b, 0b)
					[IP](Id = BTy_27 9, 0b, 0b);
					Punion(Id = BTy_29 9, 0b, 0b)
						""(Id = BTy_28 7, 0b, 0b);
						Pstruct(Id = BTy_33 1, 0b, 0b)
							".EL."(Id = BTy_30 1, 0b, 0b);
						End Pstruct;
						"."(Id = BTy_35 1, 0b, 0b);
					End Punion;
				End Pstruct;
			End Punion;
			[~14...20020927](Id = BTy_37 317, 0b, 0b);
			RArray(Id = BTy_18 317, 0b, 0b)
				Pstruct(Id = BTy_56 1041, 0b, 0b)
					Punion(Id = BTy_40 1041, 0b, 0b)
						""(Id = BTy_39 292, 0b, 0b);
						Pstruct(Id = BTy_82 749, 0b, 0b)
							Punion(Id = BTy_47 749, 0b, 0b)
								Pstruct(Id = BTy_46 748, 0b, 0b)
									Punion(Id = BTy_43 748, 0b, 0b)
										""(Id = BTy_42 733, 0b, 0b);
										[string](Id = BTy_44 15, 0b, 0b);
									End Punion;
									"."(Id = BTy_45 748, 0b, 0b);
								End Pstruct;
								""(Id = BTy_48 1, 0b, 0b);
							End Punion;
							RArray(Id = BTy_41 749, 0b, 0b)
								Pstruct(Id = BTy_51 98, 0b, 0b)
									[string](Id = BTy_49 98, 0b, 0b);
									"."(Id = BTy_50 98, 0b, 0b);
								End Pstruct;
							End RArray;
							Punion(Id = BTy_53 749, 0b, 0b)
								""(Id = BTy_52 748, 0b, 0b);
								"_"(Id = BTy_54 1, 0b, 0b);
							End Punion;
						End Pstruct;
					End Punion;
					[~44...9004](Id = BTy_55 1041, 0b, 0b);
				End Pstruct;
			End RArray;
			Punion(Id = BTy_58 317, 0b, 0b)
				""(Id = BTy_57 246, 0b, 0b);
				Pstruct(Id = BTy_59 4, 0b, 0b)
					[string](Id = BTy_60 4, 0b, 0b);
					Punion(Id = BTy_62 4, 0b, 0b)
						""(Id = BTy_61 3, 0b, 0b);
						Pstruct(Id = BTy_65 1, 0b, 0b)
							".EL4"(Id = BTy_63 1, 0b, 0b);
						End Pstruct;
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_83 61, 0b, 0b)
					"."(Id = BTy_67 61, 0b, 0b);
					RArray(Id = BTy_66 61, 0b, 0b)
						Separator: "."
						[string](Id = BTy_68 77, 0b, 0b);
					End RArray;
				End Pstruct;
				Pstruct(Id = BTy_74 6, 0b, 0b)
					"_"(Id = BTy_72 6, 0b, 0b);
					{"EL4", "nonptl", }(Id = BTy_73 6, 0b, 0b);
				End Pstruct;
			End Punion;
		End Pstruct;
	End Switch;
End Pstruct

Complexity of inferred type:
	numAlt = 10  numTBD = 0  numBtm = 0
Outputing partitions to directory: gen/

Completed data/yum.txt
Overall type complexity = 53.170b
Overall data complexity = ~infb

