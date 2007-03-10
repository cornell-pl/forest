Source file to process: data/rpmpkgs.txt
Output directory: gen/
Max depth to explore: 50
Print line numbers in output contexts: false
Print ids and output type tokens: true
Histogram comparison tolerance (percentage): 0.01
Struct determination tolerance (percentage): 0.1
Noise level threshold (percentage): 0.0
Minimum width threshold for array: 2
Junk threshold (percentage): 0.1
Starting on file data/rpmpkgs.txt
886 records.
Histogram of number of tokens per record:
	5:	9
	7:	5
	8:	154
	9:	3
	10:	354
	11:	5
	12:	195
	13:	15
	14:	82
	15:	16
	16:	20
	17:	3
	18:	23
	19:	1
	21:	1

Building histograms...
THRESHOLD for histogram equality: 9.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 2993.
Number of records with at least one token occurrence: 886.
StructScore: 370.
	2:	1	0.00112866817156
	3:	577	0.651241534989
	4:	280	0.316027088036
	5:	28	0.0316027088036


Cluster 1:
Token: (_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 877.
	1:	9	0.010158013544

Token: (-)
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 881.
	1:	5	0.00564334085779

Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 884.
	1:	2	0.00225733634312

Token: (()[Group Body]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 885.
	1:	1	0.00112866817156


Cluster 2:
Token: [int]
Total number of token occurrences: 2727.
Number of records with at least one token occurrence: 874.
StructScore: 1636.
	1:	18	0.020316027088
	2:	168	0.189616252822
	3:	462	0.52144469526
	4:	162	0.182844243792
	5:	45	0.0507900677201
	6:	19	0.0214446952596


Cluster 3:
Token: (.)
Total number of token occurrences: 3929.
Number of records with at least one token occurrence: 886.
StructScore: 2060.
	2:	9	0.010158013544
	3:	162	0.182844243792
	4:	367	0.414221218962
	5:	203	0.229119638826
	6:	99	0.111738148984
	7:	22	0.0248306997743
	8:	23	0.0259593679458
	10:	1	0.00112866817156


Cluster 4:
Token: (+)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 9.
StructScore: 2633.
	1:	2	0.00225733634312
	2:	7	0.0079006772009


Junk Tolerance Threshold: 89
Coverage: 886
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: (.)
Total number of token occurrences: 3929.
Number of records with at least one token occurrence: 886.
StructScore: 2060.
	2:	9	0.010158013544
	3:	162	0.182844243792
	4:	367	0.414221218962
	5:	203	0.229119638826
	6:	99	0.111738148984
	7:	22	0.0248306997743
	8:	23	0.0259593679458
	10:	1	0.00112866817156


Cluster 1:
Token: [string]
Total number of token occurrences: 2993.
Number of records with at least one token occurrence: 886.
StructScore: 370.
	2:	1	0.00112866817156
	3:	577	0.651241534989
	4:	280	0.316027088036
	5:	28	0.0316027088036


Cluster 2:
Token: [int]
Total number of token occurrences: 2727.
Number of records with at least one token occurrence: 874.
StructScore: 1636.
	1:	18	0.020316027088
	2:	168	0.189616252822
	3:	462	0.52144469526
	4:	162	0.182844243792
	5:	45	0.0507900677201
	6:	19	0.0214446952596


Cluster 3:
Token: (_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 877.
	1:	9	0.010158013544

Token: (-)
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 881.
	1:	5	0.00564334085779

Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 884.
	1:	2	0.00225733634312

Token: (()[Group Body]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 885.
	1:	1	0.00112866817156


Cluster 4:
Token: (+)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 9.
StructScore: 2633.
	1:	2	0.00225733634312
	2:	7	0.0079006772009


Possible array tokens:
(.)
Records in possible array context:886
Total:3929
Coverage:886
Width:8
Array	(.)	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 9.
Computed clusters
Cluster 0:
Token: (.)
Total number of token occurrences: 886.
Number of records with at least one token occurrence: 886.
StructScore: 0.
	1:	886	1.0

Token: [string]
Total number of token occurrences: 887.
Number of records with at least one token occurrence: 886.
StructScore: 1.
	1:	885	0.998871331828
	2:	1	0.00112866817156


Cluster 1:
Token: (-)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 885.
	1:	1	0.00112866817156


Cluster 2:
Token: [int]
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 9.
StructScore: 2633.
	1:	7	0.0079006772009
	2:	2	0.00225733634312

Token: (+)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 9.
StructScore: 2633.
	1:	2	0.00225733634312
	2:	7	0.0079006772009


Junk Tolerance Threshold: 89
Coverage: 885
Num Tokens: 2
Struct
Coverage:885
Token count:2
(.)	Occurrences:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 9.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 885.
Number of records with at least one token occurrence: 885.
StructScore: 1.
	1:	885	0.998871331828


Cluster 1:
Token: [int]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 885.
	1:	1	0.00112866817156


Junk Tolerance Threshold: 89
Coverage: 885
Num Tokens: 1
Struct
Coverage:885
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


Junk Tolerance Threshold: 1
Coverage: 1
Num Tokens: 1
Struct
Coverage:1
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 9.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 877.
Number of records with at least one token occurrence: 877.
StructScore: 9.
	1:	877	0.989841986456


Cluster 1:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 885.
	1:	1	0.00112866817156

Token: (-)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 885.
	1:	1	0.00112866817156


Cluster 2:
Token: (+)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 9.
StructScore: 2633.
	1:	2	0.00225733634312
	2:	7	0.0079006772009

Token: [int]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 8.
StructScore: 2636.
	1:	6	0.00677200902935
	2:	2	0.00225733634312


Junk Tolerance Threshold: 89
Coverage: 877
Num Tokens: 1
Struct
Coverage:877
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (+)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 9.
StructScore: 2.
	1:	2	0.222222222222
	2:	7	0.777777777778


Cluster 1:
Token: [int]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 8.
StructScore: 5.
	1:	6	0.666666666667
	2:	2	0.222222222222


Cluster 2:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111

Token: (-)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111


Junk Tolerance Threshold: 1
Coverage: 9
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: (+)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 9.
StructScore: 2.
	1:	2	0.222222222222
	2:	7	0.777777777778


Cluster 1:
Token: [int]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 8.
StructScore: 5.
	1:	6	0.666666666667
	2:	2	0.222222222222


Cluster 2:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111

Token: (-)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111


Possible array tokens:
(+)
Records in possible array context:9
Total:16
Coverage:9
Width:2
Array	(+)	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (+)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 0.
	1:	9	1.0


Junk Tolerance Threshold: 1
Coverage: 9
Num Tokens: 1
Struct
Coverage:9
Token count:1
(+)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (+)
Total number of token occurrences: 7.
Number of records with at least one token occurrence: 7.
StructScore: 0.
	1:	7	1.0


Junk Tolerance Threshold: 1
Coverage: 7
Num Tokens: 1
Struct
Coverage:7
Token count:1
(+)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 8.
StructScore: 5.
	1:	6	0.666666666667
	2:	2	0.222222222222


Cluster 1:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111

Token: (-)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111


Junk Tolerance Threshold: 1
Coverage: 9
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [int]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 8.
StructScore: 5.
	1:	6	0.666666666667
	2:	2	0.222222222222


Cluster 1:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111

Token: (-)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111


Possible array tokens:
[int]
Records in possible array context:9
Total:10
Coverage:8
Width:2
ARRAY NOT CHOSEN
Union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 8.
StructScore: 2.
	1:	6	0.75
	2:	2	0.25


Junk Tolerance Threshold: 1
Coverage: 8
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [int]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 8.
StructScore: 2.
	1:	6	0.75
	2:	2	0.25


Possible array tokens:
[int]
Records in possible array context:8
Total:10
Coverage:8
Width:2
Array	[int]	Occurrences:1
Array context
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
Token: [int]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 1
Struct
Coverage:2
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
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

Token: (-)
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
(-)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 31.
Computed clusters
Cluster 0:
Token: (.)
Total number of token occurrences: 3043.
Number of records with at least one token occurrence: 3043.
StructScore: 0.
	1:	3043	1.0


Cluster 1:
Token: [string]
Total number of token occurrences: 1220.
Number of records with at least one token occurrence: 1220.
StructScore: 1823.
	1:	1220	0.400920144594


Cluster 2:
Token: (_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 3034.
	1:	9	0.00295760762406

Token: (-)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 3039.
	1:	4	0.00131449227736

Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 3041.
	1:	2	0.000657246138679

Token: (()[Group Body]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 3042.
	1:	1	0.000328623069339


Cluster 3:
Token: [int]
Total number of token occurrences: 2716.
Number of records with at least one token occurrence: 1886.
StructScore: 4301.
	1:	1056	0.347025961222
	2:	830	0.272757147552


Junk Tolerance Threshold: 305
Coverage: 3043
Num Tokens: 1
Struct
Coverage:3043
Token count:1
(.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 31.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 1220.
Number of records with at least one token occurrence: 1220.
StructScore: 1823.
	1:	1220	0.400920144594


Cluster 1:
Token: (_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 3034.
	1:	9	0.00295760762406

Token: (-)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 3039.
	1:	4	0.00131449227736

Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 3041.
	1:	2	0.000657246138679

Token: (()[Group Body]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 3042.
	1:	1	0.000328623069339


Cluster 2:
Token: [int]
Total number of token occurrences: 2716.
Number of records with at least one token occurrence: 1886.
StructScore: 4301.
	1:	1056	0.347025961222
	2:	830	0.272757147552


Junk Tolerance Threshold: 305
Coverage: 1220
Num Tokens: 1
Struct
Coverage:1220
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 13.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 1156.
Number of records with at least one token occurrence: 1156.
StructScore: 64.
	1:	1156	0.947540983607


Cluster 1:
Token: (_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 1211.
	1:	9	0.00737704918033

Token: (-)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 1216.
	1:	4	0.00327868852459


Cluster 2:
Token: [int]
Total number of token occurrences: 75.
Number of records with at least one token occurrence: 64.
StructScore: 3479.
	1:	53	0.0434426229508
	2:	11	0.00901639344262


Junk Tolerance Threshold: 122
Coverage: 1156
Num Tokens: 1
Struct
Coverage:1156
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 75.
Number of records with at least one token occurrence: 64.
StructScore: 11.
	1:	53	0.828125
	2:	11	0.171875


Cluster 1:
Token: (_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 55.
	1:	9	0.140625


Cluster 2:
Token: (-)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 60.
	1:	4	0.0625


Junk Tolerance Threshold: 7
Coverage: 64
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [int]
Total number of token occurrences: 75.
Number of records with at least one token occurrence: 64.
StructScore: 11.
	1:	53	0.828125
	2:	11	0.171875


Cluster 1:
Token: (_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 55.
	1:	9	0.140625


Cluster 2:
Token: (-)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 60.
	1:	4	0.0625


Possible array tokens:
[int]
Records in possible array context:64
Total:75
Coverage:64
Width:2
Array	[int]	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 64.
Number of records with at least one token occurrence: 64.
StructScore: 0.
	1:	64	1.0


Junk Tolerance Threshold: 7
Coverage: 64
Num Tokens: 1
Struct
Coverage:64
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 11.
StructScore: 0.
	1:	11	1.0


Junk Tolerance Threshold: 2
Coverage: 11
Num Tokens: 1
Struct
Coverage:11
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 51.
Number of records with at least one token occurrence: 51.
StructScore: 13.
	1:	51	0.796875


Cluster 1:
Token: (_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 55.
	1:	9	0.140625


Cluster 2:
Token: (-)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 60.
	1:	4	0.0625


Junk Tolerance Threshold: 7
Coverage: 51
Num Tokens: 1
Struct
Coverage:51
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 4.
	1:	9	0.692307692308


Cluster 1:
Token: (-)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 9.
	1:	4	0.307692307692


Junk Tolerance Threshold: 2
Coverage: 9
Num Tokens: 1
Struct
Coverage:9
Token count:1
(_)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: (-)
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
(-)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 19.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 2641.
Number of records with at least one token occurrence: 1822.
StructScore: 822.
	1:	1003	0.550191991223
	2:	819	0.449259462425


Cluster 1:
Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 1821.
	1:	2	0.00109709270433

Token: (()[Group Body]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1822.
	1:	1	0.000548546352167


Junk Tolerance Threshold: 183
Coverage: 1823
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [int]
Total number of token occurrences: 2641.
Number of records with at least one token occurrence: 1822.
StructScore: 822.
	1:	1003	0.550191991223
	2:	819	0.449259462425


Cluster 1:
Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 1821.
	1:	2	0.00109709270433

Token: (()[Group Body]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1822.
	1:	1	0.000548546352167


Possible array tokens:
[int]
Records in possible array context:1823
Total:2641
Coverage:1822
Width:2
Array	[int]	Occurrences:1
WARNING: ARRAY first context empty!Array context
Building histograms...
THRESHOLD for histogram equality: 19.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 1822.
Number of records with at least one token occurrence: 1822.
StructScore: 1.
	1:	1822	0.999451453648


Cluster 1:
Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 1821.
	1:	2	0.00109709270433

Token: [empty]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1822.
	1:	1	0.000548546352167


Junk Tolerance Threshold: 183
Coverage: 1822
Num Tokens: 1
Struct
Coverage:1822
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 19.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 1820.
Number of records with at least one token occurrence: 1820.
StructScore: 2.
	1:	1820	0.998902305159


Cluster 1:
Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 1820.
	1:	2	0.00109769484083


Junk Tolerance Threshold: 183
Coverage: 1820
Num Tokens: 1
Struct
Coverage:1820
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 1
Struct
Coverage:2
Token count:1
[IP]	Occurrences:1
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
THRESHOLD for histogram equality: 9.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 819.
Number of records with at least one token occurrence: 819.
StructScore: 0.
	1:	819	1.0


Junk Tolerance Threshold: 82
Coverage: 819
Num Tokens: 1
Struct
Coverage:819
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 19.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 1822.
Number of records with at least one token occurrence: 1822.
StructScore: 1.
	1:	1822	0.999451453648


Cluster 1:
Token: (()[Group Body]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1822.
	1:	1	0.000548546352167


Junk Tolerance Threshold: 183
Coverage: 1822
Num Tokens: 1
Struct
Coverage:1822
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

Token: (()
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0

Token: ())
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
[string]	Occurrences:1
(()	Occurrences:1
())	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 9.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 886.
Number of records with at least one token occurrence: 886.
StructScore: 0.
	1:	886	1.0


Junk Tolerance Threshold: 89
Coverage: 886
Num Tokens: 1
Struct
Coverage:886
Token count:1
[string]	Occurrences:1

Before reduction:
Parray(Id = BTy_0 886, 0b, 0b)((.) )
First:
	Pstruct(Id = BTy_19 886, 0b, 0b)
		Punion(Id = BTy_2 886, 0b, 0b)
			[empty](Id = BTy_1 885, 0b, 0b);
			[int](Id = BTy_3 1, 0b, 0b);
		End Punion;
		[string](Id = BTy_4 886, 0b, 0b);
		Punion(Id = BTy_6 886, 0b, 0b)
			[empty](Id = BTy_5 877, 0b, 0b);
			Parray(Id = BTy_7 9, 0b, 0b)((+) )
			First:
				(+)(Id = BTy_8 9, 0b, 0b);
			Body:
				(+)(Id = BTy_9 7, 0b, 0b);
			Tail:
				Punion(Id = BTy_17 9, 0b, 0b)
					Parray(Id = BTy_10 8, 0b, 0b)([int] )
					First:
						[int](Id = BTy_11 8, 0b, 0b);
					Body:
						[int](Id = BTy_12 2, 0b, 0b);
					Tail:
						[empty](Id = BTy_13 8, 0b, 0b);
					End Parray;
					Pstruct(Id = BTy_16 1, 0b, 0b)
						(-)(Id = BTy_14 1, 0b, 0b);
						[string](Id = BTy_15 1, 0b, 0b);
					End Pstruct;
				End Punion;
			End Parray;
		End Punion;
		(.)(Id = BTy_18 886, 0b, 0b);
	End Pstruct;
Body:
	Pstruct(Id = BTy_49 3043, 0b, 0b)
		Punion(Id = BTy_32 3043, 0b, 0b)
			Pstruct(Id = BTy_31 1220, 0b, 0b)
				Punion(Id = BTy_21 1220, 0b, 0b)
					[empty](Id = BTy_20 1156, 0b, 0b);
					Parray(Id = BTy_22 64, 0b, 0b)([int] )
					First:
						[int](Id = BTy_23 64, 0b, 0b);
					Body:
						[int](Id = BTy_24 11, 0b, 0b);
					Tail:
						Punion(Id = BTy_26 64, 0b, 0b)
							[empty](Id = BTy_25 51, 0b, 0b);
							(_)(Id = BTy_27 9, 0b, 0b);
							(-)(Id = BTy_29 4, 0b, 0b);
						End Punion;
					End Parray;
				End Punion;
				[string](Id = BTy_30 1220, 0b, 0b);
			End Pstruct;
			Parray(Id = BTy_33 1823, 0b, 0b)([int] )
			First:
				Punion(Id = BTy_39 1823, 0b, 0b)
					Pstruct(Id = BTy_38 1822, 0b, 0b)
						Punion(Id = BTy_35 1822, 0b, 0b)
							[empty](Id = BTy_34 1820, 0b, 0b);
							[IP](Id = BTy_36 2, 0b, 0b);
						End Punion;
						[int](Id = BTy_37 1822, 0b, 0b);
					End Pstruct;
					[empty](Id = BTy_40 1, 0b, 0b);
				End Punion;
			Body:
				[int](Id = BTy_41 819, 0b, 0b);
			Tail:
				Punion(Id = BTy_43 1823, 0b, 0b)
					[empty](Id = BTy_42 1822, 0b, 0b);
					Pstruct(Id = BTy_47 1, 0b, 0b)
						(()(Id = BTy_44 1, 0b, 0b);
						[string](Id = BTy_45 1, 0b, 0b);
						())(Id = BTy_46 1, 0b, 0b);
					End Pstruct;
				End Punion;
			End Parray;
		End Punion;
		(.)(Id = BTy_48 3043, 0b, 0b);
	End Pstruct;
Tail:
	[string](Id = BTy_50 886, 0b, 0b);
End Parray


After final reduction:
Pstruct(Id = BTy_55 886, 0b, 0b)
	Switch(BTy_4)(Id = BTy_2 886, 0b, 0b):
	case "*":
		""(Id = BTy_1 885, 0b, 0b);
	case "Suite-1":
		[4](Id = BTy_3 1, 0b, 0b);
	End Switch;
	[string](Id = BTy_4 886, 0b, 0b);
	Switch(BTy_4)(Id = BTy_6 886, 0b, 0b):
	case "*":
		""(Id = BTy_5 877, 0b, 0b);
	case {"compat-gcc-32-c", "compat-libstdc", "gcc-c", "gcc4-c", "gtk", "libstdc", "memtest86", }:
		Pstruct(Id = BTy_52 9, 0b, 0b)
			"+"(Id = BTy_8 9, 0b, 0b);
			RArray(Id = BTy_7 9, 0b, 0b)
				"+"(Id = BTy_9 7, 0b, 0b);
			End RArray;
			Punion(Id = BTy_17 9, 0b, 0b)
				Pstruct(Id = BTy_51 8, 0b, 0b)
					[~296...~1](Id = BTy_11 8, 0b, 0b);
					RArray(Id = BTy_10 8, 0b, 0b)
						[~3...~2](Id = BTy_12 2, 0b, 0b);
					End RArray;
				End Pstruct;
				Pstruct(Id = BTy_16 1, 0b, 0b)
					"-devel-3"(Id = BTy_14 1, 0b, 0b);
				End Pstruct;
			End Punion;
		End Pstruct;
	End Switch;
	"."(Id = BTy_18 886, 0b, 0b);
	RArray(Id = BTy_0 886, 0b, 0b)
		Separator: "."
		Punion(Id = BTy_32 3929, 0b, 0b)
			Pstruct(Id = BTy_31 1220, 0b, 0b)
				Punion(Id = BTy_21 1220, 0b, 0b)
					""(Id = BTy_20 1156, 0b, 0b);
					Pstruct(Id = BTy_53 64, 0b, 0b)
						[0...875](Id = BTy_23 64, 0b, 0b);
						RArray(Id = BTy_22 64, 0b, 0b)
							[~80...~2](Id = BTy_24 11, 0b, 0b);
						End RArray;
						Punion(Id = BTy_26 64, 0b, 0b)
							""(Id = BTy_25 51, 0b, 0b);
							"_"(Id = BTy_27 9, 0b, 0b);
							"-"(Id = BTy_29 4, 0b, 0b);
						End Punion;
					End Pstruct;
				End Punion;
				[string](Id = BTy_30 2106, 0b, 0b);
			End Pstruct;
			Pstruct(Id = BTy_54 1823, 0b, 0b)
				Punion(Id = BTy_39 1823, 0b, 0b)
					Pstruct(Id = BTy_38 1822, 0b, 0b)
						Switch(BTy_37)(Id = BTy_35 1822, 0b, 0b):
						case "*":
							""(Id = BTy_34 1820, 0b, 0b);
						case [~21]:
							{"15.92.0.2", "4.1.2.90", }(Id = BTy_36 2, 0b, 0b);
						End Switch;
						[~44...20060823](Id = BTy_37 1822, 0b, 0b);
					End Pstruct;
					""(Id = BTy_40 1, 0b, 0b);
				End Punion;
				RArray(Id = BTy_33 1823, 0b, 0b)
					[~187...0](Id = BTy_41 819, 0b, 0b);
				End RArray;
				Punion(Id = BTy_43 1823, 0b, 0b)
					""(Id = BTy_42 1822, 0b, 0b);
					Pstruct(Id = BTy_47 1, 0b, 0b)
						"(none)"(Id = BTy_44 1, 0b, 0b);
					End Pstruct;
				End Punion;
			End Pstruct;
		End Punion;
	End RArray;
End Pstruct

Complexity of inferred type:
	numAlt = 7  numTBD = 0  numBtm = 0
Outputing partitions to directory: gen/

Completed data/rpmpkgs.txt
Overall type complexity = 30.585b
Overall data complexity = 3814.486b

