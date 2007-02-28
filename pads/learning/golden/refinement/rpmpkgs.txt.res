Source file to process: ../golden/data/rpmpkgs.txt
Output directory: gen/
Max depth to explore: 50
Print line numbers in output contexts: false
Print ids and output type tokens: true
Histogram comparison tolerance (percentage): 0.01
Struct determination tolerance (percentage): 0.1
Noise level threshold (percentage): 0.0
Minimum width threshold for array: 2
Junk threshold (percentage): 0.1
Starting on file ../golden/data/rpmpkgs.txt
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
Parray(Id = BTy_0 886)((.) )
First:
	Pstruct(Id = BTy_19 886)
		Punion(Id = BTy_2 886)
			[empty](Id = BTy_1 885);
			[int](Id = BTy_3 1);
		End Punion;
		[string](Id = BTy_4 886);
		Punion(Id = BTy_6 886)
			[empty](Id = BTy_5 877);
			Parray(Id = BTy_7 9)((+) )
			First:
				(+)(Id = BTy_8 9);
			Body:
				(+)(Id = BTy_9 7);
			Tail:
				Punion(Id = BTy_17 9)
					Parray(Id = BTy_10 8)([int] )
					First:
						[int](Id = BTy_11 8);
					Body:
						[int](Id = BTy_12 2);
					Tail:
						[empty](Id = BTy_13 8);
					End Parray;
					Pstruct(Id = BTy_16 1)
						(-)(Id = BTy_14 1);
						[string](Id = BTy_15 1);
					End Pstruct;
				End Punion;
			End Parray;
		End Punion;
		(.)(Id = BTy_18 886);
	End Pstruct;
Body:
	Pstruct(Id = BTy_49 3043)
		Punion(Id = BTy_32 3043)
			Pstruct(Id = BTy_31 1220)
				Punion(Id = BTy_21 1220)
					[empty](Id = BTy_20 1156);
					Parray(Id = BTy_22 64)([int] )
					First:
						[int](Id = BTy_23 64);
					Body:
						[int](Id = BTy_24 11);
					Tail:
						Punion(Id = BTy_26 64)
							[empty](Id = BTy_25 51);
							(_)(Id = BTy_27 9);
							(-)(Id = BTy_29 4);
						End Punion;
					End Parray;
				End Punion;
				[string](Id = BTy_30 1220);
			End Pstruct;
			Parray(Id = BTy_33 1823)([int] )
			First:
				Punion(Id = BTy_39 1823)
					Pstruct(Id = BTy_38 1822)
						Punion(Id = BTy_35 1822)
							[empty](Id = BTy_34 1820);
							[IP](Id = BTy_36 2);
						End Punion;
						[int](Id = BTy_37 1822);
					End Pstruct;
					[empty](Id = BTy_40 1);
				End Punion;
			Body:
				[int](Id = BTy_41 819);
			Tail:
				Punion(Id = BTy_43 1823)
					[empty](Id = BTy_42 1822);
					Pstruct(Id = BTy_47 1)
						(()(Id = BTy_44 1);
						[string](Id = BTy_45 1);
						())(Id = BTy_46 1);
					End Pstruct;
				End Punion;
			End Parray;
		End Punion;
		(.)(Id = BTy_48 3043);
	End Pstruct;
Tail:
	[string](Id = BTy_50 886);
End Parray

BTy_0	BTy_1	BTy_3	BTy_2	BTy_4	BTy_5	BTy_7	BTy_8	BTy_9	BTy_10	BTy_11	BTy_12	BTy_13	BTy_14	BTy_15	BTy_17	BTy_6	BTy_18	BTy_20	BTy_22	BTy_23	BTy_24	BTy_25	BTy_27	BTy_29	BTy_26	BTy_21	BTy_30	BTy_33	BTy_34	BTy_36	BTy_35	BTy_37	BTy_40	BTy_39	BTy_41	BTy_42	BTy_44	BTy_45	BTy_46	BTy_43	BTy_32	BTy_48	BTy_50	
6          NONE       4          2          Suite-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          Canna-libs-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          7          NONE       NONE       NONE       NONE       NONE       1          p3-7       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ElectricFence-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~19        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          FreeWnn-libs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          10         NONE       NONE       NONE       NONE       NONE       1          pl020-5    NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          GConf2-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          GConf2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          ImageMagick-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~16        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          MAKEDEV-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          15         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          MyODBC-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          50         NONE       1          ~21        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          MySQL-python-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          NetworkManager-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          NetworkManager-gnome-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ORBit-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~14        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ORBit2-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          12         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ORBit2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          12         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          Omni-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          Omni-foomatic-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          OpenIPMI-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          OpenIPMI-libs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          OpenIPMI-tools-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          PyQt-3     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          13         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          PyQt-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          13         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          PyXML-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          SDL-1      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          SDL-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          SysVinit-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          85         NONE       1          ~34        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          VFlib2-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          25         NONE       1          ~25        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          Xaw3d-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          5          NONE       1          ~24        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          Xaw3d-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          5          NONE       1          ~24        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          a2ps-4     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          13         NONE       NONE       NONE       NONE       NONE       1          b-41       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          acl-2      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          acpid-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          alchemist-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          alsa-lib-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          0          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          alsa-lib-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          0          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          alsa-utils-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          am-utils-6 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          0          NONE       1          ~15        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          anacron-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          3          NONE       1          ~32        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          apel-xemacs-10[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          6          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          apmd-3     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~24        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          apr-0      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          c4         1          []         NONE       1          9          NONE       1          ~24        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          apr-util-0 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~21        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          arptables_jf-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          arts-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          arts-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ash-0      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~20        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          aspell-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          fc3        1          []         NONE       1          50         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          aspell-en-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          51         NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          at-3       []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         3          8          ~80        NONE       _          NONE       2          1          EL4        1          []         NONE       1          1          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          at-spi-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          at-spi-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          atk-1      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          atk-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          attr-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          audiofile-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          audiofile-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          audit-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          audit-libs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          authconfig-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         2          10         NONE       NONE       NONE       -          3          1          rhel4      1          []         NONE       1          6          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          authconfig-gtk-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         2          10         NONE       NONE       NONE       -          3          1          rhel4      1          []         NONE       1          6          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          autoconf-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          59         NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          autofs-4   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~187       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          automake-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          9          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          automake14-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          p6-12      NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          automake15-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          5          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          automake16-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          6          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          automake17-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          7          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          autorun-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          14         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          basesystem-8[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          0          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          bash-3     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~19        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          bc-1       []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          6          NONE       1          ~17        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          beecrypt-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          bind-libs-9[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          2          NONE       1          ~16        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          bind-utils-9[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          2          NONE       1          ~16        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          binutils-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          NONE       15.92.0.2  2          ~21        NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          bison-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          875        NONE       NONE       NONE       NONE       NONE       1          c-2        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          bitmap-fonts-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          3          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          bitstream-vera-fonts-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          10         NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          bluez-bluefw-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          bluez-hcidump-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          11         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          bluez-libs-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          10         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          bluez-pin-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          23         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          bluez-utils-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          10         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          boost-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          32         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          boost-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          32         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          bug-buddy-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          byacc-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          9          NONE       1          ~28        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          bzip2-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          bzip2-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          bzip2-libs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          cadaver-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          22         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          cdecl-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          5          NONE       1          ~30        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          cdparanoia-devel-alpha9[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          8          NONE       1          ~24        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          cdparanoia-libs-alpha9[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          8          NONE       1          ~24        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          cdrecord-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          centos-release-4-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          checkpolicy-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          17         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          chkconfig-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          chkfontpath-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          10         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ckermit-8  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          compat-db-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          compat-gcc-32-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~47        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          compat-gcc-32-cNONE       3          +          +          1          ~3         NONE       NONE       NONE       NONE       1          2          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~47        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          compat-libcom_err-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          compat-libgcc-296-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          96         NONE       1          ~132       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
11         []         NONE       1          compat-libstdcNONE       4          +          +          2          ~296       ~2         NONE       NONE       NONE       1          2          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          96         NONE       1          ~132       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
11         []         NONE       1          compat-libstdcNONE       4          +          +          2          ~33        ~3         NONE       NONE       NONE       1          2          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~47        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          comps-4    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          CENTOS-0   1          []         NONE       1          20060823   NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          comps-extras-10[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          control-center-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          8          NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          coreutils-5[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~31        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          cpio-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      2          []         NONE       1          5          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          cpp-3      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          cproto-4   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          7          NONE       NONE       NONE       NONE       NONE       1          c-3        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          cracklib-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~29        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          cracklib-dicts-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~29        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          crash-4    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          crontabs-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          10         NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          cryptsetup-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          1          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          cscope-15  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      2          []         NONE       1          5          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ctags-5    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          cups-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rc1        1          []         NONE       1          1          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          cups-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rc1        1          []         NONE       1          1          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          cups-libs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rc1        1          []         NONE       1          1          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          curl-7     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          12         NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          curl-devel-7[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          12         NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          cvs-1      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          11         NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          cyrus-sasl-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          cyrus-sasl-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          cyrus-sasl-md5-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          cyrus-sasl-plain-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          dapl-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          db4-4      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          db4-devel-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          db4-utils-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          dbus-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          22         NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          dbus-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          22         NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          dbus-glib-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          22         NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          dbus-python-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          22         NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          dbus-x11-0 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          22         NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ddd-3      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          dejagnu-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          4          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          desktop-backgrounds-basic-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          1          NONE       NONE       NONE       NONE       NONE       1          E          2          []         NONE       1          0          NONE       1          ~26        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          desktop-backgrounds-extra-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          1          NONE       NONE       NONE       NONE       NONE       1          E          2          []         NONE       1          0          NONE       1          ~26        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          desktop-file-utils-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          9          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          desktop-printing-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          17         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          devhelp-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        2          []         NONE       1          10         NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          device-mapper-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          2          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          dhclient-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~58        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          dhcpv6_client-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         3          10         ~14        NONE       _          NONE       2          1          EL4        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          dia-0      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          94         NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          dialog-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          diffstat-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          31         NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          diffutils-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          diskdumputils-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          dmalloc-5  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          dmraid-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rc11-3_RHEL4_U41          []         NONE       1          0          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          dmraid-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rc11-3_RHEL4_U41          []         NONE       1          0          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          docbook-dtds-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          0          NONE       1          ~25        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          docbook-simple-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          0          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          docbook-slides-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          docbook-style-dsssl-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          78         NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          docbook-style-xsl-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          65         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          docbook-utils-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          6          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          docbook-utils-pdf-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          6          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          dos2unix-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          1          NONE       1          ~21        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          dosfstools-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          8          NONE       1          ~15        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          doxygen-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          dtach-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          5          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          dump-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          b39-3      1          []         NONE       1          2          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          e2fsprogs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          35         NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          e2fsprogs-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          35         NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          ed-0       []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          2          NONE       1          ~36        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          eel2-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          eel2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          eject-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          elfutils-0 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          97         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          elfutils-libelf-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          97         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          elinks-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          emacs-21   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          3          NONE       1          ~19        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          emacs-common-21[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          3          NONE       1          ~19        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          emacs-leim-21[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          3          NONE       1          ~19        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          emacspeak-17[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          enscript-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~28        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          eog-2      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          esound-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          esound-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          ethtool-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          8          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          evolution-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          0          NONE       1          ~27        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          evolution-data-server-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          0          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          evolution-webcal-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          expat-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          95         NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          expat-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          95         NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          expect-5   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          42         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          fbset-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          1          NONE       1          ~17        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          festival-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~25        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          fetchmail-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          file-4     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          10         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          file-roller-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          filesystem-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          findutils-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          1          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          finger-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          17         NONE       1          ~26        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          firefox-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          5          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          firstboot-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          3          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          flac-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          flex-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          a-33       1          []         NONE       1          5          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          fontconfig-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          2          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          fontconfig-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          2          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          fonts-xorg-100dpi-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          fonts-xorg-75dpi-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          fonts-xorg-base-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          foomatic-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          freeglut-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~14        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          freetype-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          1          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          freetype-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          1          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
12         []         NONE       1          frysk-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rh1-0      1          []         NONE       1          0          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          ftp-0      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          17         NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gail-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gail-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gaim-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          5          NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gamin-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gamin-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gawk-3     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gcc-3      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          gcc-c      NONE       3          +          +          1          ~3         NONE       NONE       NONE       NONE       1          2          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gcc-g77-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gcc-gnat-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gcc-java-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gcc-objc-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gcc4-4     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~18        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          gcc4-c     NONE       3          +          +          1          ~4         NONE       NONE       NONE       NONE       1          2          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~18        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gcc4-gfortran-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~18        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gconf-editor-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          gd-2       []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          0          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          gd-devel-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          0          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          gdb-6      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gdbm-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~24        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gdbm-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~24        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          gdk-pixbuf-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          22         NONE       1          ~17        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          gdm-2      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          6          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gedit-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gettext-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          14         NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ggv-2      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          ghostscript-7[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~33        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          ghostscript-fonts-5[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          50         NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gimp-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gimp-data-extras-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          2          NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gimp-gap-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gimp-help-2-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          1          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gimp-print-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gimp-print-plugin-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gimp-print-utils-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          glade2-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          6          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          glib-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~15        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          glib2-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          glib2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          glibc-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i686       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          glibc-common-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          glibc-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          glibc-headers-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          glibc-kernheaders-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          4          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gmp-4      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gmp-devel-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-applets-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-audio-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gnome-desktop-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          8          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gnome-desktop-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          8          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          gnome-icon-theme-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-kerberos-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-keyring-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-keyring-manager-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-libs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         4.1.2.90   1          ~44        NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-mag-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          11         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-media-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-mime-data-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-netstatus-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gnome-panel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          3          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          8          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gnome-panel-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          3          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          8          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-pilot-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-pilot-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-python2-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-python2-bonobo-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-python2-canvas-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-python2-gtkhtml2-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gnome-session-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          8          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          gnome-speech-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          3          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-spell-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-system-monitor-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          7          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-terminal-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          7          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-themes-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          gnome-user-docs-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          2          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-utils-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gnome-vfs2-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gnome-vfs2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gnome-vfs2-smb-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnome-volume-manager-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnomemeeting-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnopernicus-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gnupg-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          gnutls-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gok-0      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          11         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gpdf-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
4          []         NONE       1          gpg-pubkey-443e1821-421f218f[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       (          none       )          2          2          .          rpm        
6          []         NONE       1          gphoto2-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gpm-1      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          20         NONE       1          ~71        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gpm-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          20         NONE       1          ~71        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          grep-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~32        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          groff-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          18         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          grub-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          95         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          gstreamer-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          gstreamer-plugins-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gthumb-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          gtk        NONE       2          +          NONE       1          ~1         NONE       NONE       NONE       NONE       1          2          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~33        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          gtk-doc-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          2          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gtk-engines-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        2          []         NONE       1          12         NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gtk2-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~19        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gtk2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~19        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gtk2-engines-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          2          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gtkhtml2-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gtkhtml3-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          3          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gtksourceview-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          gtkspell-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          guile-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~14        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          gzip-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          3          NONE       1          ~16        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          hal-0      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          4          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          hal-cups-utils-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          hdparm-5   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          hesiod-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~30        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          hesiod-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~30        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          hicolor-icon-theme-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          3          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          hotplug-2004_04_01-7[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          7          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          hpijs-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          6          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          htdig-3    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          0          NONE       NONE       NONE       NONE       NONE       1          b6-3       1          []         NONE       1          2          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          htmlview-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          0          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          hwbrowser-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          19         NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          hwdata-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL-1       1          []         NONE       1          146        NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          ibmasm-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ical-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i686       1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          iiimf-libs-12[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          1          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          iiimf-libs-devel-12[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          1          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          imlib-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~23        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          indent-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          indexhtml-4-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          info-4     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          initscripts-7[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL-1       1          []         NONE       1          93         NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          intltool-0 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          31         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          iproute-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          6          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          ipsec-tools-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          3          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          iptables-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          2          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          iptraf-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          7          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          iptstate-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          3          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          iputils-20020927-18[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          3          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          irda-utils-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          isdn4k-utils-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          p1         2          []         NONE       1          2          NONE       1          ~18        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          jadetex-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          12         NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          java-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         2          2          ~27        NONE       NONE       -          3          1          gcj-compat-11          []         NONE       1          4          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          joe-3      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      2          []         NONE       1          1          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          jpackage-utils-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         2          0          ~2         NONE       NONE       NONE       NONE       1          jpp_3rh    1          []         NONE       1          6          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          jwhois-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          kbd-1      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          12         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          kdbg-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          kdeaddons-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          kdeartwork-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kdebase-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kdebase-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kdegraphics-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kdegraphics-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kdelibs-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          3          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kdelibs-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          3          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          kdemultimedia-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kdenetwork-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kdenetwork-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kdepim-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kdepim-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          kdesdk-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          kdesdk-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          kdeutils-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          kdeutils-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          kdevelop-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          kernel-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          6          NONE       1          ~42        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kernel-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          6          NONE       1          ~42        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          kernel-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          6          NONE       1          ~42        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kernel-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          6          NONE       1          ~42        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          kernel-hugemem-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          6          NONE       1          ~42        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          kernel-ib-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          kernel-smp-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          6          NONE       1          ~42        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kernel-smp-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          6          NONE       1          ~42        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          kernel-smp-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          6          NONE       1          ~42        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kernel-utils-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          4          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          keyutils-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          keyutils-libs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          krb5-auth-dialog-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          krb5-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~33        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          krb5-libs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~33        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          krb5-workstation-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~33        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          krbafs-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          krbafs-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kudzu-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          kudzu-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
4          []         NONE       1          less-382-4 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          lftp-3     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          lha-1      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          14         NONE       NONE       NONE       NONE       NONE       1          i-17       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libIDL-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libIDL-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libacl-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libacl-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libao-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libart_lgpl-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libart_lgpl-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libattr-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libattr-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          libavc1394-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          4          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libbonobo-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libbonobo-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          libbonoboui-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          99         NONE       NONE       NONE       NONE       NONE       1          cvs20040929-21          []         NONE       1          8          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          libbonoboui-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          99         NONE       NONE       NONE       NONE       NONE       1          cvs20040929-21          []         NONE       1          8          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          libcap-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          10         NONE       1          ~20        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          libcap-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          10         NONE       1          ~20        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libcroco-0 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libcroco-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libdbi-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          6          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libdbi-dbd-mysql-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          6          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          libdv-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          103        NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          libexif-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libf2c-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgail-gnome-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgal2-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgcc-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgcj-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgcj-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          libgcj4-4  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~18        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgcrypt-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          libgfortran-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~18        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libglade2-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libglade2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgnat-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgnome-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgnome-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgnomecanvas-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgnomecanvas-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgnomecups-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgnomeprint22-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgnomeprintui22-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgnomeui-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgnomeui-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          libgomp-4  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~18        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          libgpg-error-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgsf-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          10         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgsf-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          10         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          libgssapi-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgtop2-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libgtop2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libibverbs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libidn-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libidn-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libieee1284-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libieee1284-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
4          []         NONE       1          libjpeg-6b-33[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
4          []         NONE       1          libjpeg-devel-6b-33[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libmng-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libmng-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libmthca-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libmusicbrainz-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libobjc-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libogg-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libogg-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          libpcap-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          8          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libpng-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libpng-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libpng10-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libpng10-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libraw1394-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          10         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          librdmacm-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          librsvg2-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          librsvg2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libsdp-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          libselinux-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          19         NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          libselinux-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          19         NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libsepol-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libsilc-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libsoup-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          libstdc    NONE       3          +          +          1          ~3         NONE       NONE       NONE       NONE       1          2          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          libstdc    NONE       4          +          +          NONE       NONE       NONE       NONE       -          devel-3    2          2          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libtermcap-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~39        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libtermcap-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~39        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          libtheora-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          0          NONE       NONE       NONE       NONE       NONE       1          alpha3-5   NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libtiff-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libtiff-devel-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          libtool-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          5          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          libtool-libs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          5          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libungif-4 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libungif-devel-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libusb-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libusb-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libuser-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          52         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libuser-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          52         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libvorbis-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libvorbis-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libwmf-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          libwnck-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libwvstreams-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          75         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          libxklavier-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          2          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libxml2-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libxml2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libxml2-python-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libxslt-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          libxslt-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          linuxdoc-tools-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~14        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          linuxwacom-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         2          0          NONE       NONE       NONE       -          3          1          EL4        1          []         NONE       1          7          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          lksctp-tools-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          0          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          lksctp-tools-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          0          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          lm_sensors-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          lockdev-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          lockdev-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          logrotate-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          7          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          logwatch-5 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          lrzsz-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          12         NONE       1          ~19        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          lslk-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          29         NONE       1          ~12        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          lsof-4     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          72         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          ltrace-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          lvm2-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          m4-1       []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~16        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          mailcap-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          mailx-8    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~36        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          make-3     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          80         NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          man-1      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          5          NONE       NONE       NONE       NONE       NONE       1          o1-9       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          man-pages-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          67         NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          mdadm-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          memtest86  NONE       2          +          NONE       1          ~1         NONE       NONE       NONE       NONE       1          2          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          26         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          metacity-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          mgetty-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          mikmod-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~32        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          mingetty-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          minicom-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~19        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          mkbootdisk-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          mkinitrd-4 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          mkisofs-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          mktemp-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          5          NONE       1          ~20        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          module-init-tools-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          pre5       2          []         NONE       1          1          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          mpage-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          mt-st-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          mtools-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          mtr-0      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          54         NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          mutt-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          4          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          mx-2       []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          mysql-4    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          mysql-devel-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          mysqlclient10-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          23         NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          nano-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          nasm-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          98         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          nautilus-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          nautilus-cd-burner-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          nautilus-media-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          nc-1       []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          10         NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          ncurses-5  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          4          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          ncurses-devel-5[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          4          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          nedit-5    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          neon-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          24         NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          net-snmp-5 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          net-snmp-libs-5[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          net-snmp-utils-5[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          net-tools-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          60         NONE       1          ~37        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          netconfig-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          netdump-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          7          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          netpbm-10  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          25         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          netpbm-devel-10[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          25         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          netpbm-progs-10[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          25         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          newt-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          51         NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          newt-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          51         NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          nfs-utils-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~70        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          nfs-utils-lib-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          nmap-3     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          70         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          nmap-frontend-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          70         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          nscd-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          nss_db-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          2          NONE       1          ~29        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
4          []         NONE       1          nss_ldap-226-13[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          ntp-4      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          a          1          []         NONE       1          2          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          ntsysv-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          numactl-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          open-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          4          NONE       1          ~21        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          openh323-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          13         NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          openjade-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~14        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          openldap-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          openldap-clients-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          openldap-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          openmotif-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          2          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          openmotif-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          2          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          openoffice []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          org-1      1          []         NONE       1          1          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          openoffice []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          org-i18n-1 1          []         NONE       1          1          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          openoffice []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          org-libs-1 1          []         NONE       1          1          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          openssh-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          9          NONE       NONE       NONE       NONE       NONE       1          p1-8       1          []         NONE       1          17         NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          openssh-clients-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          9          NONE       NONE       NONE       NONE       NONE       1          p1-8       1          []         NONE       1          17         NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          openssl-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          7          NONE       NONE       NONE       NONE       NONE       1          a-43       1          []         NONE       1          9          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          openssl-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          7          NONE       NONE       NONE       NONE       NONE       1          a-43       1          []         NONE       1          9          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          oprofile-0 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~23        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pam-0      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          77         NONE       1          ~66        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pam-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          77         NONE       1          ~66        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
4          []         NONE       1          pam_ccreds-1-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pam_krb5-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pam_passwdqc-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          7          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pam_smb-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pango-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pango-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          parted-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          6          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          passivetex-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          25         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          passwd-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          68         NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          patch-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~20        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          patchutils-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          pax-3      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          pciutils-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          test8-3    1          []         NONE       1          1          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          pciutils-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          test8-3    1          []         NONE       1          1          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          pcmcia-cs-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          pcre-4     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      2          []         NONE       1          5          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          pcre-devel-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      2          []         NONE       1          5          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          pdksh-5    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~30        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          perl-5     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          8          NONE       1          ~36        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-Convert-ASN1-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          18         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-Crypt-SSLeay-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          51         NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          perl-DBD-MySQL-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          9004       NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-DBI-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          40         NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-DateManip-5[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          42         NONE       NONE       NONE       NONE       NONE       1          a-3        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-Digest-HMAC-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          1          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-Digest-SHA1-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-Filter-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          30         NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-HTML-Parser-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          35         NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-HTML-Tagset-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          3          NONE       1          ~30        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-LDAP-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          31         NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-Net-DNS-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          48         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-Parse-Yapp-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          5          NONE       1          ~32        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-SGMLSpm-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          3          NONE       NONE       NONE       NONE       NONE       1          ii-14      NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-Time-HiRes-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          55         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-URI-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          30         NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-XML-Dumper-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          71         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-XML-Encoding-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          1          NONE       1          ~26        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-XML-Grove-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          46         NONE       NONE       NONE       NONE       NONE       1          alpha-27   NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-XML-LibXML-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          58         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-XML-LibXML-Common-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          13         NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-XML-NamespaceSupport-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          8          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-XML-Parser-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          34         NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-XML-SAX-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          12         NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-XML-Twig-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          13         NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-libwww-perl-5[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          79         NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-libxml-enno-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          2          NONE       1          ~31        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          perl-libxml-perl-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          7          NONE       1          ~30        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pilot-link-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          11         NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pilot-link-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          11         NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pinfo-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pkgconfig-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          15         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          planner-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          12         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          pmake-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          45         NONE       1          ~16        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          pnm2ppa-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          4          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          policycoreutils-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          18         NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          popt-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         3          1          ~18        NONE       _          NONE       2          1          nonptl     1          []         NONE       1          9          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          portmap-4  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~63        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          ppp-2      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          4          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          prelink-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          3          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          procmail-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          22         NONE       1          ~14        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          procps-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          psacct-6   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          3          NONE       1          ~38        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          psgml-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          2          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          psmisc-21  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          4          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          pstack-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          psutils-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          17         NONE       1          ~23        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pwlib-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pyOpenSSL-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          p23        2          []         NONE       1          6          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pygtk2-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pygtk2-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pygtk2-libglade-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pyorbit-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pyparted-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          python-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~14        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          python-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~14        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          python-elementtree-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          python-ldap-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          python-sqlite-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          python-urlgrabber-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          9          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          pyxf86config-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          qt-3       []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          qt-designer-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          qt-devel-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          quota-3    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          12         NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          rcs-5      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~26        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          rdate-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          4          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          rdist-6    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~38        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          readline-4 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          3          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          readline-devel-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          3          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          redhat-artwork-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          2          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          120        NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          redhat-logos-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          redhat-lsb-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         2          []         NONE       1          0          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          redhat-menus-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          7          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          redhat-rpm-config-8[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          0          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          rhgb-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          14         NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          rhn-applet-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          1          NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          rhnlib-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          8          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          rhpl-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          148        NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          rmt-0      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          b39-3      1          []         NONE       1          2          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
4          []         NONE       1          rootfiles-8-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          rp-pppoe-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          5          NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          rpm-4      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         3          3          ~18        NONE       _          NONE       2          1          nonptl     1          []         NONE       1          3          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          rpm-build-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         3          3          ~18        NONE       _          NONE       2          1          nonptl     1          []         NONE       1          3          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          rpm-devel-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         3          3          ~18        NONE       _          NONE       2          1          nonptl     1          []         NONE       1          3          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          rpm-libs-4 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         3          3          ~18        NONE       _          NONE       2          1          nonptl     1          []         NONE       1          3          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          rpm-python-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         3          3          ~18        NONE       _          NONE       2          1          nonptl     1          []         NONE       1          3          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          rpmdb-CentOS-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          4          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          rsync-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          ruby-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          8          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          ruby-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          8          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          ruby-libs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          8          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          ruby-mode-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          8          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          rusers-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          17         NONE       1          ~41        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          rusers-server-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          17         NONE       1          ~41        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          rwho-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          17         NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          samba-client-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          samba-common-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          sane-backends-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          sane-backends-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          sane-frontends-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          schedutils-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          screen-4   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          scrollkeeper-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          seamonkey-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          0          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          seamonkey-mail-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          0          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          seamonkey-nspr-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          0          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
9          []         NONE       1          seamonkey-nss-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          0          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          sed-4      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          selinux-doc-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          14         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          selinux-policy-targeted-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          17         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          sendmail-8 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          13         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          setarch-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          6          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          setools-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          setserial-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          17         NONE       1          ~17        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          setup-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          5          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          setuptool-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          17         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          sgml-common-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          6          NONE       1          ~17        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          shadow-utils-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          0          NONE       1          ~60        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          shared-mime-info-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          15         NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          sip-4      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          sip-devel-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          slang-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          slang-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          slocate-2  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        2          []         NONE       1          7          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          sox-12     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          17         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          spamassassin-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          specspo-9  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          speex-1    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          splint-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          sqlite-3   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          startup-notification-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          startup-notification-devel-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          statserial-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          1          NONE       1          ~35        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          strace-4   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          5          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          stunnel-4  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          5          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          subversion-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          ent        1          []         NONE       1          1          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          sudo-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          7          NONE       NONE       NONE       NONE       NONE       1          p5-30      1          []         NONE       1          6          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          swig-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          switchdesk-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          0          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          symlinks-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          2          NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          synaptics-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          13         NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          sysfsutils-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          sysfsutils-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          sysklogd-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         3          1          ~26        NONE       _          NONE       2          1          EL         1          []         NONE       1          4          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          syslinux-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          11         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          sysreport-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          3          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          sysstat-5  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      1          []         NONE       1          0          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          system-config-boot-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          system-config-date-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          7          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          system-config-display-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          system-config-keyboard-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          system-config-kickstart-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          5          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          system-config-language-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          system-config-lvm-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          system-config-mouse-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          system-config-network-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          system-config-network-tui-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          3          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          system-config-nfs-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          system-config-packages-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          system-config-printer-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          system-config-printer-gui-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          system-config-rootpassword-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          system-config-securitylevel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          system-config-securitylevel-tui-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          system-config-services-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          system-config-soundcard-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          2          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          system-config-users-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          2          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          system-logviewer-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          9          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          systemtap-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          5          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          talk-0     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          17         NONE       1          ~26        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          tar-1      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      2          []         NONE       1          14         NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          tcl-8      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          tclx-8     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          3          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          tcp_wrappers-7[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          6          NONE       1          ~37        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          tcpdump-3  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          8          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          tcsh-6     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          13         NONE       1          ~9         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          telnet-0   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          17         NONE       1          ~31        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          termcap-5  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          tetex-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          tetex-afm-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          tetex-dvips-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          tetex-fonts-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          tetex-latex-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          tetex-xdvi-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          0          NONE       1          ~22        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          texinfo-4  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          thunderbird-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          5          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          time-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          7          NONE       1          ~25        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          tk-8       []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          tmpwatch-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          9          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          tog-pegasus-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          5          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          tog-pegasus-devel-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          5          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          traceroute-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          a12-24     NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          transfig-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          ttfprint-0 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          9          NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          ttmkfdir-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          0          NONE       1          ~14        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          tzdata-2006m-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          udev-039-10[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          15         NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          umb-scheme-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          2          NONE       1          ~36        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          unix2dos-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          2          NONE       1          ~24        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          unixODBC-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          unzip-5    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          51         NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          up2date-4  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          4          NONE       1          ~25        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          up2date-gnome-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          centos4    1          []         NONE       1          4          NONE       1          ~25        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          urw-fonts-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          2          NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          usbutils-0 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          11         NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          usermode-1 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          74         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          usermode-gtk-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          74         NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          utempter-0 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          5          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          util-linux-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          12         NONE       NONE       NONE       NONE       NONE       1          a-16       1          []         NONE       1          20         NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          valgrind-3 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          1          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          valgrind-callgrind-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          10         NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          vconfig-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          8          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          vim-common-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          40         NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          3          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          vim-enhanced-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          40         NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          3          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          vim-minimal-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          40         NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          3          NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          vino-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          vixie-cron-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        2          []         NONE       1          1          NONE       1          ~44        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          vte-0      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          el4        1          []         NONE       1          11         NONE       1          ~6         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          w3c-libwww-5[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          4          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          wget-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          40         NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          10         NONE       1          0          NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          which-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          16         NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          wireless-tools-28-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          pre16      1          []         NONE       1          3          NONE       1          NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          words-3    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          0          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          wvdial-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          54         NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          xchat-2    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        1          []         NONE       1          4          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          xdelta-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~15        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          xemacs-21  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          4          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          xemacs-common-21[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          4          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          xemacs-el-21[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          4          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          xemacs-info-21[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          4          NONE       1          ~10        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
4          []         NONE       1          xemacs-sumo-20040818-2[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          xfig-3     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          xhtml1-dtds-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     2          []         NONE       1          0          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          xinetd-2   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         1          4          NONE       NONE       NONE       NONE       NONE       1          E          1          []         NONE       1          3          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          xinitrc-4  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          0          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          xloadimage-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          RHEL4      2          []         NONE       1          1          NONE       1          ~36        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          xml-common-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     1          []         NONE       1          6          NONE       1          ~17        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          xmlsec1-1  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          xmlsec1-openssl-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
4          []         NONE       1          xmltex-20020625-3[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          noarch     NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          xmlto-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          0          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-6 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-Mesa-libGL-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-Mesa-libGLU-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-deprecated-libs-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-deprecated-libs-devel-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-devel-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-font-utils-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-libs-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-tools-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-twm-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-xauth-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-xdm-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
10         []         NONE       1          xorg-x11-xfs-6[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          8          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          xpdf-3     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          0          NONE       1          ~11        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          xrestop-0  []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          2          NONE       1          ~4         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          xsane-0    []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          92         NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          xsane-gimp-0[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          92         NONE       1          ~13        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          xscreensaver-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          rhel4      2          []         NONE       1          18         NONE       1          ~5         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          xsri-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          1          NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          xterm-192-4[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL4        NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          yelp-2     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          6          NONE       1          ~2         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          yp-tools-2 []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          8          NONE       1          ~7         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
6          []         NONE       1          ypbind-1   []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          17         NONE       1          ~8         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
7          []         NONE       1          yum-2      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          c4         1          []         NONE       1          4          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
5          []         NONE       1          zip-2      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       2          []         NONE       1          3          NONE       1          ~27        NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          zlib-1     []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          zlib-devel-1[]         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          i386       1          []         NONE       1          2          NONE       1          ~1         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
8          []         NONE       1          zsh-4      []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          .          []         NONE       NONE       NONE       NONE       NONE       NONE       NONE       1          EL         1          []         NONE       1          2          NONE       1          ~3         NONE       NONE       NONE       NONE       NONE       1          .          rpm        
Dependencies (85):
{} -> BTy_13
{} -> BTy_18
{} -> BTy_25
{} -> BTy_40
{} -> BTy_42
{} -> BTy_48
{} -> BTy_50
{BTy_3,} -> BTy_1
{BTy_1,} -> BTy_3
{BTy_4,} -> BTy_1
{BTy_4,} -> BTy_3
{BTy_4,} -> BTy_2
{BTy_4,} -> BTy_5
{BTy_4,} -> BTy_8
{BTy_4,} -> BTy_9
{BTy_4,} -> BTy_6
{BTy_4,} -> BTy_20
{BTy_4,} -> BTy_22
{BTy_4,} -> BTy_23
{BTy_4,} -> BTy_24
{BTy_4,} -> BTy_27
{BTy_4,} -> BTy_29
{BTy_4,} -> BTy_26
{BTy_4,} -> BTy_21
{BTy_4,} -> BTy_34
{BTy_4,} -> BTy_36
{BTy_4,} -> BTy_35
{BTy_4,} -> BTy_39
{BTy_4,} -> BTy_44
{BTy_4,} -> BTy_45
{BTy_4,} -> BTy_46
{BTy_4,} -> BTy_43
{BTy_4,} -> BTy_32
{BTy_7,} -> BTy_5
{BTy_8,} -> BTy_5
{BTy_5,} -> BTy_8
{BTy_17,} -> BTy_5
{BTy_7,} -> BTy_8
{BTy_7,} -> BTy_9
{BTy_7,} -> BTy_6
{BTy_17,} -> BTy_8
{BTy_11,} -> BTy_10
{BTy_11,} -> BTy_12
{BTy_15,} -> BTy_14
{BTy_14,} -> BTy_15
{BTy_17,} -> BTy_6
{BTy_30,} -> BTy_20
{BTy_44,} -> BTy_20
{BTy_20,} -> BTy_44
{BTy_45,} -> BTy_20
{BTy_20,} -> BTy_45
{BTy_46,} -> BTy_20
{BTy_20,} -> BTy_46
{BTy_43,} -> BTy_20
{BTy_20,} -> BTy_43
{BTy_22,} -> BTy_27
{BTy_24,} -> BTy_27
{BTy_30,} -> BTy_21
{BTy_44,} -> BTy_21
{BTy_21,} -> BTy_44
{BTy_45,} -> BTy_21
{BTy_21,} -> BTy_45
{BTy_46,} -> BTy_21
{BTy_21,} -> BTy_46
{BTy_43,} -> BTy_21
{BTy_21,} -> BTy_43
{BTy_32,} -> BTy_21
{BTy_21,} -> BTy_32
{BTy_30,} -> BTy_44
{BTy_30,} -> BTy_45
{BTy_30,} -> BTy_46
{BTy_30,} -> BTy_43
{BTy_33,} -> BTy_39
{BTy_37,} -> BTy_34
{BTy_37,} -> BTy_36
{BTy_37,} -> BTy_35
{BTy_35,} -> BTy_39
{BTy_45,} -> BTy_44
{BTy_44,} -> BTy_45
{BTy_46,} -> BTy_44
{BTy_44,} -> BTy_46
{BTy_46,} -> BTy_45
{BTy_45,} -> BTy_46
{BTy_32,} -> BTy_43
{BTy_43,} -> BTy_32

After reduction:
Pstruct(Id = BTy_101 886)
	Switch(BTy_4):
	case "*":
		""(Id = BTy_1 885);
	case "Suite-1":
		[4](Id = BTy_3 1);
	End Switch;
	[string](Id = BTy_4 886);
	Switch(BTy_4):
	case "*":
		""(Id = BTy_5 877);
	case "compat-gcc-32-c":
		Parray(Id = BTy_7 9)((+) )
		First:
			"+"(Id = BTy_8 9);
		Body:
			"+"(Id = BTy_9 7);
		Tail:
			Punion(Id = BTy_17 9)
				Parray(Id = BTy_10 8)([int] )
				First:
					[int](Id = BTy_11 8);
				Body:
					[int](Id = BTy_12 2);
				Tail:
					""(Id = BTy_13 8);
				End Parray;
				"-devel-3"(Id = BTy_14 1);
			End Punion;
		End Parray;
	End Switch;
	"."(Id = BTy_18 886);
	RArray(Id = BTy_0 886)
		Separator: "."
		Terminator: ".rpm"
		Switch(BTy_4):
		case "*":
			Pstruct(Id = BTy_31 1220)
				Punion(Id = BTy_21 1220)
					""(Id = BTy_20 1156);
					Parray(Id = BTy_22 64)([int] )
					First:
						[int](Id = BTy_23 64);
					Body:
						[int](Id = BTy_24 11);
					Tail:
						Punion(Id = BTy_26 64)
							""(Id = BTy_25 51);
							"_"(Id = BTy_27 9);
							"-"(Id = BTy_29 4);
						End Punion;
					End Parray;
				End Punion;
				[string](Id = BTy_30 1220);
			End Pstruct;
		case "gpg-pubkey-443e1821-421f218f":
			Parray(Id = BTy_33 1823)([int] )
			First:
				Punion(Id = BTy_39 1823)
					Pstruct(Id = BTy_38 1822)
						Switch(BTy_4):
						case "*":
							""(Id = BTy_34 1820);
						case "binutils-2":
							[IP](Id = BTy_36 2);
						End Switch;
						[int](Id = BTy_37 1822);
					End Pstruct;
					""(Id = BTy_40 1);
				End Punion;
			Body:
				[int](Id = BTy_41 819);
			Tail:
				Punion(Id = BTy_43 1823)
					""(Id = BTy_42 1822);
					"(none)"(Id = BTy_44 1);
				End Punion;
			End Parray;
		End Switch;
	End RArray;
	".rpm"(Id = BTy_48 3043);
End Pstruct

Complexity of inferred type:
	numAlt = 7  numTBD = 0  numBtm = 0
Outputing partitions to directory: gen/
