Source file to process: data/rpmpkgs.txt
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
Starting on file data/rpmpkgs.txt
886 records.
Histogram of number of tokens per record:
	4:	102
	5:	6
	7:	3
	8:	117
	9:	3
	10:	324
	11:	2
	12:	181
	13:	14
	14:	77
	15:	15
	16:	19
	17:	2
	18:	20
	21:	1

Building histograms...
THRESHOLD for histogram equality: 9.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 2855.
Number of records with at least one token occurrence: 886.
StructScore: 693.
	2:	103	0.11625282167
	3:	504	0.568848758465
	4:	258	0.291196388262
	5:	21	0.0237020316027


Cluster 1:
Token: [Host]
Total number of token occurrences: 107.
Number of records with at least one token occurrence: 107.
StructScore: 779.
	1:	107	0.120767494357


Cluster 2:
Token: [other](_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 877.
	1:	9	0.010158013544

Token: [other](-)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 880.
	1:	6	0.00677200902935

Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 884.
	1:	2	0.00225733634312

Token: [other](()[Group Body][other]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 885.
	1:	1	0.00112866817156


Cluster 3:
Token: [other](+)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 9.
StructScore: 2633.
	1:	2	0.00225733634312
	2:	7	0.0079006772009


Cluster 4:
Token: [other](.)
Total number of token occurrences: 3615.
Number of records with at least one token occurrence: 886.
StructScore: 2985.
	1:	102	0.115124153499
	2:	6	0.00677200902935
	3:	123	0.138826185102
	4:	336	0.379232505643
	5:	187	0.211060948081
	6:	92	0.103837471783
	7:	21	0.0237020316027
	8:	18	0.020316027088
	10:	1	0.00112866817156


Cluster 5:
Token: [int]
Total number of token occurrences: 2449.
Number of records with at least one token occurrence: 777.
StructScore: 3460.
	1:	14	0.0158013544018
	2:	134	0.151241534989
	3:	423	0.477426636569
	4:	149	0.168171557562
	5:	40	0.0451467268623
	6:	17	0.0191873589165


Junk Tolerance Threshold: 89
Coverage: 886
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [other](.)
Total number of token occurrences: 3615.
Number of records with at least one token occurrence: 886.
StructScore: 2985.
	1:	102	0.115124153499
	2:	6	0.00677200902935
	3:	123	0.138826185102
	4:	336	0.379232505643
	5:	187	0.211060948081
	6:	92	0.103837471783
	7:	21	0.0237020316027
	8:	18	0.020316027088
	10:	1	0.00112866817156


Cluster 1:
Token: [string]
Total number of token occurrences: 2855.
Number of records with at least one token occurrence: 886.
StructScore: 693.
	2:	103	0.11625282167
	3:	504	0.568848758465
	4:	258	0.291196388262
	5:	21	0.0237020316027


Cluster 2:
Token: [int]
Total number of token occurrences: 2449.
Number of records with at least one token occurrence: 777.
StructScore: 3460.
	1:	14	0.0158013544018
	2:	134	0.151241534989
	3:	423	0.477426636569
	4:	149	0.168171557562
	5:	40	0.0451467268623
	6:	17	0.0191873589165


Cluster 3:
Token: [Host]
Total number of token occurrences: 107.
Number of records with at least one token occurrence: 107.
StructScore: 779.
	1:	107	0.120767494357


Cluster 4:
Token: [other](_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 877.
	1:	9	0.010158013544

Token: [other](-)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 880.
	1:	6	0.00677200902935

Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 884.
	1:	2	0.00225733634312

Token: [other](()[Group Body][other]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 885.
	1:	1	0.00112866817156


Cluster 5:
Token: [other](+)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 9.
StructScore: 2633.
	1:	2	0.00225733634312
	2:	7	0.0079006772009


Possible array tokens:
[other](.)
Records in possible array context:886
Total:3615
Coverage:886
Width:9
Array	[other](.)	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 9.
Computed clusters
Cluster 0:
Token: [other](.)
Total number of token occurrences: 886.
Number of records with at least one token occurrence: 886.
StructScore: 0.
	1:	886	1.0

Token: [string]
Total number of token occurrences: 886.
Number of records with at least one token occurrence: 885.
StructScore: 4.
	1:	884	0.997742663657
	2:	1	0.00112866817156


Cluster 1:
Token: [Host]
Total number of token occurrences: 107.
Number of records with at least one token occurrence: 107.
StructScore: 779.
	1:	107	0.120767494357


Cluster 2:
Token: [other](-)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 3.
StructScore: 883.
	1:	3	0.00338600451467


Cluster 3:
Token: [int]
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 10.
StructScore: 2630.
	1:	8	0.00902934537246
	2:	2	0.00225733634312

Token: [other](+)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 9.
StructScore: 2633.
	1:	2	0.00225733634312
	2:	7	0.0079006772009


Junk Tolerance Threshold: 89
Coverage: 884
Num Tokens: 2
Struct
Coverage:884
Token count:2
[other](.)	Occurrences:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 9.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 778.
Number of records with at least one token occurrence: 778.
StructScore: 107.
	1:	778	0.879096045198


Cluster 1:
Token: [Host]
Total number of token occurrences: 106.
Number of records with at least one token occurrence: 106.
StructScore: 779.
	1:	106	0.119774011299


Cluster 2:
Token: [other](-)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 883.
	1:	2	0.00225988700565

Token: [int]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 884.
	1:	1	0.00112994350282


Junk Tolerance Threshold: 89
Coverage: 778
Num Tokens: 1
Struct
Coverage:778
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 2.
Computed clusters
Cluster 0:
Token: [Host]
Total number of token occurrences: 106.
Number of records with at least one token occurrence: 106.
StructScore: 1.
	1:	106	0.990654205607


Cluster 1:
Token: [other](-)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 105.
	1:	2	0.018691588785

Token: [int]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 106.
	1:	1	0.00934579439252


Junk Tolerance Threshold: 11
Coverage: 106
Num Tokens: 1
Struct
Coverage:106
Token count:1
[Host]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 2.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 104.
Number of records with at least one token occurrence: 104.
StructScore: 2.
	1:	104	0.981132075472


Cluster 1:
Token: [other](-)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 104.
	1:	2	0.0188679245283


Junk Tolerance Threshold: 11
Coverage: 104
Num Tokens: 1
Struct
Coverage:104
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](-)
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
[other](-)	Occurrences:1
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
Total number of token occurrences: 876.
Number of records with at least one token occurrence: 876.
StructScore: 9.
	1:	876	0.989830508475


Cluster 1:
Token: [string]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 884.
	1:	1	0.00112994350282

Token: [other](-)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 884.
	1:	1	0.00112994350282


Cluster 2:
Token: [other](+)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 9.
StructScore: 2630.
	1:	2	0.00225988700565
	2:	7	0.00790960451977

Token: [int]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 8.
StructScore: 2633.
	1:	6	0.00677966101695
	2:	2	0.00225988700565


Junk Tolerance Threshold: 89
Coverage: 876
Num Tokens: 1
Struct
Coverage:876
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](+)
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

Token: [other](-)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111


Junk Tolerance Threshold: 1
Coverage: 9
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [other](+)
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

Token: [other](-)
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 8.
	1:	1	0.111111111111


Possible array tokens:
[other](+)
Records in possible array context:9
Total:16
Coverage:9
Width:2
Array	[other](+)	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](+)
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
[other](+)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](+)
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
[other](+)	Occurrences:1
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

Token: [other](-)
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

Token: [other](-)
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

Token: [other](-)
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
[other](-)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Host]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0

Token: [int]
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
[int]	Occurrences:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 28.
Computed clusters
Cluster 0:
Token: [other](.)
Total number of token occurrences: 2729.
Number of records with at least one token occurrence: 2729.
StructScore: 0.
	1:	2729	1.0


Cluster 1:
Token: [string]
Total number of token occurrences: 1083.
Number of records with at least one token occurrence: 1083.
StructScore: 1646.
	1:	1083	0.396848662514


Cluster 2:
Token: [other](_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 2720.
	1:	9	0.00329791132283

Token: [other](-)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 3.
StructScore: 2726.
	1:	3	0.00109930377428

Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 2727.
	1:	2	0.000732869182851

Token: [other](()[Group Body][other]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2728.
	1:	1	0.000366434591425


Cluster 3:
Token: [int]
Total number of token occurrences: 2437.
Number of records with at least one token occurrence: 1699.
StructScore: 3828.
	1:	961	0.35214364236
	2:	738	0.270428728472


Junk Tolerance Threshold: 273
Coverage: 2729
Num Tokens: 1
Struct
Coverage:2729
Token count:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 28.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 1083.
Number of records with at least one token occurrence: 1083.
StructScore: 1646.
	1:	1083	0.396848662514


Cluster 1:
Token: [other](_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 2720.
	1:	9	0.00329791132283

Token: [other](-)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 3.
StructScore: 2726.
	1:	3	0.00109930377428

Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 2727.
	1:	2	0.000732869182851

Token: [other](()[Group Body][other]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 2728.
	1:	1	0.000366434591425


Cluster 2:
Token: [int]
Total number of token occurrences: 2437.
Number of records with at least one token occurrence: 1699.
StructScore: 3828.
	1:	961	0.35214364236
	2:	738	0.270428728472


Junk Tolerance Threshold: 273
Coverage: 1083
Num Tokens: 1
Struct
Coverage:1083
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 11.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 1029.
Number of records with at least one token occurrence: 1029.
StructScore: 54.
	1:	1029	0.950138504155


Cluster 1:
Token: [other](_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 1074.
	1:	9	0.00831024930748

Token: [other](-)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 3.
StructScore: 1080.
	1:	3	0.00277008310249


Cluster 2:
Token: [int]
Total number of token occurrences: 63.
Number of records with at least one token occurrence: 54.
StructScore: 3096.
	1:	45	0.0415512465374
	2:	9	0.00831024930748


Junk Tolerance Threshold: 109
Coverage: 1029
Num Tokens: 1
Struct
Coverage:1029
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 63.
Number of records with at least one token occurrence: 54.
StructScore: 9.
	1:	45	0.833333333333
	2:	9	0.166666666667


Cluster 1:
Token: [other](_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 45.
	1:	9	0.166666666667


Cluster 2:
Token: [other](-)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 3.
StructScore: 51.
	1:	3	0.0555555555556


Junk Tolerance Threshold: 6
Coverage: 54
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [int]
Total number of token occurrences: 63.
Number of records with at least one token occurrence: 54.
StructScore: 9.
	1:	45	0.833333333333
	2:	9	0.166666666667


Cluster 1:
Token: [other](_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 45.
	1:	9	0.166666666667


Cluster 2:
Token: [other](-)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 3.
StructScore: 51.
	1:	3	0.0555555555556


Possible array tokens:
[int]
Records in possible array context:54
Total:63
Coverage:54
Width:2
Array	[int]	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 54.
Number of records with at least one token occurrence: 54.
StructScore: 0.
	1:	54	1.0


Junk Tolerance Threshold: 6
Coverage: 54
Num Tokens: 1
Struct
Coverage:54
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
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
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 42.
Number of records with at least one token occurrence: 42.
StructScore: 12.
	1:	42	0.777777777778


Cluster 1:
Token: [other](_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 45.
	1:	9	0.166666666667


Cluster 2:
Token: [other](-)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 3.
StructScore: 51.
	1:	3	0.0555555555556


Junk Tolerance Threshold: 6
Coverage: 42
Num Tokens: 1
Struct
Coverage:42
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](_)
Total number of token occurrences: 9.
Number of records with at least one token occurrence: 9.
StructScore: 3.
	1:	9	0.75


Cluster 1:
Token: [other](-)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 3.
StructScore: 9.
	1:	3	0.25


Junk Tolerance Threshold: 2
Coverage: 9
Num Tokens: 1
Struct
Coverage:9
Token count:1
[other](_)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](-)
Total number of token occurrences: 3.
Number of records with at least one token occurrence: 3.
StructScore: 0.
	1:	3	1.0


Junk Tolerance Threshold: 1
Coverage: 3
Num Tokens: 1
Struct
Coverage:3
Token count:1
[other](-)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 17.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 2374.
Number of records with at least one token occurrence: 1645.
StructScore: 732.
	1:	916	0.556500607533
	2:	729	0.442891859052


Cluster 1:
Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 1644.
	1:	2	0.00121506682868

Token: [other](()[Group Body][other]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1645.
	1:	1	0.000607533414338


Junk Tolerance Threshold: 165
Coverage: 1646
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [int]
Total number of token occurrences: 2374.
Number of records with at least one token occurrence: 1645.
StructScore: 732.
	1:	916	0.556500607533
	2:	729	0.442891859052


Cluster 1:
Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 1644.
	1:	2	0.00121506682868

Token: [other](()[Group Body][other]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1645.
	1:	1	0.000607533414338


Possible array tokens:
[int]
Records in possible array context:1646
Total:2374
Coverage:1645
Width:2
Array	[int]	Occurrences:1
WARNING: ARRAY first context empty!Array context
Building histograms...
THRESHOLD for histogram equality: 17.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 1645.
Number of records with at least one token occurrence: 1645.
StructScore: 1.
	1:	1645	0.999392466586


Cluster 1:
Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 1644.
	1:	2	0.00121506682868

Token: [empty]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1645.
	1:	1	0.000607533414338


Junk Tolerance Threshold: 165
Coverage: 1645
Num Tokens: 1
Struct
Coverage:1645
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 17.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 1643.
Number of records with at least one token occurrence: 1643.
StructScore: 2.
	1:	1643	0.998784194529


Cluster 1:
Token: [IP]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 1643.
	1:	2	0.00121580547112


Junk Tolerance Threshold: 165
Coverage: 1643
Num Tokens: 1
Struct
Coverage:1643
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
THRESHOLD for histogram equality: 8.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 729.
Number of records with at least one token occurrence: 729.
StructScore: 0.
	1:	729	1.0


Junk Tolerance Threshold: 73
Coverage: 729
Num Tokens: 1
Struct
Coverage:729
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 17.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 1645.
Number of records with at least one token occurrence: 1645.
StructScore: 1.
	1:	1645	0.999392466586


Cluster 1:
Token: [other](()[Group Body][other]())
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 1645.
	1:	1	0.000607533414338


Junk Tolerance Threshold: 165
Coverage: 1645
Num Tokens: 1
Struct
Coverage:1645
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

Token: [other](()
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 0.
	1:	1	1.0

Token: [other]())
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
[other](()	Occurrences:1
[other]())	Occurrences:1
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
Parray(Id = BTy_0 886, 1b, 0b)([other](.) )
First:
	Punion(Id = BTy_26 886, 1b, 0b)
		Pstruct(Id = BTy_25 885, 1b, 0b)
			Punion(Id = BTy_2 885, 1b, 0b)
				[empty](Id = BTy_1 778, 1b, 0b);
				Pstruct(Id = BTy_7 106, 1b, 0b)
					[Host](Id = BTy_3 106, 1b, 0b);
					Punion(Id = BTy_5 106, 1b, 0b)
						[empty](Id = BTy_4 104, 1b, 0b);
						[other](-)(Id = BTy_6 2, 1b, 0b);
					End Punion;
				End Pstruct;
				[int](Id = BTy_9 1, 1b, 0b);
			End Punion;
			[string](Id = BTy_10 885, 1b, 0b);
			Punion(Id = BTy_12 885, 1b, 0b)
				[empty](Id = BTy_11 876, 1b, 0b);
				Parray(Id = BTy_13 9, 1b, 0b)([other](+) )
				First:
					[other](+)(Id = BTy_14 9, 1b, 0b);
				Body:
					[other](+)(Id = BTy_15 7, 1b, 0b);
				Tail:
					Punion(Id = BTy_23 9, 1b, 0b)
						Parray(Id = BTy_16 8, 1b, 0b)([int] )
						First:
							[int](Id = BTy_17 8, 1b, 0b);
						Body:
							[int](Id = BTy_18 2, 1b, 0b);
						Tail:
							[empty](Id = BTy_19 8, 1b, 0b);
						End Parray;
						Pstruct(Id = BTy_22 1, 1b, 0b)
							[other](-)(Id = BTy_20 1, 1b, 0b);
							[string](Id = BTy_21 1, 1b, 0b);
						End Pstruct;
					End Punion;
				End Parray;
			End Punion;
			[other](.)(Id = BTy_24 885, 1b, 0b);
		End Pstruct;
		Pstruct(Id = BTy_30 1, 1b, 0b)
			[Host](Id = BTy_27 1, 1b, 0b);
			[int](Id = BTy_28 1, 1b, 0b);
			[other](.)(Id = BTy_29 1, 1b, 0b);
		End Pstruct;
	End Punion;
Body:
	Pstruct(Id = BTy_60 2729, 1b, 0b)
		Punion(Id = BTy_43 2729, 1b, 0b)
			Pstruct(Id = BTy_42 1083, 1b, 0b)
				Punion(Id = BTy_32 1083, 1b, 0b)
					[empty](Id = BTy_31 1029, 1b, 0b);
					Parray(Id = BTy_33 54, 1b, 0b)([int] )
					First:
						[int](Id = BTy_34 54, 1b, 0b);
					Body:
						[int](Id = BTy_35 9, 1b, 0b);
					Tail:
						Punion(Id = BTy_37 54, 1b, 0b)
							[empty](Id = BTy_36 42, 1b, 0b);
							[other](_)(Id = BTy_38 9, 1b, 0b);
							[other](-)(Id = BTy_40 3, 1b, 0b);
						End Punion;
					End Parray;
				End Punion;
				[string](Id = BTy_41 1083, 1b, 0b);
			End Pstruct;
			Parray(Id = BTy_44 1646, 1b, 0b)([int] )
			First:
				Punion(Id = BTy_50 1646, 1b, 0b)
					Pstruct(Id = BTy_49 1645, 1b, 0b)
						Punion(Id = BTy_46 1645, 1b, 0b)
							[empty](Id = BTy_45 1643, 1b, 0b);
							[IP](Id = BTy_47 2, 1b, 0b);
						End Punion;
						[int](Id = BTy_48 1645, 1b, 0b);
					End Pstruct;
					[empty](Id = BTy_51 1, 1b, 0b);
				End Punion;
			Body:
				[int](Id = BTy_52 729, 1b, 0b);
			Tail:
				Punion(Id = BTy_54 1646, 1b, 0b)
					[empty](Id = BTy_53 1645, 1b, 0b);
					Pstruct(Id = BTy_58 1, 1b, 0b)
						[other](()(Id = BTy_55 1, 1b, 0b);
						[string](Id = BTy_56 1, 1b, 0b);
						[other]())(Id = BTy_57 1, 1b, 0b);
					End Pstruct;
				End Punion;
			End Parray;
		End Punion;
		[other](.)(Id = BTy_59 2729, 1b, 0b);
	End Pstruct;
Tail:
	[string](Id = BTy_61 886, 1b, 0b);
End Parray


After final reduction:
Pstruct(Id = BTy_67 886, 1b, 0b)
	Punion(Id = BTy_26 886, 1b, 0b)
		Pstruct(Id = BTy_25 885, 1b, 0b)
			Switch(BTy_10)(Id = BTy_2 885, 1b, 0b):
			case [StringConst] "*":
				[StringConst] ""(Id = BTy_1 778, 1b, 0b);
			case [Enum] {[StringConst] "arch", [StringConst] "e16", [StringConst] "e5", [StringConst] "i18n-1", [StringConst] "libs-1", }:
				Pstruct(Id = BTy_7 106, 1b, 0b)
					[Host](Id = BTy_3 106, 1b, 0b);
					Punion(Id = BTy_5 106, 1b, 0b)
						[StringConst] ""(Id = BTy_4 104, 1b, 0b);
						[StringConst] "-"(Id = BTy_6 2, 1b, 0b);
					End Punion;
				End Pstruct;
			case [StringConst] "Suite-1":
				[IntConst] [4](Id = BTy_9 1, 1b, 0b);
			End Switch;
			[string](Id = BTy_10 885, 1b, 0b);
			Switch(BTy_10)(Id = BTy_12 885, 1b, 0b):
			case [StringConst] "*":
				[StringConst] ""(Id = BTy_11 876, 1b, 0b);
			case [Enum] {[StringConst] "compat-gcc-32-c", [StringConst] "compat-libstdc", [StringConst] "gcc-c", [StringConst] "gcc4-c", [StringConst] "gtk", [StringConst] "libstdc", [StringConst] "memtest86", }:
				Pstruct(Id = BTy_63 9, 1b, 0b)
					[StringConst] "+"(Id = BTy_14 9, 1b, 0b);
					RArray(Id = BTy_13 9, 1b, 0b)
						[StringConst] "+"(Id = BTy_15 7, 1b, 0b);
					End RArray;
					Punion(Id = BTy_23 9, 1b, 0b)
						Pstruct(Id = BTy_62 8, 1b, 0b)
							[Int] [~296...~1](Id = BTy_17 8, 1b, 0b);
							RArray(Id = BTy_16 8, 1b, 0b)
								[Int] [~3...~2](Id = BTy_18 2, 1b, 0b);
							End RArray;
						End Pstruct;
						Pstruct(Id = BTy_22 1, 1b, 0b)
							[StringConst] "-devel-3"(Id = BTy_20 1, 1b, 0b);
						End Pstruct;
					End Punion;
				End Pstruct;
			End Switch;
			[StringConst] "."(Id = BTy_24 885, 1b, 0b);
		End Pstruct;
		Pstruct(Id = BTy_30 1, 1b, 0b)
			[StringConst] "openoffice.org"(Id = BTy_27 1, 1b, 0b);
			[IntConst] [~1](Id = BTy_28 1, 1b, 0b);
			[StringConst] "."(Id = BTy_29 1, 1b, 0b);
		End Pstruct;
	End Punion;
	RArray(Id = BTy_0 886, 1b, 0b)
		Separator: [StringConst] "."
		Punion(Id = BTy_43 3615, 1b, 0b)
			Pstruct(Id = BTy_42 1083, 1b, 0b)
				Punion(Id = BTy_32 1083, 1b, 0b)
					[StringConst] ""(Id = BTy_31 1029, 1b, 0b);
					Pstruct(Id = BTy_65 54, 1b, 0b)
						[Int] [0...875](Id = BTy_34 54, 1b, 0b);
						RArray(Id = BTy_33 54, 1b, 0b)
							[Int] [~80...~14](Id = BTy_35 9, 1b, 0b);
						End RArray;
						Punion(Id = BTy_37 54, 1b, 0b)
							[StringConst] ""(Id = BTy_36 42, 1b, 0b);
							[StringConst] "_"(Id = BTy_38 9, 1b, 0b);
							[StringConst] "-"(Id = BTy_40 3, 1b, 0b);
						End Punion;
					End Pstruct;
				End Punion;
				[string](Id = BTy_41 1969, 1b, 0b);
			End Pstruct;
			Pstruct(Id = BTy_66 1646, 1b, 0b)
				Punion(Id = BTy_50 1646, 1b, 0b)
					Pstruct(Id = BTy_49 1645, 1b, 0b)
						Switch(BTy_48)(Id = BTy_46 1645, 1b, 0b):
						case [StringConst] "*":
							[StringConst] ""(Id = BTy_45 1643, 1b, 0b);
						case [IntConst] [~21]:
							[Enum] {[StringConst] "15.92.0.2", [StringConst] "4.1.2.90", }(Id = BTy_47 2, 1b, 0b);
						End Switch;
						[Int] [~44...20060823](Id = BTy_48 1645, 1b, 0b);
					End Pstruct;
					[StringConst] ""(Id = BTy_51 1, 1b, 0b);
				End Punion;
				RArray(Id = BTy_44 1646, 1b, 0b)
					[Int] [~187...0](Id = BTy_52 729, 1b, 0b);
				End RArray;
				Punion(Id = BTy_54 1646, 1b, 0b)
					[StringConst] ""(Id = BTy_53 1645, 1b, 0b);
					Pstruct(Id = BTy_58 1, 1b, 0b)
						[StringConst] "(none)"(Id = BTy_55 1, 1b, 0b);
					End Pstruct;
				End Punion;
			End Pstruct;
		End Punion;
	End RArray;
End Pstruct

Complexity of inferred type:
	numAlt = 8  numTBD = 0  numBtm = 0
Outputing partitions to directory: gen/

Completed data/rpmpkgs.txt
Overall type complexity = 46.585b
Overall data complexity = 5598.053b

