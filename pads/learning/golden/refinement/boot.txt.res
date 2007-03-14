Source file to process: data/boot.txt
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
Starting on file data/boot.txt
262 records.
in flatten function
in flatten function
Histogram of number of tokens per record:
	9:	6
	10:	8
	11:	2
	12:	2
	13:	8
	14:	90
	15:	34
	16:	18
	17:	6
	18:	16
	19:	18
	20:	14
	21:	4
	22:	2
	23:	2
	24:	2
	25:	4
	27:	8
	28:	4
	29:	6
	31:	2
	33:	2
	41:	4

Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [Time]
Total number of token occurrences: 272.
Number of records with at least one token occurrence: 262.
StructScore: 10.
	1:	252	0.961832061069
	2:	10	0.0381679389313

Token: [Date]
Total number of token occurrences: 272.
Number of records with at least one token occurrence: 262.
StructScore: 10.
	1:	252	0.961832061069
	2:	10	0.0381679389313


Cluster 1:
Token: [other](:)
Total number of token occurrences: 356.
Number of records with at least one token occurrence: 261.
StructScore: 105.
	1:	184	0.702290076336
	2:	71	0.270992366412
	3:	2	0.00763358778626
	6:	4	0.0152671755725


Cluster 2:
Token: [other](=)
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 28.
StructScore: 234.
	1:	28	0.106870229008


Cluster 3:
Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 256.
	1:	6	0.0229007633588

Token: [other](()[Group Body][other]())
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 258.
	1:	4	0.0152671755725

Token: [other](?)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 258.
	1:	4	0.0152671755725

Token: [URL]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](\^[)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	2:	2	0.00763358778626

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](>)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	3:	2	0.00763358778626

Token: [other]([)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626


Cluster 4:
Token: [Host]
Total number of token occurrences: 51.
Number of records with at least one token occurrence: 49.
StructScore: 641.
	1:	47	0.179389312977
	2:	2	0.00763358778626


Cluster 5:
Token: [other](.)
Total number of token occurrences: 40.
Number of records with at least one token occurrence: 34.
StructScore: 690.
	1:	28	0.106870229008
	2:	6	0.0229007633588


Cluster 6:
Token: [Path]
Total number of token occurrences: 32.
Number of records with at least one token occurrence: 22.
StructScore: 730.
	1:	12	0.0458015267176
	2:	10	0.0381679389313


Cluster 7:
Token: [other](,)
Total number of token occurrences: 20.
Number of records with at least one token occurrence: 16.
StructScore: 742.
	1:	12	0.0458015267176
	2:	4	0.0152671755725


Cluster 8:
Token: [other](-)
Total number of token occurrences: 19.
Number of records with at least one token occurrence: 15.
StructScore: 745.
	1:	11	0.0419847328244
	2:	4	0.0152671755725


Cluster 9:
Token: [int]
Total number of token occurrences: 69.
Number of records with at least one token occurrence: 55.
StructScore: 1254.
	1:	47	0.179389312977
	2:	2	0.00763358778626
	3:	6	0.0229007633588


Cluster 10:
Token: [string]
Total number of token occurrences: 1414.
Number of records with at least one token occurrence: 262.
StructScore: 1316.
	2:	8	0.030534351145
	3:	32	0.12213740458
	4:	18	0.0687022900763
	5:	126	0.480916030534
	6:	28	0.106870229008
	7:	20	0.0763358778626
	8:	4	0.0152671755725
	9:	16	0.0610687022901
	10:	6	0.0229007633588
	14:	4	0.0152671755725


Cluster 11:
Token: [white]
Total number of token occurrences: 1863.
Number of records with at least one token occurrence: 262.
StructScore: 1689.
	4:	16	0.0610687022901
	5:	14	0.0534351145038
	6:	125	0.477099236641
	7:	31	0.118320610687
	8:	24	0.0916030534351
	9:	22	0.0839694656489
	10:	4	0.0152671755725
	11:	6	0.0229007633588
	12:	8	0.030534351145
	13:	6	0.0229007633588
	14:	2	0.00763358778626
	16:	4	0.0152671755725


Junk Tolerance Threshold: 27
Coverage: 252
Num Tokens: 2
Struct
Coverage:252
Token count:2
[Time]	Occurrences:1
[Date]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 260.
Number of records with at least one token occurrence: 260.
StructScore: 2.
	1:	260	0.992366412214


Cluster 1:
Token: [int]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [string]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other]([)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626


Junk Tolerance Threshold: 27
Coverage: 260
Num Tokens: 1
Struct
Coverage:260
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [string]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [other]([)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 3
Struct
Coverage:2
Token count:3
[int]	Occurrences:1
[string]	Occurrences:1
[other]([)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [white]
Total number of token occurrences: 262.
Number of records with at least one token occurrence: 262.
StructScore: 0.
	1:	262	1.0


Junk Tolerance Threshold: 27
Coverage: 262
Num Tokens: 1
Struct
Coverage:262
Token count:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [other](:)
Total number of token occurrences: 356.
Number of records with at least one token occurrence: 261.
StructScore: 105.
	1:	184	0.702290076336
	2:	71	0.270992366412
	3:	2	0.00763358778626
	6:	4	0.0152671755725


Cluster 1:
Token: [other](=)
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 28.
StructScore: 234.
	1:	28	0.106870229008


Cluster 2:
Token: [Time]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 252.
	1:	10	0.0381679389313

Token: [Date]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 252.
	1:	10	0.0381679389313


Cluster 3:
Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 256.
	1:	6	0.0229007633588

Token: [other](()[Group Body][other]())
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 258.
	1:	4	0.0152671755725

Token: [other](?)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 258.
	1:	4	0.0152671755725

Token: [URL]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](\^[)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	2:	2	0.00763358778626

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](>)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	3:	2	0.00763358778626


Cluster 4:
Token: [Host]
Total number of token occurrences: 51.
Number of records with at least one token occurrence: 49.
StructScore: 641.
	1:	47	0.179389312977
	2:	2	0.00763358778626


Cluster 5:
Token: [other](.)
Total number of token occurrences: 40.
Number of records with at least one token occurrence: 34.
StructScore: 690.
	1:	28	0.106870229008
	2:	6	0.0229007633588


Cluster 6:
Token: [Path]
Total number of token occurrences: 32.
Number of records with at least one token occurrence: 22.
StructScore: 730.
	1:	12	0.0458015267176
	2:	10	0.0381679389313


Cluster 7:
Token: [other](,)
Total number of token occurrences: 20.
Number of records with at least one token occurrence: 16.
StructScore: 742.
	1:	12	0.0458015267176
	2:	4	0.0152671755725


Cluster 8:
Token: [other](-)
Total number of token occurrences: 19.
Number of records with at least one token occurrence: 15.
StructScore: 745.
	1:	11	0.0419847328244
	2:	4	0.0152671755725


Cluster 9:
Token: [int]
Total number of token occurrences: 67.
Number of records with at least one token occurrence: 53.
StructScore: 1266.
	1:	45	0.171755725191
	2:	2	0.00763358778626
	3:	6	0.0229007633588


Cluster 10:
Token: [string]
Total number of token occurrences: 1412.
Number of records with at least one token occurrence: 262.
StructScore: 1356.
	2:	10	0.0381679389313
	3:	30	0.114503816794
	4:	18	0.0687022900763
	5:	126	0.480916030534
	6:	28	0.106870229008
	7:	20	0.0763358778626
	8:	4	0.0152671755725
	9:	16	0.0610687022901
	10:	6	0.0229007633588
	14:	4	0.0152671755725


Cluster 11:
Token: [white]
Total number of token occurrences: 1601.
Number of records with at least one token occurrence: 262.
StructScore: 1689.
	3:	16	0.0610687022901
	4:	14	0.0534351145038
	5:	125	0.477099236641
	6:	31	0.118320610687
	7:	24	0.0916030534351
	8:	22	0.0839694656489
	9:	4	0.0152671755725
	10:	6	0.0229007633588
	11:	8	0.030534351145
	12:	6	0.0229007633588
	13:	2	0.00763358778626
	15:	4	0.0152671755725


Junk Tolerance Threshold: 27
Coverage: 262
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [other](:)
Total number of token occurrences: 356.
Number of records with at least one token occurrence: 261.
StructScore: 105.
	1:	184	0.702290076336
	2:	71	0.270992366412
	3:	2	0.00763358778626
	6:	4	0.0152671755725


Cluster 1:
Token: [white]
Total number of token occurrences: 1601.
Number of records with at least one token occurrence: 262.
StructScore: 1689.
	3:	16	0.0610687022901
	4:	14	0.0534351145038
	5:	125	0.477099236641
	6:	31	0.118320610687
	7:	24	0.0916030534351
	8:	22	0.0839694656489
	9:	4	0.0152671755725
	10:	6	0.0229007633588
	11:	8	0.030534351145
	12:	6	0.0229007633588
	13:	2	0.00763358778626
	15:	4	0.0152671755725


Cluster 2:
Token: [string]
Total number of token occurrences: 1412.
Number of records with at least one token occurrence: 262.
StructScore: 1356.
	2:	10	0.0381679389313
	3:	30	0.114503816794
	4:	18	0.0687022900763
	5:	126	0.480916030534
	6:	28	0.106870229008
	7:	20	0.0763358778626
	8:	4	0.0152671755725
	9:	16	0.0610687022901
	10:	6	0.0229007633588
	14:	4	0.0152671755725


Cluster 3:
Token: [int]
Total number of token occurrences: 67.
Number of records with at least one token occurrence: 53.
StructScore: 1266.
	1:	45	0.171755725191
	2:	2	0.00763358778626
	3:	6	0.0229007633588


Cluster 4:
Token: [Host]
Total number of token occurrences: 51.
Number of records with at least one token occurrence: 49.
StructScore: 641.
	1:	47	0.179389312977
	2:	2	0.00763358778626


Cluster 5:
Token: [other](.)
Total number of token occurrences: 40.
Number of records with at least one token occurrence: 34.
StructScore: 690.
	1:	28	0.106870229008
	2:	6	0.0229007633588


Cluster 6:
Token: [other](=)
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 28.
StructScore: 234.
	1:	28	0.106870229008


Cluster 7:
Token: [Path]
Total number of token occurrences: 32.
Number of records with at least one token occurrence: 22.
StructScore: 730.
	1:	12	0.0458015267176
	2:	10	0.0381679389313


Cluster 8:
Token: [other](-)
Total number of token occurrences: 19.
Number of records with at least one token occurrence: 15.
StructScore: 745.
	1:	11	0.0419847328244
	2:	4	0.0152671755725


Cluster 9:
Token: [other](,)
Total number of token occurrences: 20.
Number of records with at least one token occurrence: 16.
StructScore: 742.
	1:	12	0.0458015267176
	2:	4	0.0152671755725


Cluster 10:
Token: [Time]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 252.
	1:	10	0.0381679389313

Token: [Date]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 252.
	1:	10	0.0381679389313


Cluster 11:
Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 256.
	1:	6	0.0229007633588

Token: [other](()[Group Body][other]())
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 258.
	1:	4	0.0152671755725

Token: [other](?)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 258.
	1:	4	0.0152671755725

Token: [URL]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](\^[)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	2:	2	0.00763358778626

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](>)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	3:	2	0.00763358778626


Possible array tokens:
[other](:)
Records in possible array context:262
Total:356
Coverage:261
Width:4
Array	[other](:)	Occurrences:1
WARNING: ARRAY first context empty!Array context
Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 522.
Number of records with at least one token occurrence: 261.
StructScore: 1.
	2:	261	0.996183206107

Token: [white]
Total number of token occurrences: 522.
Number of records with at least one token occurrence: 261.
StructScore: 1.
	2:	261	0.996183206107

Token: [other](:)
Total number of token occurrences: 261.
Number of records with at least one token occurrence: 261.
StructScore: 1.
	1:	261	0.996183206107


Cluster 1:
Token: [Host]
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 28.
StructScore: 234.
	1:	28	0.106870229008


Cluster 2:
Token: [empty]
Total number of token occurrences: 1.
Number of records with at least one token occurrence: 1.
StructScore: 261.
	1:	1	0.00381679389313


Junk Tolerance Threshold: 27
Coverage: 261
Num Tokens: 5
Struct
Coverage:261
Token count:5
[string]	Occurrences:2
[white]	Occurrences:2
[other](:)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 233.
Number of records with at least one token occurrence: 233.
StructScore: 28.
	1:	233	0.892720306513


Cluster 1:
Token: [Host]
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 28.
StructScore: 233.
	1:	28	0.107279693487


Junk Tolerance Threshold: 27
Coverage: 233
Num Tokens: 1
Struct
Coverage:233
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Host]
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 28.
StructScore: 0.
	1:	28	1.0


Junk Tolerance Threshold: 3
Coverage: 28
Num Tokens: 1
Struct
Coverage:28
Token count:1
[Host]	Occurrences:1
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
Token: [other](:)
Total number of token occurrences: 95.
Number of records with at least one token occurrence: 95.
StructScore: 0.
	1:	95	1.0


Cluster 1:
Token: [int]
Total number of token occurrences: 14.
Number of records with at least one token occurrence: 14.
StructScore: 81.
	1:	14	0.147368421053


Cluster 2:
Token: [Path]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 85.
	1:	10	0.105263157895


Cluster 3:
Token: [Time]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 91.
	1:	4	0.0421052631579

Token: [Date]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 91.
	1:	4	0.0421052631579


Cluster 4:
Token: [Host]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 93.
	1:	2	0.0210526315789

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 93.
	1:	2	0.0210526315789


Cluster 5:
Token: [string]
Total number of token occurrences: 216.
Number of records with at least one token occurrence: 89.
StructScore: 340.
	1:	26	0.273684210526
	2:	27	0.284210526316
	3:	14	0.147368421053
	4:	18	0.189473684211
	5:	2	0.0210526315789
	6:	2	0.0210526315789


Cluster 6:
Token: [white]
Total number of token occurrences: 224.
Number of records with at least one token occurrence: 79.
StructScore: 582.
	1:	14	0.147368421053
	2:	21	0.221052631579
	3:	18	0.189473684211
	4:	18	0.189473684211
	5:	6	0.0631578947368
	6:	2	0.0210526315789


Junk Tolerance Threshold: 10
Coverage: 95
Num Tokens: 1
Struct
Coverage:95
Token count:1
[other](:)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 14.
Number of records with at least one token occurrence: 14.
StructScore: 81.
	1:	14	0.147368421053


Cluster 1:
Token: [Path]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 85.
	1:	10	0.105263157895


Cluster 2:
Token: [Time]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 91.
	1:	4	0.0421052631579

Token: [Date]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 91.
	1:	4	0.0421052631579


Cluster 3:
Token: [Host]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 93.
	1:	2	0.0210526315789

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 93.
	1:	2	0.0210526315789


Cluster 4:
Token: [string]
Total number of token occurrences: 216.
Number of records with at least one token occurrence: 89.
StructScore: 340.
	1:	26	0.273684210526
	2:	27	0.284210526316
	3:	14	0.147368421053
	4:	18	0.189473684211
	5:	2	0.0210526315789
	6:	2	0.0210526315789


Cluster 5:
Token: [white]
Total number of token occurrences: 224.
Number of records with at least one token occurrence: 79.
StructScore: 582.
	1:	14	0.147368421053
	2:	21	0.221052631579
	3:	18	0.189473684211
	4:	18	0.189473684211
	5:	6	0.0631578947368
	6:	2	0.0210526315789


Junk Tolerance Threshold: 10
Coverage: 14
Num Tokens: 1
Struct
Coverage:14
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 8.
StructScore: 6.
	1:	8	0.571428571429


Cluster 1:
Token: [string]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 8.
	1:	6	0.428571428571


Cluster 2:
Token: [Time]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 10.
	1:	4	0.285714285714

Token: [Date]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 10.
	1:	4	0.285714285714


Cluster 3:
Token: [white]
Total number of token occurrences: 20.
Number of records with at least one token occurrence: 6.
StructScore: 26.
	2:	2	0.142857142857
	4:	4	0.285714285714


Junk Tolerance Threshold: 2
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
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 0.
	1:	6	1.0


Cluster 1:
Token: [Time]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2.
	1:	4	0.666666666667

Token: [Date]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2.
	1:	4	0.666666666667

Token: [white]
Total number of token occurrences: 20.
Number of records with at least one token occurrence: 6.
StructScore: 2.
	2:	2	0.333333333333
	4:	4	0.666666666667


Junk Tolerance Threshold: 1
Coverage: 6
Num Tokens: 1
Struct
Coverage:6
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 0.
	1:	6	1.0


Junk Tolerance Threshold: 1
Coverage: 6
Num Tokens: 1
Struct
Coverage:6
Token count:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Time]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2.
	1:	4	0.666666666667

Token: [Date]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 2.
	1:	4	0.666666666667

Token: [white]
Total number of token occurrences: 14.
Number of records with at least one token occurrence: 6.
StructScore: 2.
	1:	2	0.333333333333
	3:	4	0.666666666667


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 2
Struct
Coverage:4
Token count:2
[Time]	Occurrences:1
[Date]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
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
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
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
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
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
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
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
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 12.
StructScore: 2.
	1:	12	0.857142857143


Cluster 1:
Token: [white]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 10.
	1:	4	0.285714285714


Cluster 2:
Token: [empty]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 12.
	1:	2	0.142857142857


Junk Tolerance Threshold: 2
Coverage: 12
Num Tokens: 1
Struct
Coverage:12
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 8.
StructScore: 4.
	1:	8	0.666666666667


Cluster 1:
Token: [white]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 8.
	1:	4	0.333333333333


Junk Tolerance Threshold: 2
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
Token: [white]
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
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
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
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Path]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 71.
	1:	10	0.123456790123


Cluster 1:
Token: [Host]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 79.
	1:	2	0.0246913580247

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 79.
	1:	2	0.0246913580247


Cluster 2:
Token: [string]
Total number of token occurrences: 198.
Number of records with at least one token occurrence: 75.
StructScore: 326.
	1:	16	0.197530864198
	2:	23	0.283950617284
	3:	14	0.172839506173
	4:	18	0.222222222222
	5:	2	0.0246913580247
	6:	2	0.0246913580247


Cluster 3:
Token: [white]
Total number of token occurrences: 200.
Number of records with at least one token occurrence: 73.
StructScore: 374.
	1:	14	0.172839506173
	2:	19	0.234567901235
	3:	18	0.222222222222
	4:	18	0.222222222222
	5:	2	0.0246913580247
	6:	2	0.0246913580247


Junk Tolerance Threshold: 9
Coverage: 10
Num Tokens: 1
Struct
Coverage:10
Token count:1
[Path]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
Total number of token occurrences: 18.
Number of records with at least one token occurrence: 10.
StructScore: 4.
	1:	6	0.6
	3:	4	0.4


Cluster 1:
Token: [string]
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 4.
StructScore: 6.
	3:	4	0.4


Junk Tolerance Threshold: 1
Coverage: 10
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [white]
Total number of token occurrences: 18.
Number of records with at least one token occurrence: 10.
StructScore: 4.
	1:	6	0.6
	3:	4	0.4


Cluster 1:
Token: [string]
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 4.
StructScore: 6.
	3:	4	0.4


Possible array tokens:
[white]
Records in possible array context:10
Total:18
Coverage:10
Width:2
Array	[white]	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 0.
	1:	10	1.0


Junk Tolerance Threshold: 1
Coverage: 10
Num Tokens: 1
Struct
Coverage:10
Token count:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 8.
StructScore: 0.
	1:	8	1.0

Token: [white]
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 8.
StructScore: 0.
	1:	8	1.0


Junk Tolerance Threshold: 1
Coverage: 8
Num Tokens: 2
Struct
Coverage:8
Token count:2
[string]	Occurrences:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 4.
	1:	6	0.6


Cluster 1:
Token: [string]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 6.
	1:	4	0.4


Junk Tolerance Threshold: 1
Coverage: 6
Num Tokens: 1
Struct
Coverage:6
Token count:1
[empty]	Occurrences:1
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
Token: [Host]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 69.
	1:	2	0.0281690140845

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 69.
	1:	2	0.0281690140845


Cluster 1:
Token: [string]
Total number of token occurrences: 186.
Number of records with at least one token occurrence: 71.
StructScore: 176.
	1:	16	0.225352112676
	2:	23	0.323943661972
	3:	10	0.140845070423
	4:	18	0.253521126761
	5:	2	0.0281690140845
	6:	2	0.0281690140845


Cluster 2:
Token: [white]
Total number of token occurrences: 182.
Number of records with at least one token occurrence: 63.
StructScore: 326.
	1:	8	0.112676056338
	2:	19	0.267605633803
	3:	14	0.197183098592
	4:	18	0.253521126761
	5:	2	0.0281690140845
	6:	2	0.0281690140845


Junk Tolerance Threshold: 8
Coverage: 2
Num Tokens: 2
Clusters sorted by array criteria:
Cluster 0:
Token: [string]
Total number of token occurrences: 186.
Number of records with at least one token occurrence: 71.
StructScore: 176.
	1:	16	0.225352112676
	2:	23	0.323943661972
	3:	10	0.140845070423
	4:	18	0.253521126761
	5:	2	0.0281690140845
	6:	2	0.0281690140845


Cluster 1:
Token: [white]
Total number of token occurrences: 182.
Number of records with at least one token occurrence: 63.
StructScore: 326.
	1:	8	0.112676056338
	2:	19	0.267605633803
	3:	14	0.197183098592
	4:	18	0.253521126761
	5:	2	0.0281690140845
	6:	2	0.0281690140845


Cluster 2:
Token: [Host]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 69.
	1:	2	0.0281690140845

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 69.
	1:	2	0.0281690140845


Possible array tokens:
[string]
Records in possible array context:71
Total:186
Coverage:71
Width:6
ARRAY NOT CHOSEN
Union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
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
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Host]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 61.
	1:	2	0.031746031746

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 61.
	1:	2	0.031746031746


Cluster 1:
Token: [string]
Total number of token occurrences: 178.
Number of records with at least one token occurrence: 63.
StructScore: 146.
	1:	8	0.126984126984
	2:	23	0.365079365079
	3:	10	0.15873015873
	4:	18	0.285714285714
	5:	2	0.031746031746
	6:	2	0.031746031746


Cluster 2:
Token: [white]
Total number of token occurrences: 182.
Number of records with at least one token occurrence: 63.
StructScore: 158.
	1:	8	0.126984126984
	2:	19	0.301587301587
	3:	14	0.222222222222
	4:	18	0.285714285714
	5:	2	0.031746031746
	6:	2	0.031746031746


Junk Tolerance Threshold: 7
Coverage: 2
Num Tokens: 2
Clusters sorted by array criteria:
Cluster 0:
Token: [white]
Total number of token occurrences: 182.
Number of records with at least one token occurrence: 63.
StructScore: 158.
	1:	8	0.126984126984
	2:	19	0.301587301587
	3:	14	0.222222222222
	4:	18	0.285714285714
	5:	2	0.031746031746
	6:	2	0.031746031746


Cluster 1:
Token: [string]
Total number of token occurrences: 178.
Number of records with at least one token occurrence: 63.
StructScore: 146.
	1:	8	0.126984126984
	2:	23	0.365079365079
	3:	10	0.15873015873
	4:	18	0.285714285714
	5:	2	0.031746031746
	6:	2	0.031746031746


Cluster 2:
Token: [Host]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 61.
	1:	2	0.031746031746

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 61.
	1:	2	0.031746031746


Possible array tokens:
[white]
Records in possible array context:63
Total:182
Coverage:63
Width:6
Array	[white]	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
Total number of token occurrences: 63.
Number of records with at least one token occurrence: 63.
StructScore: 0.
	1:	63	1.0


Junk Tolerance Threshold: 7
Coverage: 63
Num Tokens: 1
Struct
Coverage:63
Token count:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 2.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 119.
Number of records with at least one token occurrence: 119.
StructScore: 0.
	1:	119	1.0

Token: [white]
Total number of token occurrences: 119.
Number of records with at least one token occurrence: 119.
StructScore: 0.
	1:	119	1.0


Junk Tolerance Threshold: 12
Coverage: 119
Num Tokens: 2
Struct
Coverage:119
Token count:2
[string]	Occurrences:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 59.
Number of records with at least one token occurrence: 59.
StructScore: 4.
	1:	59	0.936507936508


Cluster 1:
Token: [Host]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 61.
	1:	2	0.031746031746

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 61.
	1:	2	0.031746031746


Junk Tolerance Threshold: 7
Coverage: 59
Num Tokens: 1
Struct
Coverage:59
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Host]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 2.
	1:	2	0.5

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 2.
	1:	2	0.5


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 2
Struct
Coverage:2
Token count:2
[Host]	Occurrences:1
[other](()[Group Body][other]())	Occurrences:1
converting false struct into union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Host]
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
[Host]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](()[Group Body][other]())
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
[other](()[Group Body][other]())	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [other](()
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 3
Struct
Coverage:2
Token count:3
[string]	Occurrences:1
[other](()	Occurrences:1
[other]())	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [other](=)
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 28.
StructScore: 234.
	1:	28	0.106870229008


Cluster 1:
Token: [Path]
Total number of token occurrences: 22.
Number of records with at least one token occurrence: 22.
StructScore: 240.
	1:	22	0.0839694656489

Token: [Host]
Total number of token occurrences: 21.
Number of records with at least one token occurrence: 21.
StructScore: 241.
	1:	21	0.0801526717557


Cluster 2:
Token: [Time]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 256.
	1:	6	0.0229007633588

Token: [Date]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 256.
	1:	6	0.0229007633588

Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 256.
	1:	6	0.0229007633588

Token: [other](?)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 258.
	1:	4	0.0152671755725

Token: [URL]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](\^[)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	2:	2	0.00763358778626

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	1:	2	0.00763358778626

Token: [other](>)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 2.
StructScore: 260.
	3:	2	0.00763358778626


Cluster 3:
Token: [int]
Total number of token occurrences: 53.
Number of records with at least one token occurrence: 49.
StructScore: 643.
	1:	45	0.171755725191
	2:	4	0.0152671755725


Cluster 4:
Token: [other](.)
Total number of token occurrences: 40.
Number of records with at least one token occurrence: 34.
StructScore: 690.
	1:	28	0.106870229008
	2:	6	0.0229007633588


Cluster 5:
Token: [other](,)
Total number of token occurrences: 20.
Number of records with at least one token occurrence: 16.
StructScore: 742.
	1:	12	0.0458015267176
	2:	4	0.0152671755725


Cluster 6:
Token: [other](-)
Total number of token occurrences: 19.
Number of records with at least one token occurrence: 15.
StructScore: 745.
	1:	11	0.0419847328244
	2:	4	0.0152671755725


Cluster 7:
Token: [white]
Total number of token occurrences: 855.
Number of records with at least one token occurrence: 258.
StructScore: 1492.
	1:	39	0.148854961832
	2:	48	0.18320610687
	3:	102	0.389312977099
	4:	24	0.0916030534351
	5:	16	0.0610687022901
	6:	9	0.0343511450382
	7:	4	0.0152671755725
	8:	2	0.00763358778626
	9:	8	0.030534351145
	10:	2	0.00763358778626
	12:	4	0.0152671755725


Cluster 8:
Token: [string]
Total number of token occurrences: 674.
Number of records with at least one token occurrence: 242.
StructScore: 1578.
	1:	81	0.309160305344
	2:	10	0.0381679389313
	3:	112	0.427480916031
	4:	14	0.0534351145038
	5:	5	0.0190839694656
	6:	4	0.0152671755725
	7:	8	0.030534351145
	8:	4	0.0152671755725
	11:	4	0.0152671755725


Junk Tolerance Threshold: 27
Coverage: 28
Num Tokens: 1
Struct
Coverage:28
Token count:1
[other](=)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Host]
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 16.
StructScore: 12.
	1:	16	0.571428571429


Cluster 1:
Token: [white]
Total number of token occurrences: 92.
Number of records with at least one token occurrence: 28.
StructScore: 16.
	1:	4	0.142857142857
	2:	20	0.714285714286
	12:	4	0.142857142857


Cluster 2:
Token: [other](,)
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 4.
StructScore: 24.
	2:	4	0.142857142857

Token: [other](-)
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 4.
StructScore: 24.
	2:	4	0.142857142857


Cluster 3:
Token: [string]
Total number of token occurrences: 80.
Number of records with at least one token occurrence: 28.
StructScore: 40.
	1:	16	0.571428571429
	2:	4	0.142857142857
	3:	4	0.142857142857
	11:	4	0.142857142857


Cluster 4:
Token: [other](.)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 12.
StructScore: 52.
	1:	8	0.285714285714
	2:	4	0.142857142857


Junk Tolerance Threshold: 3
Coverage: 16
Num Tokens: 1
Struct
Coverage:16
Token count:1
[Host]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 16.
StructScore: 0.
	1:	16	1.0


Junk Tolerance Threshold: 2
Coverage: 16
Num Tokens: 1
Struct
Coverage:16
Token count:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 16.
StructScore: 0.
	1:	16	1.0


Cluster 1:
Token: [string]
Total number of token occurrences: 20.
Number of records with at least one token occurrence: 16.
StructScore: 4.
	1:	12	0.75
	2:	4	0.25


Cluster 2:
Token: [other](.)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 12.
	1:	4	0.25


Junk Tolerance Threshold: 2
Coverage: 16
Num Tokens: 1
Struct
Coverage:16
Token count:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 20.
Number of records with at least one token occurrence: 16.
StructScore: 4.
	1:	12	0.75
	2:	4	0.25


Cluster 1:
Token: [other](.)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 12.
	1:	4	0.25


Junk Tolerance Threshold: 2
Coverage: 16
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [string]
Total number of token occurrences: 20.
Number of records with at least one token occurrence: 16.
StructScore: 4.
	1:	12	0.75
	2:	4	0.25


Cluster 1:
Token: [other](.)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 12.
	1:	4	0.25


Possible array tokens:
[string]
Records in possible array context:16
Total:20
Coverage:16
Width:2
ARRAY NOT CHOSEN
Union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 16.
StructScore: 0.
	1:	16	1.0


Junk Tolerance Threshold: 2
Coverage: 16
Num Tokens: 1
Struct
Coverage:16
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 12.
StructScore: 4.
	1:	12	0.75


Cluster 1:
Token: [string]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 12.
	1:	4	0.25

Token: [other](.)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 12.
	1:	4	0.25


Junk Tolerance Threshold: 2
Coverage: 12
Num Tokens: 1
Struct
Coverage:12
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	1:	4	1.0

Token: [other](.)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	1:	4	1.0


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 2
Struct
Coverage:4
Token count:2
[string]	Occurrences:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](,)
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 4.
StructScore: 8.
	2:	4	0.333333333333

Token: [other](-)
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 4.
StructScore: 8.
	2:	4	0.333333333333


Cluster 1:
Token: [string]
Total number of token occurrences: 60.
Number of records with at least one token occurrence: 12.
StructScore: 16.
	1:	4	0.333333333333
	3:	4	0.333333333333
	11:	4	0.333333333333

Token: [white]
Total number of token occurrences: 60.
Number of records with at least one token occurrence: 12.
StructScore: 16.
	1:	4	0.333333333333
	2:	4	0.333333333333
	12:	4	0.333333333333

Token: [other](.)
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 8.
StructScore: 16.
	1:	4	0.333333333333
	2:	4	0.333333333333


Junk Tolerance Threshold: 2
Coverage: 4
Num Tokens: 4
Struct
Coverage:4
Token count:4
[other](,)	Occurrences:2
[other](-)	Occurrences:2
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [white]
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
[white]	Occurrences:1
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
Token: [string]
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	3:	4	1.0

Token: [white]
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	3:	4	1.0


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 6
Struct
Coverage:4
Token count:6
[string]	Occurrences:3
[white]	Occurrences:3
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 28.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	7:	4	1.0

Token: [white]
Total number of token occurrences: 32.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	8:	4	1.0

Token: [other](.)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	1:	4	1.0


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 16
Struct
Coverage:4
Token count:16
[string]	Occurrences:7
[white]	Occurrences:8
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 8.
StructScore: 4.
	1:	4	0.5
	3:	4	0.5

Token: [white]
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 8.
StructScore: 4.
	1:	4	0.5
	2:	4	0.5

Token: [other](.)
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 4.
StructScore: 4.
	2:	4	0.5


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 2
Struct
Coverage:4
Token count:2
[other](.)	Occurrences:2
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	1:	4	1.0

Token: [white]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	1:	4	1.0


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 2
Struct
Coverage:4
Token count:2
[string]	Occurrences:1
[white]	Occurrences:1
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
Token: [string]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	1:	4	1.0

Token: [white]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	1:	4	1.0


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 2
Struct
Coverage:4
Token count:2
[string]	Occurrences:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	1:	4	1.0

Token: [white]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 0.
	1:	4	1.0


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 2
Struct
Coverage:4
Token count:2
[string]	Occurrences:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 20.
Number of records with at least one token occurrence: 20.
StructScore: 8.
	1:	20	0.714285714286

Token: [white]
Total number of token occurrences: 40.
Number of records with at least one token occurrence: 20.
StructScore: 8.
	2:	20	0.714285714286


Cluster 1:
Token: [Path]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 24.
	1:	4	0.142857142857

Token: [other](?)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 24.
	1:	4	0.142857142857


Junk Tolerance Threshold: 3
Coverage: 20
Num Tokens: 3
Struct
Coverage:20
Token count:3
[int]	Occurrences:1
[white]	Occurrences:2
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Path]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 4.
	1:	4	0.5

Token: [other](?)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 4.
	1:	4	0.5


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 2
Struct
Coverage:4
Token count:2
[Path]	Occurrences:1
[other](?)	Occurrences:1
converting false struct into union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Path]
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
[Path]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](?)
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
[other](?)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [Path]
Total number of token occurrences: 18.
Number of records with at least one token occurrence: 18.
StructScore: 216.
	1:	18	0.0769230769231


Cluster 1:
Token: [other](,)
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 12.
StructScore: 222.
	1:	12	0.0512820512821

Token: [other](-)
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 11.
StructScore: 223.
	1:	11	0.0470085470085


Cluster 2:
Token: [Time]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 228.
	1:	6	0.025641025641

Token: [Date]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 228.
	1:	6	0.025641025641

Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 228.
	1:	6	0.025641025641

Token: [Host]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 229.
	1:	5	0.0213675213675


Cluster 3:
Token: [URL]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](\^[)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	2:	2	0.00854700854701

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](>)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	3:	2	0.00854700854701


Cluster 4:
Token: [int]
Total number of token occurrences: 33.
Number of records with at least one token occurrence: 29.
StructScore: 619.
	1:	25	0.106837606838
	2:	4	0.017094017094


Cluster 5:
Token: [other](.)
Total number of token occurrences: 24.
Number of records with at least one token occurrence: 22.
StructScore: 638.
	1:	20	0.0854700854701
	2:	2	0.00854700854701


Cluster 6:
Token: [white]
Total number of token occurrences: 723.
Number of records with at least one token occurrence: 230.
StructScore: 1037.
	1:	35	0.149572649573
	2:	48	0.205128205128
	3:	102	0.435897435897
	4:	4	0.017094017094
	5:	16	0.0683760683761
	6:	9	0.0384615384615
	7:	4	0.017094017094
	8:	2	0.00854700854701
	9:	8	0.034188034188
	10:	2	0.00854700854701


Cluster 7:
Token: [string]
Total number of token occurrences: 594.
Number of records with at least one token occurrence: 214.
StructScore: 1206.
	1:	65	0.277777777778
	2:	6	0.025641025641
	3:	108	0.461538461538
	4:	14	0.0598290598291
	5:	5	0.0213675213675
	6:	4	0.017094017094
	7:	8	0.034188034188
	8:	4	0.017094017094


Junk Tolerance Threshold: 24
Coverage: 18
Num Tokens: 1
Clusters sorted by array criteria:
Cluster 0:
Token: [white]
Total number of token occurrences: 723.
Number of records with at least one token occurrence: 230.
StructScore: 1037.
	1:	35	0.149572649573
	2:	48	0.205128205128
	3:	102	0.435897435897
	4:	4	0.017094017094
	5:	16	0.0683760683761
	6:	9	0.0384615384615
	7:	4	0.017094017094
	8:	2	0.00854700854701
	9:	8	0.034188034188
	10:	2	0.00854700854701


Cluster 1:
Token: [string]
Total number of token occurrences: 594.
Number of records with at least one token occurrence: 214.
StructScore: 1206.
	1:	65	0.277777777778
	2:	6	0.025641025641
	3:	108	0.461538461538
	4:	14	0.0598290598291
	5:	5	0.0213675213675
	6:	4	0.017094017094
	7:	8	0.034188034188
	8:	4	0.017094017094


Cluster 2:
Token: [int]
Total number of token occurrences: 33.
Number of records with at least one token occurrence: 29.
StructScore: 619.
	1:	25	0.106837606838
	2:	4	0.017094017094


Cluster 3:
Token: [other](.)
Total number of token occurrences: 24.
Number of records with at least one token occurrence: 22.
StructScore: 638.
	1:	20	0.0854700854701
	2:	2	0.00854700854701


Cluster 4:
Token: [Path]
Total number of token occurrences: 18.
Number of records with at least one token occurrence: 18.
StructScore: 216.
	1:	18	0.0769230769231


Cluster 5:
Token: [other](,)
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 12.
StructScore: 222.
	1:	12	0.0512820512821

Token: [other](-)
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 11.
StructScore: 223.
	1:	11	0.0470085470085


Cluster 6:
Token: [Time]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 228.
	1:	6	0.025641025641

Token: [Date]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 228.
	1:	6	0.025641025641

Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 228.
	1:	6	0.025641025641

Token: [Host]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 229.
	1:	5	0.0213675213675


Cluster 7:
Token: [URL]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](\^[)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	2:	2	0.00854700854701

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](>)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	3:	2	0.00854700854701


Possible array tokens:
[white]
Records in possible array context:234
Total:723
Coverage:230
Width:10
Array	[white]	Occurrences:1
WARNING: ARRAY first context empty!WARNING: ARRAY first context empty!WARNING: ARRAY first context empty!WARNING: ARRAY first context empty!Array context
Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [white]
Total number of token occurrences: 230.
Number of records with at least one token occurrence: 230.
StructScore: 4.
	1:	230	0.982905982906


Cluster 1:
Token: [empty]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 230.
	1:	4	0.017094017094


Junk Tolerance Threshold: 24
Coverage: 230
Num Tokens: 1
Struct
Coverage:230
Token count:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
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
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 5.
Computed clusters
Cluster 0:
Token: [white]
Total number of token occurrences: 493.
Number of records with at least one token occurrence: 493.
StructScore: 0.
	1:	493	1.0


Cluster 1:
Token: [string]
Total number of token occurrences: 456.
Number of records with at least one token occurrence: 456.
StructScore: 37.
	1:	456	0.924949290061


Cluster 2:
Token: [int]
Total number of token occurrences: 19.
Number of records with at least one token occurrence: 19.
StructScore: 474.
	1:	19	0.0385395537525


Cluster 3:
Token: [other](,)
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 12.
StructScore: 481.
	1:	12	0.0243407707911

Token: [other](-)
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 11.
StructScore: 482.
	1:	11	0.0223123732252

Token: [other](.)
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 8.
StructScore: 485.
	1:	8	0.0162271805274

Token: [Time]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 487.
	1:	6	0.0121703853955

Token: [Date]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 487.
	1:	6	0.0121703853955

Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 487.
	1:	6	0.0121703853955

Token: [Host]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 488.
	1:	5	0.0101419878296

Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 489.
	1:	4	0.00811359026369

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 491.
	1:	2	0.00405679513185


Junk Tolerance Threshold: 50
Coverage: 493
Num Tokens: 1
Struct
Coverage:493
Token count:1
[white]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 5.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 456.
Number of records with at least one token occurrence: 456.
StructScore: 37.
	1:	456	0.924949290061


Cluster 1:
Token: [int]
Total number of token occurrences: 19.
Number of records with at least one token occurrence: 19.
StructScore: 474.
	1:	19	0.0385395537525


Cluster 2:
Token: [other](,)
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 12.
StructScore: 481.
	1:	12	0.0243407707911

Token: [other](-)
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 11.
StructScore: 482.
	1:	11	0.0223123732252

Token: [other](.)
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 8.
StructScore: 485.
	1:	8	0.0162271805274

Token: [Time]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 487.
	1:	6	0.0121703853955

Token: [Date]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 487.
	1:	6	0.0121703853955

Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 487.
	1:	6	0.0121703853955

Token: [Host]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 488.
	1:	5	0.0101419878296

Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 489.
	1:	4	0.00811359026369

Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 491.
	1:	2	0.00405679513185


Junk Tolerance Threshold: 50
Coverage: 456
Num Tokens: 1
Struct
Coverage:456
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 5.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 436.
Number of records with at least one token occurrence: 436.
StructScore: 20.
	1:	436	0.956140350877


Cluster 1:
Token: [other](-)
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 11.
StructScore: 445.
	1:	11	0.0241228070175


Cluster 2:
Token: [Host]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 451.
	1:	5	0.0109649122807

Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 452.
	1:	4	0.00877192982456


Junk Tolerance Threshold: 46
Coverage: 436
Num Tokens: 1
Struct
Coverage:436
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](-)
Total number of token occurrences: 11.
Number of records with at least one token occurrence: 11.
StructScore: 9.
	1:	11	0.55


Cluster 1:
Token: [Host]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 15.
	1:	5	0.25


Cluster 2:
Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 16.
	1:	4	0.2


Junk Tolerance Threshold: 2
Coverage: 11
Num Tokens: 1
Struct
Coverage:11
Token count:1
[other](-)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Host]
Total number of token occurrences: 5.
Number of records with at least one token occurrence: 5.
StructScore: 4.
	1:	5	0.555555555556


Cluster 1:
Token: [other]($)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 5.
	1:	4	0.444444444444


Junk Tolerance Threshold: 1
Coverage: 5
Num Tokens: 1
Struct
Coverage:5
Token count:1
[Host]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other]($)
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
[other]($)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 5.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 440.
Number of records with at least one token occurrence: 440.
StructScore: 16.
	1:	440	0.964912280702


Cluster 1:
Token: [other](,)
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 12.
StructScore: 444.
	1:	12	0.0263157894737


Cluster 2:
Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 454.
	1:	2	0.00438596491228

Token: [other](.)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 454.
	1:	2	0.00438596491228


Junk Tolerance Threshold: 46
Coverage: 440
Num Tokens: 1
Struct
Coverage:440
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](,)
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 12.
StructScore: 4.
	1:	12	0.75


Cluster 1:
Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 14.
	1:	2	0.125

Token: [other](.)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 14.
	1:	2	0.125


Junk Tolerance Threshold: 2
Coverage: 12
Num Tokens: 1
Struct
Coverage:12
Token count:1
[other](,)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](()[Group Body][other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 2.
	1:	2	0.5

Token: [other](.)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 2.
	1:	2	0.5


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 2
Struct
Coverage:2
Token count:2
[other](()[Group Body][other]())	Occurrences:1
[other](.)	Occurrences:1
converting false struct into union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](()[Group Body][other]())
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
[other](()[Group Body][other]())	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [other](()
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [other]())
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 3
Struct
Coverage:2
Token count:3
[string]	Occurrences:1
[other](()	Occurrences:1
[other]())	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](.)
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
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 19.
Number of records with at least one token occurrence: 19.
StructScore: 18.
	1:	19	0.513513513514


Cluster 1:
Token: [Time]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 31.
	1:	6	0.162162162162

Token: [Date]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 31.
	1:	6	0.162162162162

Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 31.
	1:	6	0.162162162162

Token: [other](.)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 31.
	1:	6	0.162162162162


Junk Tolerance Threshold: 4
Coverage: 19
Num Tokens: 1
Struct
Coverage:19
Token count:1
[int]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Time]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 12.
	1:	6	0.333333333333

Token: [Date]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 12.
	1:	6	0.333333333333

Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 12.
	1:	6	0.333333333333

Token: [other](.)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 12.
	1:	6	0.333333333333


Junk Tolerance Threshold: 2
Coverage: 6
Num Tokens: 4
Struct
Coverage:6
Token count:4
[Time]	Occurrences:1
[Date]	Occurrences:1
[IP]	Occurrences:1
[other](.)	Occurrences:1
converting false struct into union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Time]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 0.
	1:	6	1.0


Junk Tolerance Threshold: 1
Coverage: 6
Num Tokens: 1
Struct
Coverage:6
Token count:1
[Time]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Date]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 0.
	1:	6	1.0


Junk Tolerance Threshold: 1
Coverage: 6
Num Tokens: 1
Struct
Coverage:6
Token count:1
[Date]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [IP]
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 6.
StructScore: 0.
	1:	6	1.0

Token: [other](.)
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
[IP]	Occurrences:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 3.
Computed clusters
Cluster 0:
Token: [string]
Total number of token occurrences: 138.
Number of records with at least one token occurrence: 138.
StructScore: 96.
	1:	138	0.589743589744


Cluster 1:
Token: [empty]
Total number of token occurrences: 74.
Number of records with at least one token occurrence: 74.
StructScore: 160.
	1:	74	0.316239316239


Cluster 2:
Token: [Path]
Total number of token occurrences: 18.
Number of records with at least one token occurrence: 18.
StructScore: 216.
	1:	18	0.0769230769231


Cluster 3:
Token: [URL]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](\^[)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	1:	2	0.00854700854701

Token: [other](>)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 2.
StructScore: 232.
	3:	2	0.00854700854701


Cluster 4:
Token: [other](.)
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 14.
StructScore: 662.
	1:	12	0.0512820512821
	2:	2	0.00854700854701


Cluster 5:
Token: [int]
Total number of token occurrences: 14.
Number of records with at least one token occurrence: 12.
StructScore: 668.
	1:	10	0.042735042735
	2:	2	0.00854700854701


Junk Tolerance Threshold: 24
Coverage: 138
Num Tokens: 1
Struct
Coverage:138
Token count:1
[string]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 2.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 124.
Number of records with at least one token occurrence: 124.
StructScore: 14.
	1:	124	0.898550724638


Cluster 1:
Token: [Path]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 136.
	1:	2	0.0144927536232

Token: [int]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 136.
	2:	2	0.0144927536232


Cluster 2:
Token: [other](.)
Total number of token occurrences: 14.
Number of records with at least one token occurrence: 12.
StructScore: 380.
	1:	10	0.0724637681159
	2:	2	0.0144927536232


Junk Tolerance Threshold: 14
Coverage: 124
Num Tokens: 1
Struct
Coverage:124
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](.)
Total number of token occurrences: 14.
Number of records with at least one token occurrence: 12.
StructScore: 8.
	1:	10	0.714285714286
	2:	2	0.142857142857


Cluster 1:
Token: [Path]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 12.
	1:	2	0.142857142857

Token: [int]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 12.
	2:	2	0.142857142857


Junk Tolerance Threshold: 2
Coverage: 14
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [other](.)
Total number of token occurrences: 14.
Number of records with at least one token occurrence: 12.
StructScore: 8.
	1:	10	0.714285714286
	2:	2	0.142857142857


Cluster 1:
Token: [Path]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 12.
	1:	2	0.142857142857

Token: [int]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 12.
	2:	2	0.142857142857


Possible array tokens:
[other](.)
Records in possible array context:14
Total:14
Coverage:12
Width:2
ARRAY NOT CHOSEN
Union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Path]
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
[Path]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](.)
Total number of token occurrences: 14.
Number of records with at least one token occurrence: 12.
StructScore: 2.
	1:	10	0.833333333333
	2:	2	0.166666666667


Cluster 1:
Token: [int]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 10.
	2:	2	0.166666666667


Junk Tolerance Threshold: 2
Coverage: 12
Num Tokens: 0
Clusters sorted by array criteria:
Cluster 0:
Token: [other](.)
Total number of token occurrences: 14.
Number of records with at least one token occurrence: 12.
StructScore: 2.
	1:	10	0.833333333333
	2:	2	0.166666666667


Cluster 1:
Token: [int]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 10.
	2:	2	0.166666666667


Possible array tokens:
[other](.)
Records in possible array context:12
Total:14
Coverage:12
Width:2
Array	[other](.)	Occurrences:1
Array context
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](.)
Total number of token occurrences: 12.
Number of records with at least one token occurrence: 12.
StructScore: 0.
	1:	12	1.0


Junk Tolerance Threshold: 2
Coverage: 12
Num Tokens: 1
Struct
Coverage:12
Token count:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [other](.)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 2
Struct
Coverage:2
Token count:2
[int]	Occurrences:1
[other](.)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 2.
	1:	10	0.833333333333


Cluster 1:
Token: [int]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 10.
	1:	2	0.166666666667


Junk Tolerance Threshold: 2
Coverage: 10
Num Tokens: 1
Struct
Coverage:10
Token count:1
[empty]	Occurrences:1
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
Total number of token occurrences: 74.
Number of records with at least one token occurrence: 74.
StructScore: 22.
	1:	74	0.770833333333


Cluster 1:
Token: [Path]
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 16.
StructScore: 80.
	1:	16	0.166666666667


Cluster 2:
Token: [int]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 86.
	1:	10	0.104166666667


Cluster 3:
Token: [URL]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 94.
	1:	2	0.0208333333333

Token: [other](\^[)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 94.
	1:	2	0.0208333333333

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 94.
	1:	2	0.0208333333333

Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 94.
	1:	2	0.0208333333333

Token: [other](.)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 94.
	1:	2	0.0208333333333

Token: [other](>)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 2.
StructScore: 94.
	3:	2	0.0208333333333


Junk Tolerance Threshold: 10
Coverage: 74
Num Tokens: 1
Struct
Coverage:74
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Path]
Total number of token occurrences: 16.
Number of records with at least one token occurrence: 16.
StructScore: 6.
	1:	16	0.727272727273


Cluster 1:
Token: [int]
Total number of token occurrences: 10.
Number of records with at least one token occurrence: 10.
StructScore: 12.
	1:	10	0.454545454545


Cluster 2:
Token: [URL]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 20.
	1:	2	0.0909090909091

Token: [other](\^[)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 20.
	1:	2	0.0909090909091

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 20.
	1:	2	0.0909090909091

Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 20.
	1:	2	0.0909090909091

Token: [other](.)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 20.
	1:	2	0.0909090909091

Token: [other](>)
Total number of token occurrences: 6.
Number of records with at least one token occurrence: 2.
StructScore: 20.
	3:	2	0.0909090909091


Junk Tolerance Threshold: 3
Coverage: 16
Num Tokens: 1
Struct
Coverage:16
Token count:1
[Path]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 8.
Number of records with at least one token occurrence: 8.
StructScore: 8.
	1:	8	0.5


Cluster 1:
Token: [empty]
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 12.
	1:	4	0.25


Cluster 2:
Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 14.
	1:	2	0.125

Token: [other](>)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 14.
	2:	2	0.125


Junk Tolerance Threshold: 2
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
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 4.
StructScore: 4.
	1:	4	0.5


Cluster 1:
Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 6.
	1:	2	0.25

Token: [other](>)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 6.
	2:	2	0.25


Junk Tolerance Threshold: 1
Coverage: 4
Num Tokens: 1
Struct
Coverage:4
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](')
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 2.
	1:	2	0.5

Token: [other](>)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 2.
	2:	2	0.5


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 3
Struct
Coverage:2
Token count:3
[other](')	Occurrences:1
[other](>)	Occurrences:2
converting false struct into union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](')
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
[other](')	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](>)
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
[other](>)	Occurrences:2
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 14.
Number of records with at least one token occurrence: 14.
StructScore: 2.
	1:	14	0.875


Cluster 1:
Token: [int]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 14.
	1:	2	0.125

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 14.
	1:	2	0.125

Token: [other](>)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 14.
	1:	2	0.125


Junk Tolerance Threshold: 2
Coverage: 14
Num Tokens: 1
Struct
Coverage:14
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [int]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [other](&)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [other](>)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 3
Struct
Coverage:2
Token count:3
[int]	Occurrences:1
[other](&)	Occurrences:1
[other](>)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [URL]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 4.
	1:	2	0.333333333333

Token: [other](\^[)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 4.
	1:	2	0.333333333333

Token: [other](.)
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 4.
	1:	2	0.333333333333


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 3
Struct
Coverage:2
Token count:3
[URL]	Occurrences:1
[other](\^[)	Occurrences:1
[other](.)	Occurrences:1
converting false struct into union
BUILDING UNION TY
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [URL]
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
[URL]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](\^[)
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
[other](\^[)	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [other](.)
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
[other](.)	Occurrences:1

Before reduction:
Pstruct(Id = BTy_215 262, 1b, 0b)
	Punion(Id = BTy_1 262, 1b, 0b)
		[empty](Id = BTy_0 260, 1b, 0b);
		Pstruct(Id = BTy_5 2, 1b, 0b)
			[other]([)(Id = BTy_2 2, 1b, 0b);
			[int](Id = BTy_3 2, 1b, 0b);
			[string](Id = BTy_4 2, 1b, 0b);
		End Pstruct;
	End Punion;
	[Date](Id = BTy_6 262, 1b, 0b);
	[white](Id = BTy_7 262, 1b, 0b);
	[Time](Id = BTy_8 262, 1b, 0b);
	Parray(Id = BTy_9 262, 1b, 0b)([other](:) )
	First:
		Punion(Id = BTy_19 262, 1b, 0b)
			Pstruct(Id = BTy_18 261, 1b, 0b)
				[white](Id = BTy_10 261, 1b, 0b);
				[string](Id = BTy_11 261, 1b, 0b);
				[white](Id = BTy_12 261, 1b, 0b);
				Punion(Id = BTy_14 261, 1b, 0b)
					[empty](Id = BTy_13 233, 1b, 0b);
					[Host](Id = BTy_15 28, 1b, 0b);
				End Punion;
				[string](Id = BTy_16 261, 1b, 0b);
				[other](:)(Id = BTy_17 261, 1b, 0b);
			End Pstruct;
			[empty](Id = BTy_20 1, 1b, 0b);
		End Punion;
	Body:
		Pstruct(Id = BTy_71 95, 1b, 0b)
			Punion(Id = BTy_43 95, 1b, 0b)
				Pstruct(Id = BTy_42 14, 1b, 0b)
					Punion(Id = BTy_22 14, 1b, 0b)
						[empty](Id = BTy_21 8, 1b, 0b);
						Pstruct(Id = BTy_33 6, 1b, 0b)
							[white](Id = BTy_23 6, 1b, 0b);
							[string](Id = BTy_24 6, 1b, 0b);
							Punion(Id = BTy_31 6, 1b, 0b)
								Pstruct(Id = BTy_30 4, 1b, 0b)
									[white](Id = BTy_25 4, 1b, 0b);
									[Date](Id = BTy_26 4, 1b, 0b);
									[white](Id = BTy_27 4, 1b, 0b);
									[Time](Id = BTy_28 4, 1b, 0b);
									[white](Id = BTy_29 4, 1b, 0b);
								End Pstruct;
								[white](Id = BTy_32 2, 1b, 0b);
							End Punion;
						End Pstruct;
					End Punion;
					[int](Id = BTy_34 14, 1b, 0b);
					Punion(Id = BTy_40 14, 1b, 0b)
						Pstruct(Id = BTy_39 12, 1b, 0b)
							Punion(Id = BTy_36 12, 1b, 0b)
								[empty](Id = BTy_35 8, 1b, 0b);
								[white](Id = BTy_37 4, 1b, 0b);
							End Punion;
							[string](Id = BTy_38 12, 1b, 0b);
						End Pstruct;
						[empty](Id = BTy_41 2, 1b, 0b);
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_53 10, 1b, 0b)
					Parray(Id = BTy_44 10, 1b, 0b)([white] )
					First:
						[white](Id = BTy_45 10, 1b, 0b);
					Body:
						Pstruct(Id = BTy_48 8, 1b, 0b)
							[string](Id = BTy_46 8, 1b, 0b);
							[white](Id = BTy_47 8, 1b, 0b);
						End Pstruct;
					Tail:
						Punion(Id = BTy_50 10, 1b, 0b)
							[empty](Id = BTy_49 6, 1b, 0b);
							[string](Id = BTy_51 4, 1b, 0b);
						End Punion;
					End Parray;
					[Path](Id = BTy_52 10, 1b, 0b);
				End Pstruct;
				[string](Id = BTy_55 8, 1b, 0b);
				Parray(Id = BTy_56 63, 1b, 0b)([white] )
				First:
					[white](Id = BTy_57 63, 1b, 0b);
				Body:
					Pstruct(Id = BTy_60 119, 1b, 0b)
						[string](Id = BTy_58 119, 1b, 0b);
						[white](Id = BTy_59 119, 1b, 0b);
					End Pstruct;
				Tail:
					Punion(Id = BTy_62 63, 1b, 0b)
						[string](Id = BTy_61 59, 1b, 0b);
						[Host](Id = BTy_63 2, 1b, 0b);
						Pstruct(Id = BTy_67 2, 1b, 0b)
							[other](()(Id = BTy_64 2, 1b, 0b);
							[string](Id = BTy_65 2, 1b, 0b);
							[other]())(Id = BTy_66 2, 1b, 0b);
						End Pstruct;
					End Punion;
				End Parray;
			End Punion;
			[other](:)(Id = BTy_70 95, 1b, 0b);
		End Pstruct;
	Tail:
		Punion(Id = BTy_141 262, 1b, 0b)
			Pstruct(Id = BTy_140 28, 1b, 0b)
				Punion(Id = BTy_84 28, 1b, 0b)
					Pstruct(Id = BTy_83 16, 1b, 0b)
						[white](Id = BTy_72 16, 1b, 0b);
						[Host](Id = BTy_73 16, 1b, 0b);
						[string](Id = BTy_75 16, 1b, 0b);
						Punion(Id = BTy_77 16, 1b, 0b)
							[empty](Id = BTy_76 12, 1b, 0b);
							Pstruct(Id = BTy_80 4, 1b, 0b)
								[other](.)(Id = BTy_78 4, 1b, 0b);
								[string](Id = BTy_79 4, 1b, 0b);
							End Pstruct;
						End Punion;
						[white](Id = BTy_81 16, 1b, 0b);
					End Pstruct;
					Pstruct(Id = BTy_115 4, 1b, 0b)
						[white](Id = BTy_85 4, 1b, 0b);
						[other](-)(Id = BTy_86 4, 1b, 0b);
						[other](-)(Id = BTy_87 4, 1b, 0b);
						[string](Id = BTy_88 4, 1b, 0b);
						[other](,)(Id = BTy_89 4, 1b, 0b);
						[white](Id = BTy_90 4, 1b, 0b);
						[string](Id = BTy_91 4, 1b, 0b);
						[white](Id = BTy_92 4, 1b, 0b);
						[string](Id = BTy_93 4, 1b, 0b);
						[white](Id = BTy_94 4, 1b, 0b);
						[string](Id = BTy_95 4, 1b, 0b);
						[other](,)(Id = BTy_97 4, 1b, 0b);
						[white](Id = BTy_98 4, 1b, 0b);
						[string](Id = BTy_99 4, 1b, 0b);
						[white](Id = BTy_100 4, 1b, 0b);
						[string](Id = BTy_101 4, 1b, 0b);
						[white](Id = BTy_102 4, 1b, 0b);
						[string](Id = BTy_103 4, 1b, 0b);
						[other](.)(Id = BTy_104 4, 1b, 0b);
						[white](Id = BTy_105 4, 1b, 0b);
						[string](Id = BTy_106 4, 1b, 0b);
						[white](Id = BTy_107 4, 1b, 0b);
						[string](Id = BTy_108 4, 1b, 0b);
						[white](Id = BTy_109 4, 1b, 0b);
						[string](Id = BTy_110 4, 1b, 0b);
						[white](Id = BTy_111 4, 1b, 0b);
						[string](Id = BTy_112 4, 1b, 0b);
						[white](Id = BTy_113 4, 1b, 0b);
					End Pstruct;
					Pstruct(Id = BTy_126 4, 1b, 0b)
						[white](Id = BTy_117 4, 1b, 0b);
						[string](Id = BTy_118 4, 1b, 0b);
						[other](.)(Id = BTy_120 4, 1b, 0b);
						[string](Id = BTy_121 4, 1b, 0b);
						[other](.)(Id = BTy_122 4, 1b, 0b);
						[string](Id = BTy_123 4, 1b, 0b);
						[white](Id = BTy_124 4, 1b, 0b);
					End Pstruct;
					Pstruct(Id = BTy_130 4, 1b, 0b)
						[white](Id = BTy_128 4, 1b, 0b);
						[string](Id = BTy_129 4, 1b, 0b);
					End Pstruct;
				End Punion;
				[other](=)(Id = BTy_131 28, 1b, 0b);
				Punion(Id = BTy_136 28, 1b, 0b)
					Pstruct(Id = BTy_135 20, 1b, 0b)
						[white](Id = BTy_132 20, 1b, 0b);
						[int](Id = BTy_133 20, 1b, 0b);
						[white](Id = BTy_134 20, 1b, 0b);
					End Pstruct;
					[Path](Id = BTy_137 4, 1b, 0b);
					[other](?)(Id = BTy_138 4, 1b, 0b);
				End Punion;
			End Pstruct;
			Parray(Id = BTy_142 234, 1b, 0b)([white] )
			First:
				Punion(Id = BTy_144 234, 1b, 0b)
					[white](Id = BTy_143 230, 1b, 0b);
					[empty](Id = BTy_145 4, 1b, 0b);
				End Punion;
			Body:
				Pstruct(Id = BTy_175 493, 1b, 0b)
					Punion(Id = BTy_165 493, 1b, 0b)
						Pstruct(Id = BTy_164 456, 1b, 0b)
							Punion(Id = BTy_147 456, 1b, 0b)
								[empty](Id = BTy_146 436, 1b, 0b);
								[other](-)(Id = BTy_148 11, 1b, 0b);
								[Host](Id = BTy_150 5, 1b, 0b);
								[other]($)(Id = BTy_152 4, 1b, 0b);
							End Punion;
							[string](Id = BTy_153 456, 1b, 0b);
							Punion(Id = BTy_155 456, 1b, 0b)
								[empty](Id = BTy_154 440, 1b, 0b);
								[other](,)(Id = BTy_156 12, 1b, 0b);
								Pstruct(Id = BTy_161 2, 1b, 0b)
									[other](()(Id = BTy_158 2, 1b, 0b);
									[string](Id = BTy_159 2, 1b, 0b);
									[other]())(Id = BTy_160 2, 1b, 0b);
								End Pstruct;
								[other](.)(Id = BTy_162 2, 1b, 0b);
							End Punion;
						End Pstruct;
						[int](Id = BTy_166 19, 1b, 0b);
						[Time](Id = BTy_168 6, 1b, 0b);
						[Date](Id = BTy_169 6, 1b, 0b);
						Pstruct(Id = BTy_172 6, 1b, 0b)
							[IP](Id = BTy_170 6, 1b, 0b);
							[other](.)(Id = BTy_171 6, 1b, 0b);
						End Pstruct;
					End Punion;
					[white](Id = BTy_174 493, 1b, 0b);
				End Pstruct;
			Tail:
				Punion(Id = BTy_190 234, 1b, 0b)
					Pstruct(Id = BTy_189 138, 1b, 0b)
						[string](Id = BTy_176 138, 1b, 0b);
						Punion(Id = BTy_178 138, 1b, 0b)
							[empty](Id = BTy_177 124, 1b, 0b);
							[Path](Id = BTy_179 2, 1b, 0b);
							Parray(Id = BTy_180 12, 1b, 0b)([other](.) )
							First:
								[other](.)(Id = BTy_181 12, 1b, 0b);
							Body:
								Pstruct(Id = BTy_184 2, 1b, 0b)
									[int](Id = BTy_182 2, 1b, 0b);
									[other](.)(Id = BTy_183 2, 1b, 0b);
								End Pstruct;
							Tail:
								Punion(Id = BTy_186 12, 1b, 0b)
									[empty](Id = BTy_185 10, 1b, 0b);
									[int](Id = BTy_187 2, 1b, 0b);
								End Punion;
							End Parray;
						End Punion;
					End Pstruct;
					[empty](Id = BTy_191 74, 1b, 0b);
					Pstruct(Id = BTy_209 16, 1b, 0b)
						Punion(Id = BTy_194 16, 1b, 0b)
							[int](Id = BTy_193 8, 1b, 0b);
							[empty](Id = BTy_195 4, 1b, 0b);
							[other](')(Id = BTy_197 2, 1b, 0b);
							Pstruct(Id = BTy_200 2, 1b, 0b)
								[other](>)(Id = BTy_198 2, 1b, 0b);
								[other](>)(Id = BTy_199 2, 1b, 0b);
							End Pstruct;
						End Punion;
						[Path](Id = BTy_202 16, 1b, 0b);
						Punion(Id = BTy_204 16, 1b, 0b)
							[empty](Id = BTy_203 14, 1b, 0b);
							Pstruct(Id = BTy_208 2, 1b, 0b)
								[other](>)(Id = BTy_205 2, 1b, 0b);
								[other](&)(Id = BTy_206 2, 1b, 0b);
								[int](Id = BTy_207 2, 1b, 0b);
							End Pstruct;
						End Punion;
					End Pstruct;
					[URL](Id = BTy_211 2, 1b, 0b);
					[other](\^[)(Id = BTy_212 2, 1b, 0b);
					[other](.)(Id = BTy_213 2, 1b, 0b);
				End Punion;
			End Parray;
		End Punion;
	End Parray;
End Pstruct


After final reduction:
Pstruct(Id = BTy_215 262, 1b, 0b)
	Punion(Id = BTy_1 262, 1b, 0b)
		[StringConst] ""(Id = BTy_0 260, 1b, 0b);
		Pstruct(Id = BTy_5 2, 1b, 0b)
			[StringConst] "["(Id = BTy_2 2, 1b, 0b);
			[IntConst] [60](Id = BTy_3 2, 1b, 0b);
			[StringConst] "G"(Id = BTy_4 2, 1b, 0b);
		End Pstruct;
	End Punion;
	[Date](Id = BTy_6 262, 1b, 0b);
	[StringConst] " "(Id = BTy_7 262, 1b, 0b);
	[Time](Id = BTy_8 262, 1b, 0b);
	Punion(Id = BTy_19 262, 1b, 0b)
		Pstruct(Id = BTy_18 261, 1b, 0b)
			[StringConst] " srv7 "(Id = BTy_10 261, 1b, 0b);
			Punion(Id = BTy_14 261, 1b, 0b)
				[StringConst] ""(Id = BTy_13 233, 1b, 0b);
				[Enum] {[StringConst] "lvm.st", [StringConst] "rc.sy", }(Id = BTy_15 28, 1b, 0b);
			End Punion;
			[string](Id = BTy_16 261, 1b, 0b);
			[StringConst] ":"(Id = BTy_17 261, 1b, 0b);
		End Pstruct;
		[StringConst] ""(Id = BTy_20 1, 1b, 0b);
	End Punion;
	RArray(Id = BTy_9 262, 1b, 0b)
		Pstruct(Id = BTy_71 95, 1b, 0b)
			Punion(Id = BTy_43 95, 1b, 0b)
				Pstruct(Id = BTy_42 14, 1b, 0b)
					Switch(BTy_34)(Id = BTy_22 14, 1b, 0b):
					case [IntConst] [0]:
						[StringConst] ""(Id = BTy_21 8, 1b, 0b);
					case [Enum] {[IntConst] [53], [IntConst] [2006], }:
						Pstruct(Id = BTy_33 6, 1b, 0b)
							[StringConst] " "(Id = BTy_23 6, 1b, 0b);
							[Enum] {[StringConst] "Mon", [StringConst] "line", }(Id = BTy_24 6, 1b, 0b);
							[StringConst] " Apr 24 "(Id = BTy_32 6, 1b, 0b);
							[Enum] {[StringConst] "14:47:06", [StringConst] "21:08:22", }(Id = BTy_28 4, 1b, 0b);
							[StringConst] " "(Id = BTy_29 4, 1b, 0b);
						End Pstruct;
					End Switch;
					[Int] [0...2006](Id = BTy_34 14, 1b, 0b);
					Punion(Id = BTy_40 14, 1b, 0b)
						Pstruct(Id = BTy_39 12, 1b, 0b)
							Switch(BTy_38)(Id = BTy_36 12, 1b, 0b):
							case [StringConst] "c":
								[StringConst] ""(Id = BTy_35 8, 1b, 0b);
							case [StringConst] "NOTE":
								[StringConst] "  "(Id = BTy_37 4, 1b, 0b);
							End Switch;
							[Enum] {[StringConst] "NOTE", [StringConst] "c", }(Id = BTy_38 12, 1b, 0b);
						End Pstruct;
						[StringConst] ""(Id = BTy_41 2, 1b, 0b);
					End Punion;
				End Pstruct;
				Pstruct(Id = BTy_53 10, 1b, 0b)
					[StringConst] " "(Id = BTy_45 10, 1b, 0b);
					RArray(Id = BTy_44 10, 1b, 0b)
						Pstruct(Id = BTy_48 8, 1b, 0b)
							[Enum] {[StringConst] "Listening", [StringConst] "Sending", }(Id = BTy_46 8, 1b, 0b);
							[StringConst] " "(Id = BTy_47 8, 1b, 0b);
						End Pstruct;
					End RArray;
					Punion(Id = BTy_50 10, 1b, 0b)
						[StringConst] ""(Id = BTy_49 6, 1b, 0b);
						[StringConst] "LPF"(Id = BTy_51 4, 1b, 0b);
					End Punion;
					[Path](Id = BTy_52 10, 1b, 0b);
				End Pstruct;
				[StringConst] "f1"(Id = BTy_55 8, 1b, 0b);
				Pstruct(Id = BTy_218 63, 1b, 0b)
					[StringConst] " "(Id = BTy_57 63, 1b, 0b);
					RArray(Id = BTy_56 63, 1b, 0b)
						Pstruct(Id = BTy_60 119, 1b, 0b)
							[string](Id = BTy_58 119, 1b, 0b);
							[StringConst] " "(Id = BTy_59 119, 1b, 0b);
						End Pstruct;
					End RArray;
					Punion(Id = BTy_62 63, 1b, 0b)
						[string](Id = BTy_61 59, 1b, 0b);
						[StringConst] "srv7.grmtech.com"(Id = BTy_63 2, 1b, 0b);
						Pstruct(Id = BTy_67 2, 1b, 0b)
							[StringConst] "(localtime)"(Id = BTy_64 2, 1b, 0b);
						End Pstruct;
					End Punion;
				End Pstruct;
			End Punion;
			[StringConst] ":"(Id = BTy_70 95, 1b, 0b);
		End Pstruct;
	End RArray;
	Punion(Id = BTy_141 262, 1b, 0b)
		Pstruct(Id = BTy_140 28, 1b, 0b)
			[StringConst] " "(Id = BTy_85 28, 1b, 0b);
			Punion(Id = BTy_84 28, 1b, 0b)
				Pstruct(Id = BTy_83 16, 1b, 0b)
					[Host](Id = BTy_73 16, 1b, 0b);
					[string](Id = BTy_75 16, 1b, 0b);
					Punion(Id = BTy_77 16, 1b, 0b)
						[StringConst] ""(Id = BTy_76 12, 1b, 0b);
						Pstruct(Id = BTy_80 4, 1b, 0b)
							[StringConst] ".rp_filter"(Id = BTy_78 4, 1b, 0b);
						End Pstruct;
					End Punion;
					[StringConst] " "(Id = BTy_81 16, 1b, 0b);
				End Pstruct;
				Pstruct(Id = BTy_115 4, 1b, 0b)
					[StringConst] "--use-syslog, no facility specified, using default value.  Did you forget the "(Id = BTy_86 4, 1b, 0b);
				End Pstruct;
				Pstruct(Id = BTy_126 4, 1b, 0b)
					[StringConst] "net.ipv4.ip_forward "(Id = BTy_118 4, 1b, 0b);
				End Pstruct;
				Pstruct(Id = BTy_130 4, 1b, 0b)
					[StringConst] "LABEL"(Id = BTy_129 4, 1b, 0b);
				End Pstruct;
			End Punion;
			[StringConst] "="(Id = BTy_131 28, 1b, 0b);
			Punion(Id = BTy_136 28, 1b, 0b)
				Pstruct(Id = BTy_135 20, 1b, 0b)
					[StringConst] " "(Id = BTy_132 20, 1b, 0b);
					[Int] [0...1](Id = BTy_133 20, 1b, 0b);
					[StringConst] " "(Id = BTy_134 20, 1b, 0b);
				End Pstruct;
				[Enum] {[StringConst] "/boot duplicate - not mounted", [StringConst] "/boot duplicate - not mounted ", }(Id = BTy_137 4, 1b, 0b);
				[StringConst] "?"(Id = BTy_138 4, 1b, 0b);
			End Punion;
		End Pstruct;
		Pstruct(Id = BTy_221 234, 1b, 0b)
			Punion(Id = BTy_144 234, 1b, 0b)
				[StringConst] " "(Id = BTy_143 230, 1b, 0b);
				[StringConst] ""(Id = BTy_145 4, 1b, 0b);
			End Punion;
			RArray(Id = BTy_142 234, 1b, 0b)
				Pstruct(Id = BTy_175 493, 1b, 0b)
					Punion(Id = BTy_165 493, 1b, 0b)
						Pstruct(Id = BTy_164 456, 1b, 0b)
							Punion(Id = BTy_147 456, 1b, 0b)
								[StringConst] ""(Id = BTy_146 436, 1b, 0b);
								[StringConst] "-"(Id = BTy_148 11, 1b, 0b);
								[Enum] {[StringConst] "rpc.id", [StringConst] "rpc.st", }(Id = BTy_150 5, 1b, 0b);
								[StringConst] "$"(Id = BTy_152 4, 1b, 0b);
							End Punion;
							[string](Id = BTy_153 456, 1b, 0b);
							Punion(Id = BTy_155 456, 1b, 0b)
								[StringConst] ""(Id = BTy_154 440, 1b, 0b);
								[StringConst] ","(Id = BTy_156 12, 1b, 0b);
								Pstruct(Id = BTy_161 2, 1b, 0b)
									[StringConst] "(s)"(Id = BTy_158 2, 1b, 0b);
								End Pstruct;
								[StringConst] "."(Id = BTy_162 2, 1b, 0b);
							End Punion;
						End Pstruct;
						[Int] [0...4858](Id = BTy_166 19, 1b, 0b);
						[Time](Id = BTy_168 6, 1b, 0b);
						[StringConst] "Apr 24"(Id = BTy_169 6, 1b, 0b);
						Pstruct(Id = BTy_172 6, 1b, 0b)
							[IP](Id = BTy_170 6, 1b, 0b);
							[StringConst] "."(Id = BTy_171 6, 1b, 0b);
						End Pstruct;
					End Punion;
					[StringConst] " "(Id = BTy_174 493, 1b, 0b);
				End Pstruct;
			End RArray;
			Punion(Id = BTy_190 234, 1b, 0b)
				Pstruct(Id = BTy_189 138, 1b, 0b)
					[string](Id = BTy_176 138, 1b, 0b);
					Punion(Id = BTy_178 138, 1b, 0b)
						[StringConst] ""(Id = BTy_177 124, 1b, 0b);
						[StringConst] "/fallback/fallback-net"(Id = BTy_179 2, 1b, 0b);
						Pstruct(Id = BTy_220 12, 1b, 0b)
							[StringConst] "."(Id = BTy_181 12, 1b, 0b);
							RArray(Id = BTy_180 12, 1b, 0b)
								Pstruct(Id = BTy_184 2, 1b, 0b)
									[IntConst] [0](Id = BTy_182 2, 1b, 0b);
									[StringConst] "."(Id = BTy_183 2, 1b, 0b);
								End Pstruct;
							End RArray;
							Punion(Id = BTy_186 12, 1b, 0b)
								[StringConst] ""(Id = BTy_185 10, 1b, 0b);
								[IntConst] [1](Id = BTy_187 2, 1b, 0b);
							End Punion;
						End Pstruct;
					End Punion;
				End Pstruct;
				[StringConst] ""(Id = BTy_191 74, 1b, 0b);
				Pstruct(Id = BTy_209 16, 1b, 0b)
					Punion(Id = BTy_194 16, 1b, 0b)
						[Int] [38...2299759](Id = BTy_193 8, 1b, 0b);
						[StringConst] ""(Id = BTy_195 4, 1b, 0b);
						[StringConst] "'"(Id = BTy_197 2, 1b, 0b);
						Pstruct(Id = BTy_200 2, 1b, 0b)
							[StringConst] ">>"(Id = BTy_198 2, 1b, 0b);
						End Pstruct;
					End Punion;
					[Path](Id = BTy_202 16, 1b, 0b);
					Punion(Id = BTy_204 16, 1b, 0b)
						[StringConst] ""(Id = BTy_203 14, 1b, 0b);
						Pstruct(Id = BTy_208 2, 1b, 0b)
							[StringConst] ">&"(Id = BTy_205 2, 1b, 0b);
							[IntConst] [1](Id = BTy_207 2, 1b, 0b);
						End Pstruct;
					End Punion;
				End Pstruct;
				[StringConst] "http://www.isc.org/sw/dhcp/"(Id = BTy_211 2, 1b, 0b);
				[StringConst] "\^["(Id = BTy_212 2, 1b, 0b);
				[StringConst] "."(Id = BTy_213 2, 1b, 0b);
			End Punion;
		End Pstruct;
	End Punion;
End Pstruct

Complexity of inferred type:
	numAlt = 9  numTBD = 0  numBtm = 0
Outputing partitions to directory: gen/

Completed data/boot.txt
Overall type complexity = 123.662b
Overall data complexity = 9476.291b

