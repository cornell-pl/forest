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

Token: [Date]
Total number of token occurrences: 3000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	1:	3000	1.0

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
Num Tokens: 5
Struct
Coverage:3000
Token count:5
[Time]	Occurrences:1
[Date]	Occurrences:1
(:)	Occurrences:1
([)	Occurrences:1
(])	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: (\")
Total number of token occurrences: 6000.
Number of records with at least one token occurrence: 3000.
StructScore: 0.
	2:	3000	1.0

Token: [Path]
Total number of token occurrences: 3002.
Number of records with at least one token occurrence: 3000.
StructScore: 2.
	1:	2998	0.999333333333
	2:	2	0.000666666666667

Token: [string]
Total number of token occurrences: 3002.
Number of records with at least one token occurrence: 3000.
StructScore: 2.
	1:	2998	0.999333333333
	2:	2	0.000666666666667

Token: [white space]
Total number of token occurrences: 3002.
Number of records with at least one token occurrence: 3000.
StructScore: 2.
	1:	2998	0.999333333333
	2:	2	0.000666666666667


Cluster 1:
Token: (*)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 2998.
	2:	2	0.000666666666667


Junk Tolerance Threshold: 300
Coverage: 2998
Num Tokens: 5
Struct
Coverage:2998
Token count:5
(\")	Occurrences:2
[Path]	Occurrences:1
[string]	Occurrences:1
[white space]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 30.
Computed clusters
Cluster 0:
Token: [empty]
Total number of token occurrences: 2998.
Number of records with at least one token occurrence: 2998.
StructScore: 2.
	1:	2998	0.999333333333


Cluster 1:
Token: [Path]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 2998.
	1:	2	0.000666666666667

Token: [string]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 2998.
	1:	2	0.000666666666667

Token: [white space]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 2998.
	1:	2	0.000666666666667

Token: (*)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 2998.
	2:	2	0.000666666666667


Junk Tolerance Threshold: 300
Coverage: 2998
Num Tokens: 1
Struct
Coverage:2998
Token count:1
[empty]	Occurrences:1
Building histograms...
THRESHOLD for histogram equality: 1.
Computed clusters
Cluster 0:
Token: [Path]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [string]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: [white space]
Total number of token occurrences: 2.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	1:	2	1.0

Token: (*)
Total number of token occurrences: 4.
Number of records with at least one token occurrence: 2.
StructScore: 0.
	2:	2	1.0


Junk Tolerance Threshold: 1
Coverage: 2
Num Tokens: 5
Struct
Coverage:2
Token count:5
[Path]	Occurrences:1
[string]	Occurrences:1
[white space]	Occurrences:1
(*)	Occurrences:2
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
Pstruct(Id = BTy_54 3000, 0b, 0b)
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
	[Date](Id = BTy_28 3000, 0b, 0b);
	(:)(Id = BTy_29 3000, 0b, 0b);
	[Time](Id = BTy_30 3000, 0b, 0b);
	(])(Id = BTy_31 3000, 0b, 0b);
	[white space](Id = BTy_33 3000, 0b, 0b);
	(\")(Id = BTy_34 3000, 0b, 0b);
	[string](Id = BTy_35 3000, 0b, 0b);
	[white space](Id = BTy_36 3000, 0b, 0b);
	[Path](Id = BTy_37 3000, 0b, 0b);
	Punion(Id = BTy_39 3000, 0b, 0b)
		[empty](Id = BTy_38 2998, 0b, 0b);
		Pstruct(Id = BTy_45 2, 0b, 0b)
			(*)(Id = BTy_40 2, 0b, 0b);
			(*)(Id = BTy_41 2, 0b, 0b);
			[white space](Id = BTy_42 2, 0b, 0b);
			[string](Id = BTy_43 2, 0b, 0b);
			[Path](Id = BTy_44 2, 0b, 0b);
		End Pstruct;
	End Punion;
	(\")(Id = BTy_46 3000, 0b, 0b);
	[white space](Id = BTy_48 3000, 0b, 0b);
	[int](Id = BTy_49 3000, 0b, 0b);
	[white space](Id = BTy_50 3000, 0b, 0b);
	Punion(Id = BTy_52 3000, 0b, 0b)
		[int](Id = BTy_51 2651, 0b, 0b);
		(-)(Id = BTy_53 349, 0b, 0b);
	End Punion;
End Pstruct


After final reduction:
Pstruct(Id = BTy_54 3000, 0b, 0b)
	Punion(Id = BTy_1 3000, 0b, 0b)
		[IP](Id = BTy_0 1704, 0b, 0b);
		Pstruct(Id = BTy_56 1296, 0b, 0b)
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
	{"15/Oct/1997", "16/Oct/1997", }(Id = BTy_28 3000, 0b, 0b);
	":"(Id = BTy_29 3000, 0b, 0b);
	[Time](Id = BTy_30 3000, 0b, 0b);
	"] \""(Id = BTy_31 3000, 0b, 0b);
	{"GET", "POST", }(Id = BTy_35 3000, 0b, 0b);
	" "(Id = BTy_36 3000, 0b, 0b);
	[Path](Id = BTy_37 3000, 0b, 0b);
	Switch(BTy_37)(Id = BTy_39 3000, 0b, 0b):
	case "*":
		""(Id = BTy_38 2998, 0b, 0b);
	case "/whatsnew.html":
		Pstruct(Id = BTy_45 2, 0b, 0b)
			"** HTTP/1.0"(Id = BTy_40 2, 0b, 0b);
		End Pstruct;
	End Switch;
	"\" "(Id = BTy_46 3000, 0b, 0b);
	[200...404](Id = BTy_49 3000, 0b, 0b);
	" "(Id = BTy_50 3000, 0b, 0b);
	Punion(Id = BTy_52 3000, 0b, 0b)
		[35...37947](Id = BTy_51 2651, 0b, 0b);
		"-"(Id = BTy_53 349, 0b, 0b);
	End Punion;
End Pstruct

Complexity of inferred type:
	numAlt = 7  numTBD = 0  numBtm = 0
Outputing partitions to directory: gen/

Completed data/ai.3000
Overall type complexity = 37b
Overall data complexity = nanb

