#! /bin/bash
timingDir=$1
dataDir=$2
prog=$3
fmt=$4
dsize=$5
mods=$6

TIMEFORMAT='%3R %3U %3S'

if [ -z $PROCESS_ONLY ]; then
    rm -f $timingDir/results_"$mods"_$fmt.txt
    (time $prog $dataDir/$fmt.$dsize 2>&1) 2> $timingDir/results_"$mods"_$fmt.txt
    (time $prog $dataDir/$fmt.$dsize  2>&1) 2>> $timingDir/results_"$mods"_$fmt.txt
    (time $prog $dataDir/$fmt.$dsize  2>&1) 2>> $timingDir/results_"$mods"_$fmt.txt
    (time $prog $dataDir/$fmt.$dsize  2>&1) 2>> $timingDir/results_"$mods"_$fmt.txt
    (time $prog $dataDir/$fmt.$dsize  2>&1) 2>> $timingDir/results_"$mods"_$fmt.txt
    (time $prog $dataDir/$fmt.$dsize  2>&1) 2>> $timingDir/results_"$mods"_$fmt.txt
    (time $prog $dataDir/$fmt.$dsize  2>&1) 2>> $timingDir/results_"$mods"_$fmt.txt
    (time $prog $dataDir/$fmt.$dsize  2>&1) 2>> $timingDir/results_"$mods"_$fmt.txt
fi
cat $timingDir/results_"$mods"_$fmt.txt | awk '{ real += $1; user += $2; sys += $3; ct += 1; } END { print real/ct, user/ct, sys/ct; }'
