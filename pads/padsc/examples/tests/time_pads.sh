#!/bin/bash
#run as: <script_name> data_format <bulk | linear> input_extension

format=$1
kind=$2
size=$3
# timing_dir=../timing
if [[ $format = dibbler_new && $kind = bulk ]]; then
    case $size in
        10001) alloc_hint=11000;;
	10MB) alloc_hint=50000;;
	20MB) alloc_hint=100000;;
	50MB) alloc_hint=250000;;
	100MB) alloc_hint=500000;;
    esac
fi
make PADS_KIND=$kind PADS_SIZE=$size PADS_ALLOC_HINT=$alloc_hint time_CaS_$format
#cat $timing_dir/"$format"_"$size"_"$kind".time
