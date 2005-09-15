#!/bin/bash
#run as: <script_name> data_format <bulk | linear> input_extension

format=$1
kind=$2
size=$3
# timing_dir=../timing
make PADS_FORMAT=$format PADS_KIND=$kind PADS_SIZE=$size do_time_pads
#cat $timing_dir/"$format"_"$size"_"$kind".time
