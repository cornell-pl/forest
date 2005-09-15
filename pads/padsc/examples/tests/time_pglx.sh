#!/bin/bash
#run as: <script_name> data_format <bulk | linear | smart> input_extension

format=$1
kind=$2
size=$3
timing_dir=../timing
make PGLX_FORMAT=$format PGLX_KIND=$kind PGLX_SIZE=$size do_time_pglx
#cat $timing_dir/"$format"_"$size"_"$kind".time
