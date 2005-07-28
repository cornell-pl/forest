#! /bin/bash

dataDir=$1
fmt=$2

# target file size for generated data
targetSize=$3

# file name for generated data. outname must be one word.
targetName=$4

function getFifth { echo $5; }
function genList { 
    local count=$1
    for ((i=0; i < count; i++)); do echo $2; done 
}
function getCount { 
    local size=$(getFifth $(ls -l $1))
    echo $(( targetSize/size ))
}

cd $dataDir
rm -f $targetName
if [ $fmt = dibbler ]; then
    (read line; cat > dibbler.body) < dibbler.10001
    head -n 1 dibbler.10001 > $targetName 
    count=$(getCount dibbler.body)  
    if [ $count = 0 ]; then 
	echo "error target size $targetSize is smaller than file size."
	exit 1
    fi
    cat $(genList $count dibbler.body) >> $targetName
    rm -f dibbler.body
elif [ -e $fmt ]; then
    count=$(getCount $fmt)  
    if [ $count = 0 ]; then 
	echo "error: target size $targetSize is smaller than size of file $fmt."
	exit 1
    fi
    cat $(genList $count $fmt) > $targetName
else
    echo "error: unknown format $fmt"
fi
