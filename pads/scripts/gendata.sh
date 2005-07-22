#! /bin/bash
dataDir=$1
fmt=$2
dsize=$3

cd $dataDir
if [ $fmt = dns ]; then
    cat dns dns > dns.big
elif [ $fmt = dibbler_new ]; then
    (read line; cat > dibbler.body) < dibbler.10001
    head -n 1 dibbler.10001 | cat /dev/stdin dibbler.body dibbler.body > $fmt.$dsize
    rm -f dibbler.body
else
    echo "Unknown Format"
fi
