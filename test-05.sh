#!/bin/bash

X=`mktemp`
rm $X

function finish {
   rm -rf $X 
   popd
}

pushd .

trap finish EXIT

cd out/production/* 
mkfifo $X
for i in TEST SUBMIT;
do
    echo ""
    echo Phase $i
    nc 52.49.91.111 3241 < $X | ../../../scala-2.12.5/bin/scala -J-Xmx2g Challenge05DNASplicer $i > $X
done

