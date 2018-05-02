#!/bin/bash

export H=`pwd`
export SCALA_HOME=$H/`ls -d scala*`
export PATH=$SCALA_HOME/bin:$PATH
export S=$H/solutions

mkdir -p $S

for i in `ls -r data`;
do
    echo Testing $i
    pushd .
    cd out/production/2018-04-27-Tuenti-Challenge
    DATA_DIR=$H/data/$i
    SOLUTION_DIR=${S}/$i
    mkdir -p $SOLUTION_DIR
    for j in `ls $DATA_DIR`;
    do
        echo $j
        scala $i < $DATA_DIR/$j | tee $SOLUTION_DIR/$j
    done
    popd
done
