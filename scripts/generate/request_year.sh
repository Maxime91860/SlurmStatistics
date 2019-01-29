#!/bin/bash

YEAR=2018
declare -a MONTHS=(janvier fevrier mars avril mai juin juillet aout septembre octobre novembre decembre)
declare -a MONTHS_DIGIT=(01 02 03 04 05 06 07 08 09 10 11 12)
declare -a END_DAY_MONTHS=(31 28 31 30 31 30 31 31 30 31 30 31)

# sacct tuning 2018-12-01 -E 2018-12-31 >> 12_decembre2018.txt
OPTIONS="-P --units=G --allusers"
FIELDS="jobid%15,User%22,Account%15,Partition,AllocNodes,AllocCPUS,AllocGRES,reqmem,timelimit,elapsed,state,submit,start"

# Jan et Fevr manquent
for i in $(seq 2 11)
    do
    FILENAME=${MONTHS_DIGIT[${i}]}_${MONTHS[${i}]}.txt
    START=${YEAR}-${MONTHS_DIGIT[${i}]}-01
    END=${YEAR}-${MONTHS_DIGIT[${i}]}-${END_DAY_MONTHS[${i}]}

    echo "sacct ${OPTIONS} -o ${FIELDS} -S ${START} -E ${END} >> $FILENAME"
    sacct ${OPTIONS} -o ${FIELDS} -S ${START} -E ${END} >> $FILENAME
done
