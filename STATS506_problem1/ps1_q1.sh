#!/bin/bash

###
#title: question1
#author: jingxian chen
#date: Sep 13th, 2019
###


## a)
FILE="/afs/umich.edu/user/j/i/jingxian/506/problem1/recs2015_public_v4.csv"
if test -f "$FILE"; then
	echo "recs2015_public_v4.csv already exist"
else wget -O $FILE  https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv
fi
#---------------------------------------------------------------

## b)
# the following code block deletes recs_names.txt if the file already exists.
FILEb="/afs/umich.edu/user/j/i/jingxian/506/problem1/recs_names.txt"
if test -f "$FILEb"; then
	rm $FILEb
	echo "recs_names.txt already exists, and we now delete the old one."
fi

# the one-liner command:
head -1 $FILE | sed 's/,/\n/g' > $FILEb
#--------------------------------------------------------------

## c)
grep -n -Eo 'DOEID|^BRR' $FILEb | cut -f1 -d":" | paste -s -d,
#--------------------------------------------------------------

## d)
# construct cols variable:
declare "cols=$(grep -n -Eo 'DOEID|^BRR' $FILEb | cut -f1 -d":" | paste -s -d,)"
# the one-liner command:
cut -f$cols -d,  ~/506/problem1/recs2015_public_v4.csv > ~/506/problem1/recs_weights.csv
