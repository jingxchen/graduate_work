#!/bin/bash

###
#title: question 2
#author: jingxian chen
#date: Sep 14th, 2019
###

#def parameters:
file=$1
expression=$2

declare cols1=$(head -1 ${file} | sed 's/,/\n/g'| grep -n -E ${expression} | cut -f1 -d":" | paste -s -d,)
declare cols2=$(head -1 ${file} | sed 's/,/\n/g'| grep -n ${expression} | cut -f1 -d":" | paste -s -d,)
c1="echo $cols1"
c2="echo $cols2"
if [ "$c1" ">" "$c2" ]; then
cut -f$cols1 -d"," ${file};
else 
cut -f$cols2 -d"," ${file}; 
fi 
