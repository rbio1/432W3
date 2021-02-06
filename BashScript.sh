#!/bin/bash

for Count in {1..20} 
do 
if [ $Count -lt 10 ] 
then
 echo $Count is a single-digit number
else
 echo $Count is a double-digit number
fi
done
