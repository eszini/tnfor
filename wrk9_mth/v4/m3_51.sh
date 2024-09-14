#!/bin/bash

exec > log.txt 2>&1


> check1.txt
> check2.txt
> files_modified.txt

echo "check for files with these vars"
cat m3_41.sh
echo

cat m3_41.sh
sh m3_41.sh
echo

echo
wc -l check1.txt
echo 

echo "check for files with these vars"
cat m3_42.sh
echo

cat m3_41.sh
sh m3_41.sh
echo

echo
wc -l check2.txt
echo 

echo "files modified "
cat check1.txt | awk '{ print $1 }' | sort -u > files_modified.txt
cat files_modified.txt


echo
echo "files with duplicate vars "
cat check2.txt
echo "-----"

echo "end check"


