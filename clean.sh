#!/bin/bash

ply=${1/.txt/.ply}

cut -d " " -f 2,3,4 $1 | sed '1d' > tmp.txt

num_rows=$(wc -l tmp.txt)
num_Rows=${num_rows/ tmp.txt/}

echo "ply" > cabeza.txt
echo "format ascii 1.0" >> cabeza.txt
echo "comment merged by NaredoKitsch" >> cabeza.txt
echo "element vertex $num_Rows" >> cabeza.txt
sed 's/    / /g' cabeza.txt > cabeza2.txt
echo "property float x" >> cabeza2.txt
echo "property float y" >> cabeza2.txt
echo "property float z" >> cabeza2.txt
echo "end_header" >> cabeza2.txt
cat tmp.txt >> cabeza2.txt

cp cabeza2.txt $ply


rm tmp.txt
rm cabeza.txt
rm cabeza2.txt

open $ply

rm $1
