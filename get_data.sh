#!/bin/bash

threeDFile=$1
fileName=${threeDFile/.ply/}
textFile=$fileName.txt

######################################################

if [ ! -d "$fileName" ]; then
   mkdir $fileName
else
   :
fi

cd $fileName

cp ../$threeDFile $textFile
mv ../$threeDFile $threeDFile

#####################################################

grep -E 'ply|format|comment|element|property|end_header' $textFile > $fileName.header.txt

######################################################

numsVertex=$(grep element $textFile | grep vertex | cut -d " " -f 3)
numsFaces=$(grep element $textFile | grep face | cut -d " " -f 3)

grep property $textFile | grep -v list | cut -d " " -f 3 > tmp.txt
header=$(tr -d "[\000-\011\013-\020]" < tmp.txt | awk ' BEGIN { ORS="\t" } { print }')

grep -v ply $textFile | grep -v format | grep -v element | grep -v property | grep -v comment  | grep -v end_header > elements.txt

sed -n -e "1,$numsVertex p" -e "$numsVertex q" elements.txt | sed 's/ /	/g' > $fileName.xyz.txt

echo $header | cat - $fileName.xyz.txt > temp && mv temp $fileName.xyz.txt

sed 's/ /	/g' $fileName.xyz.txt | sed '/^\s*$/d' > $fileName.vertex.txt

######################################################

sed -n -e "$numsVertex,$(cat elements.txt | wc -l) p" -e "$(cat elements.txt | wc -l) q" elements.txt | sed 's/ /        /g' | sed '1d' | sed '/^\s*$/d' > $fileName.faces.txt

######################################################

rm $fileName.xyz.txt
rm tmp.txt
rm elements.txt


