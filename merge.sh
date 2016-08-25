#!/bin/bash

threeDFile1=$1
threeDFile2=$2
finalName=$3

fileName1=${threeDFile1/.ply/}
fileName2=${threeDFile2/.ply/}
name_finale=${finalName/.ply/.txt}

if [ ! -d $fileName1 ]; then
./get_data.sh $threeDFile1
else
:
fi

if [ ! -d $fileName2 ]; then
./get_data.sh $threeDFile2
else
:
fi

while [ ! -d $fileName1 ]
do
:
done

while [ ! -d $fileName2 ]
do
:
done

R --no-save --no-restore --quiet --args $fileName1/$fileName1.vertex.txt $fileName2/$fileName2.vertex.txt $name_finale < matching.R > $name_finale.log 2>&1


while [ ! -f $name_finale ]
do
:
done

./clean.sh $name_finale
