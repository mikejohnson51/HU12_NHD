#!/bin/bash
for i in *.png
do
    convert $prevFile $i -composite ./stacked/$i
    prevFile=./stacked/$i
done	