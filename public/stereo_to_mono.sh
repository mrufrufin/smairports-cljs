#!/bin/bash

for file in *.mp3
do  
    fname="${file%.*}_mono.mp3"
    ffmpeg -i $file -ac 1 $fname
    echo $fname
done


