#!/bin/bash

max=0
current=0

while read -r line; do
    if [ -z "$line" ]; then
        current=0
        continue
    fi
    current=$(($current+$line))
    if (( $current > $max )); then
        max=$current
    fi
done

echo $max
