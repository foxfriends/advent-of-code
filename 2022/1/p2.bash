#!/bin/bash

top1=0
top2=0
top3=0

current=0

while read -r line; do
    if [ -z "$line" ]; then
        if (( $current > $top1 )); then
            temp=$current
            current=$top1
            top1=$temp
        fi
        if (( $current > $top2 )); then
            temp=$current
            current=$top2
            top2=$temp
        fi
        if (( $current > $top3 )); then
            temp=$current
            current=$top3
            top3=$temp
        fi
        current=0
        continue
    fi
    current=$(($current+$line))
done

if (( $current > $top1 )); then
    temp=$current
    current=$top1
    top1=$temp
fi
if (( $current > $top2 )); then
    temp=$current
    current=$top2
    top2=$temp
fi
if (( $current > $top3 )); then
    temp=$current
    current=$top3
    top3=$temp
fi

echo $((top1 + top2 + top3))
