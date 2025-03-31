#!/bin/bash

for i in {1..16}
do
    num=$(printf "%02d" $i)
    plan_count=$(grep "($num)" plan.txt | wc -l)
    corregir_count=$(grep "($num)" corregir.txt | wc -l)
    printf "plan______(%02d)    %d\n" $i $plan_count
    printf "corregir__(%02d)    %d\n" $i $corregir_count
    echo " "
done

