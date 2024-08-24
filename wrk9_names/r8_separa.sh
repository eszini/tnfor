

egrep -i -E '\.mon' lr1 > lr1_mon
egrep -i -E '\.for' lr1 > lr1_for
egrep -i -E '\.f9'  lr1 > lr1_f9


cat lr1_for lr1_f9 lr1_mon > lr2


wc -l lr1
wc -l lr2

cp lr2 lr1

