wc -l lr1
grep -i -v "\.mon" lr1 > lr_src
grep -i    "\.mon" lr1 > lr_mon
cat lr_src lr_mon > lr1
wc -l lr1

wc -l lrj
grep -i -v "\.mon" lrj > lr_src
grep -i    "\.mon" lrj > lr_mon
cat lr_src lr_mon > lrj
wc -l lrj

wc -l lrk
grep -i -v "\.mon" lrk > lr_src
grep -i    "\.mon" lrk > lr_mon
cat lr_src lr_mon > lrk
wc -l lrk


