

./tfor -v -opciones=d5 -tool=6 -inp=t2.for -out=t3.for --chgtyp  > log2

wc  t2.for t3.for 

grep -i integer t2.for > t2i.log
grep -i integer t3.for > t3i.log

grep -i logical t2.for > t2l.log
grep -i logical t3.for > t3l.log

grep -i real t2.for > t2r.log
grep -i real t3.for > t3r.log

grep -i character t2.for > t2c.log
grep -i character t3.for > t3c.log



