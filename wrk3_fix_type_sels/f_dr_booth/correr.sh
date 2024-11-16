


./tfor -v -opciones=d5 -tool=6 -inp=t1.for -out=t2.for -aux=p.err --chgcom --chgtyp  > log

wc  t1.for t2.for 

grep -i integer     t1.for > t1i.log
grep -i integer     t2.for > t2i.log

grep -i logical     t1.for > t1l.log
grep -i logical     t2.for > t2l.log

grep -i real        t1.for > t1r.log
grep -i real        t2.for > t2r.log

grep -i character   t1.for > t1c.log
grep -i character   t2.for > t2c.log



