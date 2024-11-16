


./tfor -v -opciones=d5 -tool=6 -inp=t1.for -out=t2.for -aux=p.err --chgcom --chgtyp --chglco  > log

wc  t1.for t2.for 


grep -i integer t1.for > t1i.log
grep -i integer t2.for > t2i.log

grep -i real    t1.for > t1r.log
grep -i real    t2.for > t2r.log


