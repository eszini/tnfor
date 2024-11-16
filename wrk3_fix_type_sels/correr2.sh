


./tfor -v -opciones=d5 -tool=4 -inp=t1.for -out=t2.for -aux=tparser.err  > log

wc  t1.for t2.for 

grep -i integer t1.for > t1i.log
grep -i integer t2.for > t2i.log

grep -i logical t1.for > t1l.log
grep -i logical t2.for > t2l.log

grep -i real t1.for > t1r.log
grep -i real t2.for > t2r.log

grep -i character t1.for > t1c.log
grep -i character t2.for > t2c.log






./tfor -v -opciones=d5 -tool=4 -inp=u1.for -out=u2.for -aux=uparser.err  > log

wc  u1.for u2.for 



grep -i integer u1.for > u1i.log
grep -i integer u2.for > u2i.log

grep -i logical u1.for > u1l.log
grep -i logical u2.for > u2l.log

grep -i real u1.for > u1r.log
grep -i real u2.for > u2r.log

grep -i character u1.for > u1c.log
grep -i character u2.for > u2c.log

