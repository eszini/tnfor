#!/bin/bash

parser_log="parser.err"

./tfor -v -opciones=d5 -tool=6 -inp=t5.for -out=t6.for -aux=p.err -log=p.log --chgini > log

./tfor -exec=1 -inp=t5.for -in2=t6.for -out=d1.chr


wc  t5.for t6.for 



wc -l t5.for t6.for

grep ^X d1.chr


if [ -e "$archivo" ]; then
  if [ ! -s "$archivo" ]; then
    echo "parser ok"
  else
    echo "at! chars no detectardos por parser "
  fi
else
  echo "no hay log de parser"
fi



