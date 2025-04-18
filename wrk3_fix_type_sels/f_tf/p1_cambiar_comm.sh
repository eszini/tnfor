

parser_log="parser.err"


./tfor -v -opciones=d5 -tool=6 -t -inp=t1.for -out=t2.for --chgcomm  > log1


./tfor -exec=1 -inp=t1.for -in2=t2.for -out=d1.chr

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

