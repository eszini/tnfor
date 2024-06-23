#!/bin/bash


parser_log="parser.err"
statis_log="tool.sta"

arc1="t1.for"
arc2="t2.for"



./tfor -v -opciones=d5 -tool=6 -t -inp="$arc1" -out="$arc2" --chgcomm  > log1

./tfor -exec=1 -inp="$arc1" -in2="$arc2" -out=d1.chr



echo "Cantidad de lineas"
for file in "$arc1"  "$arc2"; do
    echo "$(wc -l < "$file") $file"
done


echo
grep ^X d1.chr

echo
if [ -e "$parser_log" ] && [ -s "$parser_log" ]; then
    echo "Log de parser:"
    cat "$parser_log"
else
    echo "No hay errores en parser."
fi

echo
if [ -e "$statis_log" ] && [ -s "$statis_log" ]; then
    echo "Estadisticas:"
    cat "$statis_log"
else
    echo "No hay estadisticas"
fi





