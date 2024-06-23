#!/bin/bash

parser_log="parser.err"
statis_log="tool.sta"

arc1="t4.for"
arc2="t5.for"

./tfor -v -opciones=d5 -tool=6 -inp="$arc1" -out="$arc2" -aux=p.err -log=p.log --chgini > log4

./tfor -exec=1 -inp="$arc1" -in2="$arc2" -out=d1.chr


echo "Cantidad de lineas y caracteres :"
for file in "$arc1"  "$arc2"; do
    echo "$(wc  < "$file") $file"
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





