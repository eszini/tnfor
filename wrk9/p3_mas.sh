#!/bin/bash

parser_log="parser.err"
statis_log="tool.sta"

arc1="t3.for"
arc2="t4.for"

./tfor -v -opciones=d5 -tool=6 -inp="$arc1" -out="$arc2" -aux=p.err  --chgmas  > log3

echo "Cantidad de lineas en cada file:"
for file in "$arc1"  "$arc2"; do
    echo "$(wc -l < "$file") $file"
done




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





