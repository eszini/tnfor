#!/bin/bash

parser_log="parser.err"
statis_log="tool.sta"

ejecutar_greps=false

arc1="t2.for"
arc2="t3.for"

./tfor -v -opciones=d5 -tool=6 -inp="$arc1" -out="$arc2" --chgtyp  > log2

echo "Cantidad de lineas en cada file:"
for file in "$arc1"  "$arc2"; do
    echo "$(wc -l < "$file") $file"
done

if $ejecutar_greps; then
  grep -i integer t2.for > t2i.log
  grep -i integer t3.for > t3i.log
  
  grep -i logical t2.for > t2l.log
  grep -i logical t3.for > t3l.log
  
  grep -i real t2.for > t2r.log
  grep -i real t3.for > t3r.log
  
  grep -i character t2.for > t2c.log
  grep -i character t3.for > t3c.log
fi


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





