#!/bin/bash

clear_file="clear.cfg"
parser_log="parser.err"
statis_log="tool.sta"
checks_log="check.log"

arc1="t4.for"
arc2="t5.for"

# Borrar pantalla segun clear.cfg .... 
if [ -f "$clear_file" ]; then
    # Obtener la línea que contiene "clear" y extraer el valor después del "="
    clear_option=$(grep -i '^\s*clear\s*=' "$clear_file" | sed 's/^\s*clear\s*=\s*//i')
    
    # Quitar espacios en blanco alrededor
    clear_option=$(echo "$clear_option" | xargs)
    
    if [ "$clear_option" == "yes" ]; then
        clear
    fi
fi

./tfor -v -opciones=d5 -tool=6 -inp="$arc1" -out="$arc2" -aux=p.err -log=p.log --chgini > log4

./tfor -exec=1 -inp="$arc1" -in2="$arc2" -out=d1.chr


echo "Cantidad de lineas y caracteres :"
for file in "$arc1"  "$arc2"; do
    echo "$(wc  < "$file") $file"
done


echo
echo "Mapeo chars  file1   file2"
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

echo
if [ -e "$checks_log" ] && [ -s "$checks_log" ]; then
    echo "Check file:"
    cat "$checks_log"
else
    echo "No hay datos en check file"
fi





