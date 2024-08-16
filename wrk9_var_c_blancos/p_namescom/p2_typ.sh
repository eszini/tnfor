#!/bin/bash

clear_file="clear.cfg"
parser_log="parser.err"
statis_log="tool.sta"
checks_log="check.log"

ejecutar_greps=false

arc1="t2.for"
arc2="t3.for"

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



./tfor -v -opciones=d5 -tool=6 -inp="$arc1" -out="$arc2" --chgtyp     > log2

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

echo
if [ -e "$checks_log" ] && [ -s "$checks_log" ]; then
    echo "Check file:"
    cat "$checks_log"
else
    echo "No hay datos en check file"
fi






echo
if [ -e "$arc2" ] ; then
    echo "cp t3.for a t4.for por si quiere saltar el paso p3_mas.sh"
    cp t3.for t4.for
else
    echo "No se genero t3.for !!"
fi



