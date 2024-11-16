#!/bin/bash

# Archivo de entrada con las selecciones
m3_13="m3_13"
# Archivo de referencia donde hacer el grep
m0="m0"
# Archivo de salida final
output="wrk_vcb.txt"

# Elimina el archivo de salida si ya existe
> $output

# Itera sobre las líneas de m3_13 que están marcadas con "x"
while read -r line; do
  # Verifica si la línea está marcada con "x"
  if echo "$line" | grep -q '^ *x'; then
    # Extrae el nombre del programa que está en la cuarta columna
    prog=$(echo "$line" | awk '{ print $3 }')
    # Elimina la extensión del archivo
    prog="${prog%.*}"
    # Realiza grep del programa en m0 y guarda temporalmente en un archivo
    grep "$prog" "$m0" >> temp_matches.txt
  fi
done < "$m3_13"

# Extrae la seg col del archivo temp, elimina dups y genera la salida
awk -F',' '{ print $2 }' temp_matches.txt | sort -u > "$output"

# Elimina archivo temporal
rm temp_matches.txt

echo "Proceso completado. El archivo generado es: $output"




#x     1 coal_obj.for
#x     2 msgmmfa.f90
#x     2 servicac.for
#      5 GAS_objt.for
#      5 msgmmexprevcash.f90
#     25 tf_objt.for
#     35 msgmmsb6.for
#     65 msgmmadm.f90
#     86 msgmmout.for
#     91 msgmmdbt.for
#    161 namescom.mon
