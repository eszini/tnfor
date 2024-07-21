#!/bin/bash

# Archivos de entrada
archivo_dif="z2_dif_inc"
archivo_log="jp.log"

# Archivo de salida
archivo_result="z3_inc_src"

# Limpiar el archivo de salida si ya existe
> "$archivo_result"

# Leer el archivo z2_dif_inc y almacenar en un array las líneas con 'x' en la primera posición
declare -A includes_x
while IFS= read -r line; do
    if [[ $line == x* ]]; then
        include_file=$(echo "$line" | cut -c4-)
        includes_x["$include_file"]=1
    fi
done < "$archivo_dif"

# Leer el archivo jp.log y procesar cada línea
while IFS= read -r line; do
    source_file=$(echo "$line" | awk '{print $1}')
    include_file=$(echo "$line" | awk '{print $2}')
    if [[ ${includes_x["$include_file"]} ]]; then
        printf "%-15s%s\n" "$include_file" "$source_file" >> "$archivo_result"
    fi
done < "$archivo_log"

# Ordenar el archivo de salida
sort -u "$archivo_result" -o "$archivo_result"

echo "Archivo $archivo_result creado y ordenado exitosamente."

