#!/bin/bash

# Archivos de entrada
archivo_j="z1_inc_j"
archivo_k="z1_inc_k"

# Archivo de salida
archivo_dif="z2_dif_inc"

# Leer el archivo z1_inc_j y procesar cada línea
while read -r line; do
    # Eliminar los espacios iniciales y obtener el nombre del archivo include
    include_file=$(echo "$line" | sed 's/^[[:space:]]*//')
    
    # Verificar si el include file está en z1_inc_k
    if grep -Fxq "   $include_file" "$archivo_k"; then
        echo "   $include_file" >> "$archivo_dif"
    else
        echo "x  $include_file" >> "$archivo_dif"
    fi
done < "$archivo_j"

echo "Archivo $archivo_dif creado exitosamente."

