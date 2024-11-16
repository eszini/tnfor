#!/bin/bash

# Define los archivos de entrada y salida
archivo_lr1="lr1"
archivo_todos="lr1_todos"
archivo_solo_nms="lr1_solo_nms"
archivo_todos_sin_nms="lr1_todos_sin_nms"

# crear el archivo lr1_solo_nms
grep -i use ~/wrk/Midas/ABBICAP/*.* | grep -i namescom | awk -F/ '{print $NF}' | awk -F: '{print $1}' | sort -u > lr1_solo_nms



# saco namescom de todos
# cat "$archivo_lr1" | grep -v namescom  > "$archivo_todos"

[ -d repo4 ] && rm -rf repo4
mkdir repo4


# Crear el archivo de salida vacío
> "$archivo_todos_sin_nms"

# Leer el archivo "lr1_solo_nms" y almacenar los nombres en un array
mapfile -t solo_nms < "$archivo_solo_nms"

# Leer cada línea del archivo "lr1_todos"
while IFS= read -r line; do
    # Extraer el nombre del archivo (sin el "repo1/")
    archivo=$(basename "$line")
    
    # Verificar si el archivo no está en el array de "solo_nms"
    if [[ ! " ${solo_nms[@]} " =~ " ${archivo} " ]]; then
        # Si no está, agregar la línea al archivo de salida
        echo "$line" >> "$archivo_todos_sin_nms"
    fi
done < "$archivo_todos"

# agrego repo... a lr1_solo_nms
sed -i 's/^/repo1\//' lr1_solo_nms

echo "repo1/namescom.f90" >> lr1_solo_nms


