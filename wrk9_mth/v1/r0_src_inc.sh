#!/bin/bash

# Define los archivos de entrada y salida
archivo_lr1="lr1"
archivo_todos="lr1_todos"
archivo_solo_mth="lr1_solo_mth"
archivo_todos_sin_mth="lr1_todos_sin_mth"

# crear el archivo lr1_solo_mth
grep -i include ~/wrk/Midas/ABBICAP/*.* | grep -i mthnmcom | awk -F/ '{print $NF}' | awk -F: '{print $1}' | sort -u > lr1_solo_mth

sed -i 's/^/repo1\//' lr1_solo_mth


# saco mthnmcom de todos
cat "$archivo_lr1" | grep -v mthnmcom  > "$archivo_todos"

[ -d repo4 ] && rm -rf repo4
mkdir repo4


# Crear el archivo de salida vacío
> "$archivo_todos_sin_mth"

# Leer el archivo "lr1_solo_mth" y almacenar los nombres en un array
mapfile -t solo_mth < "$archivo_solo_mth"

# Leer cada línea del archivo "lr1_todos"
while IFS= read -r line; do
    # Extraer el nombre del archivo (sin el "repo1/")
    archivo=$(basename "$line")
    
    # Verificar si el archivo no está en el array de "solo_mth"
    if [[ ! " ${solo_mth[@]} " =~ " ${archivo} " ]]; then
        # Si no está, agregar la línea al archivo de salida
        echo "$line" >> "$archivo_todos_sin_mth"
    fi
done < "$archivo_todos"

