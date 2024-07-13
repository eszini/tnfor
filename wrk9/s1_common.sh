#!/bin/bash

# Directorio del repositorio
REPO_DIR=~/wrk/Midas/ABBICAP/

# Archivo de lista de programas
PROGS_FILE="progs.txt"

# Archivo de salida
OUTPUT_FILE="output.txt"

# Limpiar archivo de salida si ya existe
> "$OUTPUT_FILE"

# Leer archivo de lista de programas
while IFS= read -r FILE; do
    # Verificar si el archivo existe en el repositorio
    if [ -f "$REPO_DIR$FILE" ]; then
        # Copiar el archivo al directorio de trabajo
        cp "$REPO_DIR$FILE" .

        # Contar las líneas de código
        LINE_COUNT=$(wc -l < "$FILE")

        # Contar los common blocks
        COMMON_BLOCK_COUNT=$(grep -ic "common" "$FILE")

        # Escribir detalles en el archivo de salida
        echo "$FILE, $LINE_COUNT, $COMMON_BLOCK_COUNT" >> "$OUTPUT_FILE"

        # Imprimir el nombre del archivo procesado
        echo "Procesado: $FILE"
    else
        echo "Archivo no encontrado: $REPO_DIR$FILE"
    fi
done < "$PROGS_FILE"

echo "Proceso completado. Detalles guardados en $OUTPUT_FILE"

