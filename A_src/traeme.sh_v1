#!/bin/bash

# Definir el directorio de origen
SOURCE_DIR=~/wrk/tnfor/A_src

# Archivo de configuración que contiene la lista de archivos a copiar
FILELIST=~/wrk/tnfor/A_src/filelist.txt

# Verificar si el archivo de configuración existe
if [ ! -f "$FILELIST" ]; then
    echo "Error: El archivo de configuración $FILELIST no existe."
    exit 1
fi

# Leer el archivo de configuración y copiar los archivos al directorio actual
while IFS= read -r file; do
    # Construir la ruta completa del archivo de origen
    src_file="$SOURCE_DIR/$file"
    
    # Verificar si el archivo de origen existe
    if [ -f "$src_file" ]; then
        echo "Copiando $src_file a $(pwd)"
        cp "$src_file" .
    else
        echo "Advertencia: $src_file no existe y no será copiado."
    fi
done < "$FILELIST"

