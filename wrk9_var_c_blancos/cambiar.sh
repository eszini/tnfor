#!/bin/bash

# Archivo que contiene la lista de archivos a modificar
FILE_LIST="progs.txt"

# Archivo de registro de errores
ERROR_LOG="errores.log"

# Limpiar archivo de errores anterior si existe
> $ERROR_LOG

# Leer archivo de lista de archivos línea por línea
while IFS= read -r file; do
    if [[ -f $file ]]; then
        echo "Procesando archivo: $file"
        # Usar sed para realizar la sustitución
        sed -i 's/clear.txt/clear.cfg/g' "$file"
        
        if [[ $? -ne 0 ]]; then
            echo "Error al procesar archivo: $file" | tee -a $ERROR_LOG
        else
            echo "Sustitución realizada correctamente en: $file"
        fi
    else
        echo "Archivo no encontrado: $file" | tee -a $ERROR_LOG
    fi
done < "$FILE_LIST"

echo "Script completado."

