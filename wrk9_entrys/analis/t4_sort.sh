

#!/bin/bash


# Archivo de entrada
archivo_entrada="z3_inc_src"

# Archivo de salida
archivo_salida="z4_src_inc"

# Limpiar el archivo de salida si ya existe
> "$archivo_salida"

# Leer el archivo de entrada y procesar cada línea
while IFS= read -r line; do
    include_file=$(echo "$line" | awk '{print $1}')
    source_file=$(echo "$line" | awk '{print $2}')
    printf "%-20s%s\n" "$source_file" "$include_file" >> "$archivo_salida"
done < "$archivo_entrada"

# Ordenar el archivo de salida de manera única
sort -u "$archivo_salida" -o "$archivo_salida"

echo "Archivo $archivo_salida creado y ordenado exitosamente."

