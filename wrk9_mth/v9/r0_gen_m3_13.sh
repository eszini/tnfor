#!/bin/bash

# Paso 1: Copiar m3 a m3_11
cp m3 m3_11

# Paso 2: Eliminar líneas vacías y luego eliminar todas las líneas pares en m3_11
sed -i '/^$/d' m3_11
sed -i 'n;d' m3_11

# Paso 3: Extraer la primera columna de m3_11 y guardarla en m3_12
awk '{ print $1 }' m3_11 > m3_12

# Paso 4: Ejecutar r0_count.sh en m3_12 y ordenar la salida en m3_13
uniq -c m3_12 | sort > m3_13


