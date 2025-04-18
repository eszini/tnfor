#!/bin/bash

# Leer el archivo jp.out y extraer la primera columna, eliminar duplicados y ordenar
awk '{print $1}' r_kres.txt | sort -u > z1_inc_k_temp

# Formatear el archivo z1_inc_k_temp para tener "   " (tres espacios) antes de cada archivo
awk '{print "   " $0}' z1_inc_k_temp > z1_inc_k

# Eliminar el archivo temporal
rm z1_inc_k_temp

echo "Archivo z1_inc_k creado exitosamente."

