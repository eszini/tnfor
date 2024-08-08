#!/bin/bash

# Leer el archivo r_jres.txt y extraer la primera columna, eliminar duplicados y ordenar
awk '{print $1}' r_jres.txt | sort -u > z1_inc_j_temp

# Formatear el archivo z1_inc_j_temp para tener "   " (tres espacios) antes de cada archivo
awk '{print "   " $0}' z1_inc_j_temp > z1_inc_j

# Eliminar el archivo temporal
rm z1_inc_j_temp

echo "Archivo z1_inc_j creado exitosamente."

