#!/bin/bash

set -x


# Definir las variables programa1 y programa2
programa1="msgmmfa"
programa2=""
programa3=""

# Procesar programa1
cat check1.sh | grep "$programa1" > check11.sh
cat check1.sh | grep "$programa2" > check12.sh
cat check1.sh | grep "$programa3" > check13.sh

# Procesar programa2
cat check2.sh | grep "$programa1" > check21.sh
cat check2.sh | grep "$programa2" > check22.sh
cat check2.sh | grep "$programa3" > check23.sh

# Para cada par de checks (1 y 2):
# 1. Borrar archivos temporales check1.txt y check2.txt si existen
rm -f check1.txt check2.txt

# 2. Concatenar check11.sh y check12.sh en check1.txt
cat check11.sh check12.sh check13.sh > check1.txt

# 3. Concatenar check21.sh y check22.sh en check2.txt
cat check21.sh check22.sh check23.sh > check2.txt

# 4. Procesar check1.txt usando awk y sort para generar check31.sh
cat check1.txt | awk -F, '{ print $2 }' | sort -u > check31.sh

# 5. Procesar check2.txt usando awk y sort para generar check32.sh
cat check2.txt | awk -F, '{ print $2 }' | sort -u > check32.sh

echo "Proceso completado. Archivos generados: check31.sh y check32.sh"


# chequeo que variables c/b solo esten en archivos lr1_solo_mth
sh check31.sh > check31.txt
cat check31.txt | awk -F: '{ print $1 }' | sort -u > check41.txt

# este deberia dar vacio !!
sh check32.sh > check32.txt
cat check32.txt | awk -F: '{ print $1 }' | sort -u > check42.txt

