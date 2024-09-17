#!/bin/bash

set -x  

# toma el mthnmcom.mon, y genera una lista de las 
# variables que tienen blancos intermedios !
#

# Verifica si el archivo mthnmcom.mon no existe en el directorio actual
if [ ! -f ./mthnmcom.mon ]; then
    # Si no existe, lo copia desde ~/wrk/Midas/ABBICAP/
    cp ~/wrk/Midas/ABBICAP/mthnmcom.mon .
    echo "Archivo mthnmcom.mon copiado al directorio actual."
fi

./tfor -v -opciones=d5 -exec=5 -inp=mthnmcom.mon -out=mth.mon -ou2=vcb.txt -ou3=vcb2.txt -ou4=que  -log=vsb.txt  > log



# cosas que hubo que cambiar en el mthnmcom.mon original
# para que el programa lo deje razonable
# 
# 
# 1) hay una una con un + suelto
# 2) hay una continuacion de linea con +
# 3) hay una variable payment_vars que tiene el parameter aparte
# 4) hay variables largas que empiezan directo despues del ::variable
# 5) sacar todos los blancos adelante y despues del ' = '
# 

# mth.mon     nueva version de mth ... solo sirve si alguien cambio formatos etc
# vcb.txt     lista de variables que tienen blancos intermedios
# vcb2.txt    misma lista, pero ordenada para evitar los errores de busqueda 
# vsb.txt     lista de variables sin blancos intermedios





