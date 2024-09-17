#!/bin/bash

set -x  

# toma el namescom.mon, y genera una lista de las 
# variables que tienen blancos intermedios !
#

# Verifica si el archivo namescom.mon no existe en el directorio actual
if [ ! -f ./namescom.mon ]; then
    # Si no existe, lo copia desde ~/wrk/Midas/ABBICAP/
    cp ~/wrk/Midas/ABBICAP/namescom.mon .
    echo "Archivo namescom.mon copiado al directorio actual."
fi

./tfor -v -opciones=d5 -exec=5 -inp=namescom.mon -out=nms.mon -ou2=vcb.txt -ou3=vcb2.txt -ou4=que  -log=vsb.txt  > log



# cosas que hubo que cambiar en el namescom.mon original
# para que el programa lo deje razonable
# 
# 
# 1) hay una una con un + suelto
# 2) hay una continuacion de linea con +
# 3) hay una variable payment_vars que tiene el parameter aparte
# 4) hay variables largas que empiezan directo despues del ::variable
# 5) sacar todos los blancos adelante y despues del ' = '
# 

# nms.mon     nueva version de nam ... solo sirve si alguien cambio formatos etc
# vcb.txt     lista de variables que tienen blancos intermedios
# vcb2.txt    misma lista, pero ordenada para evitar los errores de busqueda 
# vsb.txt     lista de variables sin blancos intermedios





