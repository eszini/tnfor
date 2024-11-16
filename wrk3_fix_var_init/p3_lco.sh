#!/bin/bash


./tfor -v -opciones=d5 -tool=6 -inp=t3.for -out=t4.for -aux=p.err  --chglco  > log

wc  t3.for t4.for 



# Verificar si el archivo tool.sta existe 
if [ -f "tool.sta" ]; then
    cat tool.sta
else
    echo "El archivo tool.sta no existe."
fi

