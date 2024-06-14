#!/bin/bash


./tfor -v -opciones=d5 -tool=6 -inp=t4.for -out=t5.for -aux=p.err  --chgmas  > log

wc  t4.for t5.for 



# Verificar si el archivo tool.sta existe 
if [ -f "tool.sta" ]; then
    cat tool.sta
else
    echo "El archivo tool.sta no existe."
fi

