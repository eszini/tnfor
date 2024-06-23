#!/bin/bash


./tfor -v -opciones=d5 -tool=6 -inp=t5.for -out=t6.for -aux=p.err  --chglco  > log5

wc  t5.for t6.for 



# Verificar si el archivo tool.sta existe 
if [ -f "tool.sta" ]; then
    cat tool.sta
else
    echo "El archivo tool.sta no existe."
fi

