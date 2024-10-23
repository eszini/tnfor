#!/bin/bash

set -x

# recorre todos los archivos
# graba en repo4
# resultado en prueba.txt  ...
#
# busca variables con blancos intermedios en todas las lineas
# de todos los fuentes.
#
#

# lr1_todos          todos los files

# Función para crear directorios si no existen
create_dir_if_not_exists() 
{
    local dir=$1
    if [ ! -d "$dir" ]; then
        echo "Creando directorio: $dir"
        mkdir -p "$dir"
    fi
}

create_dir_if_not_exists "repo4"

# ./tfor -v -opciones=d5 -exec=6 -opt=1 -inp=lr1_solo_mth  -in2=vcb2.txt -in3=vsb.txt -dato=repo4 -ou4=m0 -out=m1 -aux=m2 -log=m3 -ou5=check1.sh -ou6=check2.sh


./tfor -v -opciones=d5 -exec=8 -opt=3 -inp=lr1 -out=prueba.txt  -dato=repo4  > log





