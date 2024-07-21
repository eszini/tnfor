#!/bin/bash

# Directorio de origen
source_dir=~/wrk/Midas/ABBICAP

# Directorio de destino
destination_dir=repo1

# Crear el directorio de destino si no existe
if [ ! -d "$destination_dir" ]; then
    mkdir "$destination_dir"
fi

# Copiar los archivos con las extensiones especificadas
cp $source_dir/*.for $source_dir/*.FOR $source_dir/*.f90 $source_dir/*.F90 $source_dir/*.f95 $source_dir/*.F95 "$destination_dir" 2>/dev/null

# Hacer un listado de los archivos copiados y guardarlo en el archivo "lr1"
ls -1 "$destination_dir"/*.* > lr1



