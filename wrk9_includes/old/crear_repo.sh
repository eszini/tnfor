#!/bin/bash

# Directorio de origen
source_dir=~/wrk/Midas/ABBICAP

# Directorio de destino
destination_dir=repo1

# Crear el directorio de destino si no existe
mkdir -p $destination_dir

# Copiar archivos con las extensiones especificadas
cp $source_dir/*.[Ff][Oo][Rr] $destination_dir 2>/dev/null
cp $source_dir/*.[Ff]90 $destination_dir 2>/dev/null
cp $source_dir/*.[Ff]95 $destination_dir 2>/dev/null

# Listar los archivos en el directorio de destino y guardar el listado en 'lr1'
ls -1 $destination_dir/*.* > lr1



