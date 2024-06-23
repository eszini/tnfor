#!/bin/bash

# Definir el directorio raíz desde el cual empezar
root_dir="$HOME/wrk/tnfor"

# Recorrer el árbol de directorios y eliminar los archivos .exe
find "$root_dir" -type f -name "*.exe" -exec echo "Eliminando: {}" \; -exec rm -f {} \;
find "$root_dir" -type f -name "*.log" -exec echo "Eliminando: {}" \; -exec rm -f {} \;
find "$root_dir" -type f -name "log*" -exec echo "Eliminando: {}" \; -exec rm -f {} \;
find "$root_dir" -type f -name "*.chr*" -exec echo "Eliminando: {}" \; -exec rm -f {} \;
find "$root_dir" -type f -name "*.err*" -exec echo "Eliminando: {}" \; -exec rm -f {} \;
find "$root_dir" -type f -name "*.sta*" -exec echo "Eliminando: {}" \; -exec rm -f {} \;

echo "Todos los archivos .exe han sido eliminados desde $root_dir."


echo "tfor en .. "
find . -print | grep -i 'tfor\.c'


