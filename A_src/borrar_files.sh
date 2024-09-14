#!/bin/bash

# Descripción:
# Este script busca y elimina recursivamente todos los archivos que
# comienzan con 'tfor.' en el directorio actual y sus subdirectorios.

# Opcional: Hacer una lista de los archivos que se eliminarán antes de borrarlos.
# Para activarlo, descomenta la línea correspondiente.

# Paso 1: Buscar los archivos que coinciden con el patrón 'tfor.*'
echo "Buscando archivos que comienzan con 'tfor.' en el directorio actual y subdirectorios..."

# Guardar la lista de archivos en una variable
files=$(find . -type f -name 'tfor.*')

# Verificar si se encontraron archivos
if [[ -z "$files" ]]; then
  echo "No se encontraron archivos que coincidan con el patrón 'tfor.*'."
  exit 0
fi

# Mostrar los archivos encontrados
echo "Archivos encontrados que serán eliminados:"
echo "$files"

# Paso 2: Confirmar la eliminación
read -p "¿Estás seguro de que deseas eliminar estos archivos? (s/n): " confirm

if [[ "$confirm" =~ ^[Ss]$ ]]; then
  # Eliminar los archivos
  find . -type f -name 'tfor.*' -exec rm -f {} +
  echo "Archivos eliminados exitosamente."
else
  echo "Operación cancelada. No se eliminaron archivos."
  exit 0
fi

