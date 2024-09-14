#!/bin/bash

# Recorrer todos los directorios en el directorio actual
for dir in */; do
  # Eliminar la barra final del nombre del directorio
  dir=${dir%/}

  # Si el nombre del directorio es "build" seguido de un solo dígito (0-9)
  if [[ $dir =~ ^build[0-9]$ ]]; then
    # Extraer el dígito
    num=${dir:5}

    # Renombrar el directorio a "build0N"
    mv "$dir" "build0$num"
    echo "Renombrado $dir a build0$num"
  fi
done

