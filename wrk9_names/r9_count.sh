#!/bin/bash

# Verifica si se proporcionó un archivo como argumento
if [ -z "$1" ]; then
  echo "Por favor, proporciona un archivo como argumento."
  exit 1
fi

# Usa el comando uniq con la opción -c para contar las repeticiones de cada palabra
uniq -c "$1"

