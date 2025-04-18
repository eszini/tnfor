#!/bin/bash

# Directorios
DIR_REPO1="repo1"
DIR_REPO4="repo4"

# Iterar sobre cada archivo en repo1/
for file in "$DIR_REPO1"/*; do
    # Obtener solo el nombre del archivo sin la ruta completa
    filename=$(basename "$file")

    # Definir las variables arc1 y arc2
    arc1="$DIR_REPO1/$filename"
    arc2="$DIR_REPO4/$filename"

    # Construir la línea de comando (puedes modificarla después para probar distintos comandos)
    CMD="./tfor -v -opciones=d5 -debug=1 -tool=6 -inp=\"$arc1\" -out=\"$arc2\" -aux=p.err -log=p.log --chgalo > log2"

    # Imprimir el comando (opcional, útil para depuración)
    echo "$CMD"

    # Ejecutar el comando (descomenta la siguiente línea para ejecutarlo realmente)
    # eval "$CMD"
done

