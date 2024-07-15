#!/bin/bash

# Definir los directorios
REPO1=~/wrk/j/Midas/ABBICAP
REPO2=~/wrk/k/Midas/ABBICAP

# 1) Borrar y crear repo1/
if [ -d "repo1" ]; then
    rm -rf repo1
fi
mkdir repo1

# 2) Borrar y crear repo2/
if [ -d "repo2" ]; then
    rm -rf repo2
fi
mkdir repo2

# 3) Copiar archivos a repo1
cp $REPO1/*.f* $REPO1/*.F* $REPO1/*.mon $REPO1/*.MON repo1/ 2>/dev/null

# 4) Copiar archivos a repo2
cp $REPO2/*.f* $REPO2/*.F* $REPO2/*.mon $REPO2/*.MON repo2/ 2>/dev/null

# 5) Crear archivos lr1 y lr2
ls -1 repo1/*.* > lr1
ls -1 repo2/*.* > lr2

# 6) Borrar y crear repo3/ y repo4/
if [ -d "repo3" ]; then
    rm -rf repo3
fi
mkdir repo3

if [ -d "repo4" ]; then
    rm -rf repo4
fi
mkdir repo4

echo "Script ejecutado con éxito."

