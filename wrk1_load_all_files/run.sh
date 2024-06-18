#!/bin/bash

# En todos los programas en lr1 ...
# Cambiar inicializaciones del tipo   var/xxxxx/  a var = xxxxx
# Grabar resultado en repo2

./tfor -v -opciones=d5 -prue=2 -inp=lr1 -out=r.txt -dato=repo2  -log=p.log > log
