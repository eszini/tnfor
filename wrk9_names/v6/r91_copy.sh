#!/bin/bash

set -x  

#
# archivos a copiar para una iteracion
#

cp ~/wrk/tnfor/A_src/tfor.c .
cp ~/wrk/tnfor/A_src/Makefile .

make 

cp ~/wrk/tnfor/A_src/readme_pos.txt .

cp ~/wrk/tnfor/A_src/r91_copy.sh  .
cp ~/wrk/tnfor/A_src/r92_namescom.sh  .
cp ~/wrk/tnfor/A_src/r93_crear_repo.sh  .
cp ~/wrk/tnfor/A_src/r94_src_inc.sh  .
cp ~/wrk/tnfor/A_src/r95_prueba_nms1.sh  .
cp ~/wrk/tnfor/A_src/r96_gen_m3_13.sh  .
cp ~/wrk/tnfor/A_src/r97_cat_sort.sh  .
cp ~/wrk/tnfor/A_src/r98_armar_checks.sh  .



