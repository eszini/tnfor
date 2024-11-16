#!/bin/bash

set -x  

#
# archivos a copiar para una iteracion
#

cp ~/wrk/tnfor/A_src/tfor.c .
cp ~/wrk/tnfor/A_src/Makefile .

make 

cp ~/wrk/tnfor/A_src/r0_namescom.sh .
cp ~/wrk/tnfor/A_src/r8_crear_repo.sh .
cp ~/wrk/tnfor/A_src/r0_src_inc_nms.sh .
cp ~/wrk/tnfor/A_src/r0_prueba_nms1.sh  .
cp ~/wrk/tnfor/A_src/r0_prueba_nms2.sh  .
cp ~/wrk/tnfor/A_src/r0_gen_m3_13.sh .
cp ~/wrk/tnfor/A_src/r0_grep_vars.sh .
cp ~/wrk/tnfor/A_src/r0_armar_checks.sh  .


