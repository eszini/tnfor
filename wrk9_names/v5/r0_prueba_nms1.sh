
set -x

# recorre todos los archivos
# busca variables vcb o vsb segun indicado
# hace los cambios en el lr1_ .... indicado
# graba en repo4
# resultado en m ...
#
#

# lr1_todos          todos los files
# lr1_solo_nms       solo los que tienen include namnmcom.mon
# lr1_todos_sin_nms  todos los files, menos los que tienen include


./tfor -v -opciones=d5 -exec=6 -opt=1 -inp=lr1_solo_nms  -in2=vcb2.txt -in3=vsb.txt -dato=repo4 -ou4=m0 -out=m1 -aux=m2 -log=m3 -ou5=check1.sh -ou6=check2.sh


cat m0 | awk '{ print $1 }' | sort -u > m5
cp m5 files_a_modificar.txt
echo "archivos a modificar "
cat m5
