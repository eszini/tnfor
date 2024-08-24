


# recorre todos los archivos
# busca variables vcb o vsb segun indicado
# hace los cambios en el lr1_ .... indicado
# graba en repo4
# resultado en m ...
#
#

# lr1_todos          todos los files
# lr1_solo_mth       solo los que tienen include mthnmcom.mon
# lr1_todos_sin_mth  todos los files, menos los que tienen include


./tfor -v -opciones=d5 -exec=6 -opt=1 -inp=lr1_todos  -in2=vcb.txt -in3=vsb.txt -dato=repo4 -out=m1 -aux=m2 -log=m3 

