

# toma el namescom.mon, y genera una lista de las 
# variables que tienen blancos intermedios !
#

./tfor -v -opciones=d5 -exec=5 -inp=namescom.mon -out=names.mon -ou2=vcb.txt -log=vsb.txt  > log






# cosas que hubo que cambiar en el namescom.mon original
# para que el programa lo deje razonable
# 
# 
# 1) hay una una con un + suelto
# 2) hay una continuacion de linea con +
# 3) hay una variable payment_vars que tiene el parameter aparte
# 4) hay variables largas que empiezan directo despues del ::variable
# 5) sacar todos los blancos adelante y despues del ' = '
# 
