# 
# lee todo el codebase, y genera archivo con todos los allocates detectados
# genera una linea con el tipo de allocate, las siglas sl ml sv mv etc
#

./tfor -v -opciones=d5i1 -debug=0 -zap=0 -exec=9 -opt=1 -inp=lr1 -dato=repo4 -out=list_allocate.txt -ou2=zfor.txt -ou3=zf90.txt -ou4=plan.txt -aux=lfor.txt -log=lf90.txt -ou5=mapa.txt -ou6=error.txt -dbg=debug.lst

