

ls -1 ~/wrk/Midas/ABBICAP > list_src

wc -l list_src


cp ~/wrk/Midas/ABBICAPOBJ73/Makefile makefile_in_use

wc -l makefile_in_use





#
#
#	-tool=2
#	-inp=makefile_in_use		es el makefile en uso en el repo
#	-out=m_file_lst			es el listado de archivos que se encuentran en el make
#
#
#


./tfor -v -opciones=d5 -tool=2 -inp=makefile_in_use -out=m_file_lst  > log


#
#
#	-tool=1			compara dos listas de archivos, y dice cual esta/no esta
#	-inp=m_file_lst		lista de archivos que estan en el makefile
#	-in2=list_src		listado de files que estan en el directorio build
#	-out=l_si		archivos en list_src que SI estan en m_file_lst
#	-ou2=l_no		archivos en list_src que NO estan en m_file_lst
# 	-m			las comparaciones son pasando todo a minusculas !!!
#


./tfor -v -opciones=d5 -m -tool=1 -inp=m_file_lst -in2=list_src -out=l_si -ou2=l_no  > log



wc -l makefile_in_use

wc -l list_src

wc -l l_si l_no

wc -l m_file_lst

