#
#
#	-tool=1			compara dos listas de archivos, y dice cual esta/no esta
#	-inp=m_file_lst		lista de archivos que estan en el makefile
#	-in2=list_src		listado de files que estan en el directorio build
#	-out=l_si		archivos en list_src que SI estan en m_file_lst
#	-ou2=l_no		archivos en list_src que NO estan en m_file_lst
# 	-m			las comparaciones son pasando todo a minusculas !!!
#


./tfor -v -opciones=d5 -m -tool=1 -inp=makefile_in_use -in2=list_src -out=l_si -ou2=l_no  > log
