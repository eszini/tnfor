

Como trabajar con este punto


1) El mthnmcom.mon en src, debe estar "prolijo".
   sino, el resto no funciona

   primer paso, traerse el "copy"

   cp ~/wrk/tnfor/A_src/r0_copy.sh .
   sh r0_copy.sh 


2) correr
   sh r0_mthnmcom.sh

   esto:
 
   a) copia el mthnmcom.mon al directorio
  
   b) genera: 
   mth.mon  (mthnmcom.mon nuevo)
   vcb.txt  variables con blancos
   vcb2.txt variables con blancos, ordenados !!
   vsb.txt  variables sin blancos

   revisar si salieron bien vcb y vcb2 ....

   atencion!!
   es posible que haya que cambiar orden de las variables 
   en vcb.txt ... (ej las que tienen BTL ... al principio )

   opcion !!!
   en tfor.c .. cambiar sw1 a 1 y marca las vars en mth.mon directamente



3) correr 

   sh r8_crear ...  m

   crea repo con todos los programas a ultima version 
   ( m para usar ~/wrk/Midas )

   esto genera 
   lr1        ... lista de files
   lr1_todos  ... copia 

   de aqui hay que correr / construir los lr1...

4) correr

   sh r0_src_inc.sh

   esto genera

   lr1_solo_mth       aquellos src que inclu mthnmcom (23 a la fecha )
   lr1_todos_sin_mth  todos menos los que tienen el include


5) para hacer cambio en algunas variables y probar 


   correr 
   sh r0_prueba1.sh    (corre con lr1_solo_mth)

   esto genera 

   m0     tabla:   src  variable   (sort -u)
   m1     tabla:   variables (todas las utilizadas para todos los src)
                   tambien ya sort -u
   m2     tabla:   src #var_x_linea   linea_de_src  variable_encontrada

   m3     listado de todos los cambios de variables identificados en 
          los programas que figuran en lr1_solo_mth

   m5     files modificados ! son los que hay que recompilar en midas


   check1.sh  grep de variables sin blancos en codebase
   check2.sh  idem con var con blancos


   de aca, hay que generar resumen para ver cuantas var atacar

6) correr

   sh r0_gen_m3_13

   genera

   m3_11  lista de programa con var cambiada
   m3_13  resumen ordenado de variables por programa

   
5) definir las variablres de que programas vamos a modificar.

   identificar esas variables (ver el m3_13)

   x cada programa identificado "de interes" en m3_13 hacer


   grep progama1 m0 > m3_21
   grep progama2 m0 > m3_22   etc


   hacer grep nombre programa m3_11 > m3_21
   asi con varios a eleccion

   cat m3_21 m3_22 ... > m3_31

   cat m3_31 | awk -F, '{ print $2 }' | sort -u > m3_32


5) hacer prueba de cambio con las variables elegidas !


   en vcb2.txt ... 
   poner # como primer caracter en todas las lineas
   agregar al principio el m3_32 construido

   correr nuevamente:
   sh r0_prueba1.sh

   esto genera
   
   m0   los programas en los que aparecen las variables seleccionads
   m1   las variables (debe coincidir con lo que agregamos a vcb2 )
   m2   en que linea aparece cada variable en cada programa
   m3   salida de los cambios realizados !! (code review aca )
 
   check1.sh  lineas con grep, por cada src, por cada variable c/b
   check2.sh  lineas con grep, por cada src, por cada variable s/b
   
6) hacer chequeos 


   correr r0_armar_checks.sh

   esto genera:

   check41.txt 
   solo deberia haber src que esten en lr1_solo_mth
   si no es asi ... revisar conflicto !!!

   check42.txt 
   esto deberia quedar en cero 
   si no es asi ... revisar conflicto !!!
   esto implica que hay variables s/b que ya existen en otros
   programas aparte de los que estan en lr1_solo_mth
   y habra que modificar a mano las variables en los files 
   procesados.




     
   
