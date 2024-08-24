

Como trabajar con este punto


1) hay que traerse el mthnmcom.mon del src
   dejarlo prolijo (si no, no funciona bien el programa)


2) correr
   sh r0_mthnmcom.sh

   esto genera:
   
   mth.mon  (mthnmcom.mon nuevo)
   vcb.txt  variables con blancos
   vsb.txt  variables sin blancos

   opcion !!!
   en tfor.c ... cambiar sw1 a 1 y marca las variables en mth.mon directamente



3) correr 

   sh r8_crear ...  m

   crea repo con todos los programas a ultima version ( m para usar ~/wrk/Midas )

   esto genera 
   lr1        ... lista de files
   lr1_todos  ... copia 

   de aqui hay que construir

   r0_src_inc.sh

   esto genera

   lr1_solo_mth       aquellos files que tienen include mthnmcom (23 ... a la fecha )
   lr1_todos_sin_mth  todos menos los que tienen el include


4) para hacer cambio en algunas variables y probar 


   correr 
   sh r0_prueba1.sh    (corre con lr1_solo_mth)

   esto genera 

   m3     listado de todos los cambios de variables identificados en 
          los programas que figuran en lr1_solo_mth

   de aca

   r0_gen_m3_13

   genera

   m3_11  lista de programa con var cambiada
   m3_13  resumen ordenado de variables por programa

   
5) Para los programas con menos variables modificadas ....

   identificar esas variables

   hacer grep nombre programa m3_11 > m3_21
   asi con varios a eleccion

   cat m3_21 m3_22 ... > m3_31

   con el vi ... limpiar las variables encontradas

   vi m3_31
   ::%s/^.\{50\}//   (saca los 50 caracteres de cada linea)

   despues ... por ahora ... a manopla


5) hacer prueba de cambio con las variables elegidas !


   en vcb.txt ... poner # como primer caracter en todas las lineas
   sacar el # de la variables identificadas en los programas seleccionados


6) idem

   sh r0_prueba2.sh   (corre con lr1_todos)

   muestra si la variable, sin los underscores, existe en algun otro fuente que no sea
   los que incluyen el include file

   muestra cambios que hizo en todos los fuentes (lr1_todos)


7) por ahora, a mano 

   hacer un grep de la variable con los underscores en toda la base
   tienen que aparecer solos los cambiados !!
   si aparecen mas, significa que la variable ya existia con los underscores 
   tener cuidado, si aparece en un programa de los lr1_solo_mth, en lugar
   donde no se mostro que lo cambio, es porque ya existia una variable de ese
   nombre local !! hay que cambiarla a una ... _loc ... o similar





     
   
