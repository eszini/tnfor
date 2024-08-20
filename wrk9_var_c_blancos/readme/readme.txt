

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

   esto genera lr1 ... lista de files
   de aqui hay que construir

   lr1_todos          es lr1 menos mthnmcom
   lr1_solo_mth       aquellos files que tienen include mthnmcom (23 ... a la fecha )
   lr1_todos_sin_mth  todos menos los que tienen el include


4) para hacer cambio en algunas variables y probar 

   en vcb.txt ... poner # como primer caracter en todas las lineas
   sacar el # de la variable que se quiere cambiar
   dejar la variable como esta (el prog pone los underscores)

   correr 
   sh r0_prueba1.sh    (corre con lr1_solo_mth)

   esto muestra que programas de los lr1_solo_mth se veran afectados con cambios 

5) idem

   sh r0_prueba2.sh   (corre con lr1_todos)

   muestra si la variable, sin los underscores, existe en algun otro fuente que no sea
   los que incluyen el include file

   muestra cambios que hizo en todos los fuentes (lr1_todos)


6) prueba
   por ahora, a mano 

   hacer un grep de la variable con los underscores en toda la base
   tienen que aparecer solos los cambiados !!
   si aparecen mas, significa que la variable ya existia con los underscores 
   tener cuidado, si aparece en un programa de los lr1_solo_mth, en lugar
   donde no se mostro que lo cambio, es porque ya existia una variable de ese
   nombre local !! hay que cambiarla a una ... _loc ... o similar





     
   
