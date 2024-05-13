/*
 *	search:
 *      =============================================================
 *	"header"		header del programa
 *	"ejemplos"		ejemplos de uso (lineas de comando )
 *	"documentacion" 	alguna documentacion
 *	"ojo"			cosas a tener en cuenta !!!!!
 *	"programa"		programa
 *
 *
 */



/*
 *	//header//
 *
 *	tfor.c
 *	
 *
 * 	Sat May 11 06:59:21 -03 2024
 *	agregue que funcione con logical real y character
 *
 *	Thu May  9 06:34:25 -03 2024
 *	agregue tool6 para arreglar lineas de continuacion
 *
 *	Sat May  4 04:43:47 -03 2024
 *	agregue a opcion "prue2" que genere los archivos cargados 
 *	en un nuevo directorio
 *
 *
 *	proyecto estabiliazcion de codigo fuente de fortran
 *
 * 	Opciones:
 * 	-h   		forma de uso
 *	-v		verbose
 *	-opciones=Xi	X = (D)ebug, (I)nformative, (E)xtra (user def)
 *
 *
 *	Changes pending...
 *	
 *
 *
 */



/*
 *	//ejemplos//
 *
 * 	Ejemplos de uso:
 *
 *	1) Mostrar uso
 *
 *	tfor
 *	tfor -h
 *
 *
 *	2) Opciones de debug
 *
 *	tfor -v                     activa debug, nivel full ... equivalente a d5 
 *	tfor -v -opciones=Ln        letra L (i informativo d debug e no_me_acuerdo)  
 *	                                  Nro n nivel 1 a 5 ... 5 full
 *	3) parser
 *
 *	parsea programa.for, output a copia.for en mismo formato, 
 *	si parser no reconoce caracter, lo pone en log2
 *	
 *	tfor -v -f -opciones=d5 -inp=programa.for -out=copia.for -aux=log2  
 *
 *	4) contar caracteres
 * 
 *	para comparar si programa1.for y p2.for tienen la misma cantidad de caracteres "utiles"
 *
 *	./tfor -v -opciones=d5 -inp=programa1.for -out=p1.chr -exec=1 > log1
 * 	./tfor -v -opciones=d5 -inp=p2.for        -out=p2.chr -exec=1 > log2
 *
 *	5) abre archivo, lee a vector en memoria para procesos varios y genera output
 *
 *	./prog  -v -f -opciones=d5 -proc=3 -src=input -out=output
 *
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	xx) Muestra por pantalla, 14 ejercicios de como manejar estructuras
 *
 *	./prog  -v -f -opciones=d5 -prue=1
 *
 *	prue1: 
 *	prueba de manejo de estructuras y punteros a estructuras
 *	Distintas formas de referenciar a strcturas a traves 
 *	de punteros .
 *	funciones de ejemplo para pasarles punteros a punteros y mas ...
 *
 *
 *
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool1: 
 *
 *	Carga dos lista l1 y l2, verifica que elementos de l2 estan / no estan en l1
 *
 *	./tfor -v -opciones=d5 -tool=1 -inp=l1 -in2=l2 -out=l_si -ou2=l_no
 *	./tfor -v -opciones=d5 -tool=1 -inp=l1 -in2=l2 -out=l_si -ou2=l_no -m
 *
 *	-m  el chequeo se hace en minusculas, pero la salida respeta el case original
 *	    sin -m el chequeo es case sensitive
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool2: 
 *
 *	Carga un archivo formato makefile y devuelve una lista de archivos de dependencias
 *
 *	./tfor -v -opciones=d5 -tool=2 -inp=makefile -out=lista
 *	./tfor -v -opciones=d5 -tool=2 -inp=makefile -out=lista -m   (genera en minusculas)
 *
 *
 *	carga file tipo makefile y saca lista de archivos involucrados
 *	arma lista unica (elimina dup) con archivos que tengan ext indicada
 *	por ahora: for, f90, f95, mon  (min y may )
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool3: 
 *
 *	Carga un archivo formato con nombre files y genera igual en minuscula
 *
 *	./tfor -v -opciones=d5 -tool=3 -inp=nom_file -out=nom_file_minusculas
 *
 *
 *	carga un file con listado de files y genera listado con mismos archivos,
 *	pasados a minuscula
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool4:
 *
 *	WIP
 *
 *	prototipo de cargar source en memo, trabajar, y grabarlo
 *	en este caso, se cargan las lineas en vector de punteros a estruct
 *	a dif de otros encuadres, que son descomp en tokens
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool5: 
 *
 *	Carga un archivo fuente, parsea, y deja listo para cambios
 *	pero ... parsea por linea !! no tiene visibilidad del resto del src
 *
 *
 *	./tfor -v -opciones=d5 -tool=5 -inp=a_org -out=a_new -f
 *
 *	./tfor -v -opciones=d5 -tool=5 -inp=a_org -out=a_new -f -nvd=1,2,3
 *
 *
 *	toma un fuente fortran a_org
 *	genera un nuevo fuente a_new
 *	-f genera file nuevo con lineas completas, sin descomp en tokens
 *	cambios :
 *	WIP 
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool6:
 *
 *	Carga source en memo, trabaja, y graba
 *	Carga como vector de punteros a estructuras lineas completa
 *	Parsea cada linea a token
 *	
 *	Arregla las lineas de continuacion en fortran
 *
 *	./tfor -v -opciones=d5 -tool=6 -inp=t1.for -out=t2.for > log
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */


/*
 * 	nivel de verbose
 *
 * 	-v	activa verbose (por defecto, se pone en 'd5' )
 * 	-v -opciones=XX
 *
 * 		XX puede ser d0 d1 d2 d3 d4 d5
 *
 *
 * 	d0	no imprime nada
 *
 * 	d1	muestra lo minimo indispensable, 
 * 		- comienzo y finalizacion de procesos
 * 		- resultados generales
 * 		- estadisticas
 *
 *	d2,d3	distintos niveles de debug / informacion
 *
 *	d4	imprime todo
 *	
 *	d5	especial para funciones gp
 *
 */




/*
 *	errores
 *
 *	101	no pudo abrir archivo de input 
 *	102	no pudo abrir archivo de output 
 *	103	no 
 *	104	no 
 *	105	no 
 *
 *	111	no pudo abrir archivo de input src
 *	112	no pudo abrir archivo de input lst
 *
 *	501	overflow en busco_use
 *	502	overflow en busco_use
 *
 *	901	error en malloc
 *	902	error en malloc
 *	999	error debug
 */


/*
 *	//programa//
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#define	DEBUG 0

#define	MAXD	16	/* buffer de juguete */
#define	MAXV	64	/* buffer chico */
#define	MAXR	128	/* buffer razonable */
#define	MAXB	1024	/* buffer grande */
#define	MAXT	256 	/* maximo de tokens en tabla */
#define	MAXF	64	/* largo de archivos de input - output - marcas etc */
#define	MAXP	128	/* maximo de palabras en general */
#define	HUGE	4096	/* huge buffer */


/*
 *
 *	Funciones, variables y estructuras para manejo de parametros globales
 *
 */

#define	GP_SET 1
#define	GP_GET 0

int	gp_default();
int	gp_init(int ,char **);
int	gp_test();
int	gp_print();
int	gp_parser();
int	gp_fq(int,int);
int	gp_fverbose(char *);
char	*gp_tm();
char	*gp_fp(int,int,char **);

int	gp_q_partype1=0;	/* parametero tipo "name" */
int	gp_q_partype2=0;	/* parametero tipo "-something" */
int	gp_q_partype3=0;	/* parametero tipo "-someoption=somename" */
int	gp_q_partype4=0;	/* parametero tipo "--someoption" */


int	gp_verbose=0;		/* verbose 0 no 1 si */

int	gp_minusculas=0;	/* output en minuscula 0 no 1 si */
int	gp_fsentencia=0;	/* archivo de salida en formato sentencias 0 no 1 si */
int	gp_eol=0;		/* fuerzo string EOL al final de la linea ... a veces hay . en medio de la linea */
int	gp_reidx;		/* re indexar archivo de transacciones */
int	gp_pause;		/* pausa al mostrar ... */
int	gp_niveldes=0;		/* nivel de descripcion que se vuelca en archivo de salida (solo con fsentencia = 0 ) */
				/* 0 normal 1 sentencia y numero de token 2 .... agrego cosas del diccionario si se usa */

int	gp_help=0;		/* help 0 no 1 si */
int	gp_uso(int);		/* usage */
				/* 0 normal 1 sentencia y numero de token 2 .... agrego cosas del diccionario si se usa */

int	gp_tpar[4];		/* vector con cantidad de parametros de cada tipo */

#if 0
char	*gp_e = " Entra a proceso ";
char	*gp_s = " Sale de proceso ";
#endif

char	*gp_e = " (+) ";
char	*gp_s = " (-) ";
char	*gp_m[2] = { " ((+)) ", " ((-)) " };
int	gp_cnt;
char	gp_p[MAXP];


char	gp_opciones[MAXV];	/* opciones adicionales, informadas de la forma -opciones=[letras]  */
char	gp_dato[MAXV];		/* dato adicional que se desee informar ... cualquier string */
char	gp_pruebas[MAXV];	/* pruebas ...    -prue=[nro]   ej:  -prue=1    */
char	gp_exec[MAXV];		/* execs ...      -exec=[rnp]   ej:  -exec=1    */
char	gp_proc[MAXV];		/* procs ...      -proc=[rnp]   ej:  -proc=1    */
char	gp_tool[MAXV];		/* procs ...      -tool=[rnp]   ej:  -tool=1    */
char	gp_fyh[MAXR];		/* fecha y hora */



/*
 *
 *	Funciones, variables y estructuras globales en el programa
 *
 */

int	abro_files();
int	cierro_files();
int	mostrar_cargas();
int	mostrar_reportes();



int	gna(int,int);
char	*gnf(int);			/* genera fecha al azar */
char	*gnh(int,int);			/* genera hora al azar */
char	*gsf(char *);			/* string para imprimir fecha */
char	*gsh(char *);			/* string para imprimir hora */



char	*desde_igual(char *s);
char	*pasar_a_minusc(char *s);
char	*df(char *);
int	clear_screen();


int	tiene_igual(char *);
int	linea_vacia(char *);
int	tipo_char(char );
int	es_numero(char);
int	es_num_tk(char *);
int	es_word(char *);
int	es_puntuacion(char *);
int	char_demed(char);
int	mostrar(int,int,char *);
int	error(int);

int	proceso_principal();
int	proc_principal();

int	pro_prue1();
int	pro_prue2();
int	pro_prue3();

int	pro_exec1();
int	pro_exec2();
int	pro_exec3();

int	pro_proc1();
int	pro_proc2();
int	pro_proc3();

int	pro_tool1();
int	pro_tool2();
int	pro_tool3();
int	pro_tool4();
int	pro_tool5();
int	pro_tool6();
int	pro_tool7();




#define	TC_EOL	0
#define	TC_BLA	1
#define	TC_CCE	2	/* algunos caracteres especiales  ',;:/' */
#define	TC_PNT	3	/* punto '.' */
#define	TC_PAA	4	/* parent '([{' */
#define TC_PAC	5	/* parent ')]}' */
#define	TC_LET	6	/* letras */
#define	TC_NUM	7	/* numero */
#define	TC_CVR	8	/* caracteres varios ' % */
#define	TC_GBJ	9	/* guion bajo '_' */
#define	TC_CHO	10	/* 0 - choto */
#define	TC_RST	99

char	finp[MAXF];	/* archivo inp para entrada segun necesidad */
char	fin2[MAXF];	/* archivo inp para entrada segun necesidad */
char	fout[MAXF];
char	fou2[MAXF];
char	faux[MAXF];	/* archivo aux para salidas segun necesidad */
char	flog[MAXF];	/* archivo log para salidas segun necesidad */

char	fsrc[MAXF];	/* archivo de src de entrada */
char	flst[MAXF];	/* archivo de listado de src de entrada */

int	ffinp;
int	ffin2;
int	ffout;
int	ffou2;
int	ffaux;		/* archivo aux para output segun necesidad */
int	fflog;		/* archivo log para output segun necesidad */
int	ffcfg;		/* archivo de configuracion */

int	ffsrc;
int	fflst;

int	ffprb;		/* utilizo -prueba=1 ... 2 .. 3 etc */
int	ffexc;		/* utilizo -exec=1 .. 2  .. 3 etc   */
int	ffpro;		/* utilizo -proceso=1 .. 2 .. 3 etc */
int	fftoo;		/* utilizo -tool=1 .. 2 .. 3 etc */

int	ffdat;		/* utilizo -dato */

int	fftb1;
int	fftb2;
int	fftb3;
int	fftb4;

FILE	*hfinp;
FILE	*hfin2;
FILE	*hfout;
FILE	*hfou2;
FILE	*hfaux;
FILE	*hflog;

FILE	*hfsrc;
FILE	*hflst;

FILE	**fptr;


int	flag_caracteres;
int	flag_before_exit;


/*
 *	Estructuras y variables para tokens de las lineas procesadas
 *	en este caso, estan defindas localmente en proc parser
 *	de ser necesario acceder globalmente, definir aca
 *
 */


/* lo necesito para l_pars */
#if 1

int	q_tk;
char	tk[MAXT][MAXB];

#endif



/*
 * 	Variables y estructs para cargar todo un archivo en memoria
 *	const y estructs especificas para todo el tema de estabilizacion de fortran
 *
 */


#define	MAX_FSRC	400000		/* 10000 lineas de codigo ... seran suf ? */
int	qf_lin;				/* lineas en file */
int	qf_fen;				/* strings con nombre de archivos validos encontrados */

typedef	struct	tfn	*fnptr;
typedef	struct	tfn
{	char	l[HUGE];		/* por ahora, despues malloc */
	int	f1;			/* usos varios */
	int	f2;
	int	f3;
}	node;

fnptr	fnp1,fnp2,*fnq1,*fnq2,*fnpa;

fnptr	fnp[MAX_FSRC];			/* vector de punteros a lineas de source */
fnptr	fnf[MAX_FSRC];			/* vector de punteros a lineas de archivos encontrados */

int	pf_load();			/* proceso de carga del file */
int	qf_load(FILE *,fnptr *,int *);	/* proceso de carga del file */
int	pf_write();			/* proceso de write del file */



#define	MAX_FF		500		/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_FF];


/*
 *
 *	otras rutinas especificas de buscadores en source fortran 
 *
 */

int	busco_use();
int	encuentro_use(char *, char *);
int	is_use_valid(char);
int	linea_comentario(char *);




/*
 * 	Variables y estructs para cargar todo un archivo en memoria
 *
 *	Estructura de nodoslinkeados al siguiente
 *
 */



int	load_file(FILE *);

int	q_wrd;				/* cantidad de palabras en diccionario */

typedef	struct	pnode	*pnodeptr;
typedef	struct	pnode
{	char	*wrd;
	int	num;
	struct	pnode	*nx;
}	knode;

pnodeptr	npx,np1,*npa;





int	load_makefile(FILE *);
int	proc_makefile();
int	write_makefile(FILE *);
int	filter_makefile();
int	to_min();
int	es_char_v(char);
int	es_str_v(char *);

int	check_c1();
int	write_result();
int	check_fnames();
char	*extract_fname(char *);

int	linea_vacia_for(char *);
int	cfor_vars(int *,int *);
int	cfor_lcon(int *,int *);
int	l_pars(int, int *);
int	tiene_dec_var1();
int	tiene_mas(char *);
int	fix_dec_var1();
int	fix_dec_var2();
int	p_src();
int	es_cadena_valida(int,char *);
int	tiene_coment_intermedio (char *);




/*
 *	prueba loca
 *
 */



int	st_load();			/* funcion de carga de listas */
int	q_st;                           /* cantidad de palabras en lista */

typedef	struct	tst	*stptr;
typedef	struct	tst
{	char	*wrd;
	int	num;
	struct	tst	*nx;
}	nlst;

stptr	st_p1,st_p2,*st_n1,*st_n2,**st_nx,**st_ny;



#if 0
Definicion de chatGPT de esta struct:


typedef struct  tnode   *nodeptr;
typedef struct  tnode
{       char    *wrd;
        int     num;
        struct  tnode   *nx;
}       node;

nodeptr npx,np1,*npa;


Vamos a analizar cada línea de código C y explicar qué es cada cosa:

1. `typedef struct tnode *nodeptr;`
   
   Esta línea declara un alias (`nodeptr`) para un puntero a una estructura `tnode`. 
   En otras palabras, `nodeptr` es un tipo de dato que representa un puntero a un `tnode`. 
   Este tipo de dato se utilizará para manejar punteros a nodos en una lista enlazada.

2. `typedef struct tnode`
   `{ char *wrd;`
   `  int num;`
   `  struct tnode *nx;`
   `} node;`

   Esta es la definición de una estructura (`struct`) llamada `tnode`. 
   La estructura `tnode` tiene tres miembros:
   - `char *wrd;`: Un puntero a un carácter (`char`), que representa una cadena de caracteres (string).
   - `int num;`: Un entero (`int`), que puede utilizarse para almacenar un número.
   - `struct tnode *nx;`: Un puntero a otro `tnode`, es decir, un puntero a otra 
      estructura del mismo tipo (`struct tnode`). 

   Esto permite crear una lista enlazada de nodos.

   La definición también crea un alias (`node`) para la estructura `tnode`, 
   por lo que la estructura puede referirse como `node` en lugar de `struct tnode`.

3. `nodeptr npx, np1, *npa;`

   Esta línea declara tres variables:
   - `npx` y `np1`: Ambas son variables de tipo `nodeptr`, es decir, punteros a `node` o `tnode`.
   - `*npa`: Esta es una variable que apunta a un puntero (`nodeptr`). 
   En otras palabras, `npa` es un puntero a un puntero a `node` o `tnode`.

En resumen, este código define una estructura `node` con 
un puntero a una cadena de caracteres (`wrd`), 
un entero (`num`) y un puntero a otro nodo (`nx`), 
y crea alias para manejar punteros a la estructura. 
Además, declara variables de tipos `nodeptr` y un puntero a `nodeptr`.



#endif











/*
 * -----------------------------------------------------------------------------------
 *
 *	main
 *
 * -----------------------------------------------------------------------------------
 */

int	main(argc,argv)
int	argc;
char	**argv;
{
	int	n;

	/* miscelaneas gobales */
	flag_before_exit = 0;
	srand (time(NULL));

	/* init varios */
	gp_default();
	gp_init(argc,argv);
	gp_parser();

	char	z[MAXV];
	sprintf (z,"proceso main");



	/* shit finger ? */
	if (gp_help)
	 	gp_uso(0);


	/* programa */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
		gp_print();
	}


	/* opciones ingresadas  */
	if (gp_fverbose("d2"))
	{	gp_test();
	}


	/* abrir files */
	abro_files();

	/* proceso principal */
	proceso_principal();

	/* cerrar files */
	cierro_files();


	/* reportes ... */
	if (gp_fverbose("i1"))
	{
		mostrar_reportes();
	}

	/* si hay algo que hacer antes de salir, es aca ! */
	if (flag_before_exit)
	{	
		printf ("Atencion: .... \n");
	}

	/* programa */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	reportes 
 *
 * 	muestra reportes al finalizar todos los procesos
 *
 * -----------------------------------------------------------------------------------
 */



int	mostrar_reportes()
{
	int	n;

	/* proceso  */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra a mostrar reportes \n\n",gp_tm());
	}



	/* proceso  */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale de mostrar reportes \n\n",gp_tm());
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	proceso_principal
 *
 * 	procesa zonas definidas en el mapa
 *
 * -----------------------------------------------------------------------------------
 */

int	proceso_principal()
{

	char	z[MAXV];
	sprintf (z,"proceso principal");

	int	i;
	int	px,py;

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


#if 0
	/* muestro resultado de cargas, si las hubo */
	if (gp_fverbose("d3"))
	{
		mostrar_cargas();
	}
#endif


	/* proceso por defecto */
	if (!ffprb && !ffexc && !ffpro && !fftoo )
		proc_principal();



	if ( ffprb)
	{
		if (ffprb == 1)
			pro_prue1();
		if (ffprb == 2)
			pro_prue2();
		if (ffprb == 3)
			pro_prue3();
	}	

	if ( ffexc)
	{
		if (ffexc == 1)	
			pro_exec1();
		if (ffexc == 2)
			pro_exec2();
	}

	if ( ffpro)
	{
		if (ffpro == 1)	
			pro_proc1();
		if (ffpro == 2)
			pro_proc2();
		if (ffpro == 3)
			pro_proc3();
	}

	if ( fftoo)
	{
		if (fftoo == 1)	
			pro_tool1();
		if (fftoo == 2)
			pro_tool2();
		if (fftoo == 3)
			pro_tool3();
		if (fftoo == 4)
			pro_tool4();
		if (fftoo == 5)
			pro_tool5();
		if (fftoo == 6)
			pro_tool6();
	}



	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	proc_principal
 *
 * 	parser1
 * 	parser de linea de texto
 *
 * -----------------------------------------------------------------------------------
 */

int	proc_principal()
{

	int	px,py;


	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	q_lin;
	int	q_tk;
	int	p1,p2,p3,p4;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	tk[MAXT][MAXB];


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra a proc principal \n\n",gp_tm());
	}



/* parser */
/* parser */
#if 1

	/* si encuentro chars no detect por el parser, avisar al final del proceso */
	flag_caracteres = 0;

	q_lin=0;
	while (fgets(b1, MAXB, hfinp) != NULL)
	{

	    /* procesar solo lineas no vacias */
#if 0
	    /* opcion - no proceso lineas vacias */
	    if ( !linea_vacia(b1))
#endif

	    /* opcion - proceso lineas vacias */
	    if ( 1 )
	    {
		/* blancos al final */
		for (i=strlen(b1)-1, f4=1 ; i && f4 ; i-- )
			if (b1[i] == ' ' || b1[i] == '\n' )
				b1[i]=0;
			else
				b1[i+1]='\n', b1[i+2]=0, f4=0;

 
		f1=1;

		if (gp_fverbose("d3"))
		{
			printf ("Linea  : %d \n\n",q_lin);
			printf ("Buffer :|%s|\n\n",b1);
		}



		/* comienzo parser de tokens */
		p1=0;
		q_tk=0;

		while ( f1 )
		{
			/* controlamos cantidad de tokens ... */
			if (q_tk > MAXT-10)
			{	error(501);
			} 

			j=tipo_char(b1[p1]);

			switch (j)
			{
				/* otro caracter !!! */
				case TC_RST:
					flag_caracteres=1;
					if (gp_fverbose("d1"))
					{
						printf ("Caracter no definido para parser: %c %d\n",b1[p1],b1[p1]);
						if (ffaux)
							fprintf (hfaux,"Caracter no definido %d %c\n",b1[p1],b1[p1]);
							
					}
					p1++; 
					break; 

				/* letras */
				case TC_LET:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_LET || \
					        (j == TC_NUM && !char_demed(b1[p1-1])  ) )
						tk[q_tk][p2++]=b1[p1++];
					tk[q_tk][p2]=0;
					q_tk++;
					break;

				/* numeros tenemos que contemplar 3.3 o 3,3 !! */
				case TC_NUM:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_NUM || \
						(tipo_char(b1[p1]) == TC_PNT && tipo_char(b1[p1+1]) == TC_NUM ) || \
						( (b1[p1]) == ',' && tipo_char(b1[p1+1]) == TC_NUM ) )
					{	tk[q_tk][p2]=b1[p1];
						p1++;
						p2++;
					}
					tk[q_tk][p2]=0;
					q_tk++;
					break;


				/* blanco o tab */
				case TC_BLA:
#if 0
					/* opcion: no los guardo */
					while ( (j=tipo_char(b1[p1])) == TC_BLA)
					       p1++;	
#endif
					/* opcion: los guardo */
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;

					break;

				/* coma */
				case TC_CCE:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* punto */
				case TC_PNT:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis abre */
				case TC_PAA:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis cierra */
				case TC_PAC:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				case TC_EOL:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					p1++;
					f1=0;
					break;

				/* caracteres varios */
				case TC_CVR:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				default:
					printf ("Default, algo salio mal  !!!\n\n");
					f1=0;
					break;
			}

		} /* while */



		if (gp_fverbose("d3"))
			printf ("termine de parsear file \n");


		/* verifico si hay que sacar output  en minusculas */
		if (gp_minusculas)
		{
			for (j=0; j< q_tk; j++)
				strcpy(tk[j],pasar_a_minusc(tk[j]));
		}

#if 0
		/* si esta usando tabla de marcas ... verificar si hay que taggear */
		if (gp_tabmrk)
		{
			for (j=0; j< q_tk; j++)
				if (es_word(tk[j]) || es_puntuacion(tk[j]) )
					strcpy(tk[j],bm_tag(tk[j]));
		}
#endif



		/* hay que forzar  string EOL al final de la linea */
		if (gp_eol)
			strcpy(tk[q_tk++],"EOL");


		/* salida en formato token columnar */
		if (gp_fsentencia == 0)
		{


			/* pidio nivel de descripcion en salida ... agrego la sentencia */
			if (gp_niveldes)
				fprintf (hfout,"%s\n",b1);


			/* grabo los tokens encontrados */
			for (j=0; j< q_tk; j++)
			{
				switch (gp_niveldes)
				{

					case 0:
						fprintf (hfout,"%s\n",tk[j]);
						break;

					case 1:
						fprintf (hfout,"%3d,%s\n",j,tk[j]);
						break;

					default:
						fprintf (hfout,"%s\n",tk[j]);
						break;
				}


				if (gp_fverbose("d1"))
					printf ("%3d,%s\n",j,tk[j]);

			}



#if 0
			/* agrego termino EOL para indentificar donde termino
			 * esto ya que tal vez haya un . en mitad de linea 
			 */
			if (gp_eol)
				fprintf (hfout,"%s\n","EOL");

#endif
		}



		/* salida en formato sentencia */
		if (gp_fsentencia == 1)
		{


			/* grabo los tokens encontrados */
#if 0
			fprintf (hfout,"%s\n",b1);
#endif

			for (j=0; j< q_tk; j++)
			{
#if 0
				fprintf (hfout,"%3d,%s\n",j,tk[j]);
#endif
				fprintf (hfout,"%s",tk[j]);

				if (gp_fverbose("d1"))
					printf ("%3d,%s\n",j,tk[j]);

			}

#if 0
			/* agrego termino EOL para indentificar donde termino
			 * esto ya que tal vez haya un . en mitad de linea 
			 */
			if (gp_eol)
				fprintf (hfout,"%s\n","EOL");
			else
				fprintf (hfout,"\n");
#endif


			/* se termino la linea */
			fprintf (hfout,"\n");

		}


		if (gp_niveldes)
			fprintf (hfout,"\n\n");


		if (gp_fverbose("d1"))
		{
			printf ("\n");
		}




		/* 
		 * Termine todo lo que tenia que hacer con esta linea,
		 * sumo lineas 
		 *
		 */

		q_lin++;


	    } /* if ... no esta vacia la linea */

	}  /* while fgets ... */


	
	if (gp_fverbose("d1"))
	{
		printf ("Cant de lineas procesadas %d\n",q_lin);
		printf ("\n\n\n");
	}


	/* parser1 */




#endif
/* parser */
/* parser */




	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale de proc principal \n\n",gp_tm());
	}

}




/*
 * -----------------------------------------------------------------------------------
 *
 *	(MMM)
 *
 *	pro_exec 1
 *
 *	exec aparte ...
 *
 * -----------------------------------------------------------------------------------
 */


/* 
 *	pro_exec1
 *
 *	cuenta caracteres
 *
 */


int	pro_exec1()
{
	int	px,py;
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	q_lin;
	int	q_tk;
	int	p1,p2,p3,p4;
	
	int	tabla[256];

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	tk[MAXT][MAXB];

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso exec 1 \n\n",gp_tm());
	}
		


	/* init de valores */
	for (i=0; i<256; i++)
		tabla[i]=0;

	/* si encuentro caracteres no considerados para el parser, avisar al final de todo el proceso */
	flag_caracteres = 0;


	q_lin=0;
	while (fgets(b1, MAXB, hfinp) != NULL)
	{

	    /* opcion - proceso lineas vacias */
	    if ( 1 )
	    {
 
		if (gp_fverbose("d3"))
		{
			printf ("Linea  : %d \n\n",q_lin);
			printf ("Buffer :|%s|\n\n",b1);
		}

		f1=1;


		/* comienzo parser de tokens */
		p1=0;
		q_tk=0;

		while ( f1 )
		{
			j=tipo_char(b1[p1]);
			tabla[b1[p1]]++;
			
			if (b1[p1] == 0)
				f1=0;
			else
				p1++;

		} /* while */


		if (gp_fverbose("d3"))
			printf ("termine de parsear fila \n");
	   }

	   q_lin++;
	}


	/* grabo resultados */

	for (j=0; j< 256; j++)
	{
		fprintf (hfout,"Char %3d %5d \n",j,tabla[j]);

		if (gp_fverbose("d1"))
			printf ("Char %3d %5d \n",j,tabla[j]);
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale proceso exec 1 \n\n",gp_tm());
	}


}




/*
 * -----------------------------------------------------------------------------------
 *
 *	(MMM)
 *
 *	pro_exec 2
 *
 *	exec aparte ...
 *
 * -----------------------------------------------------------------------------------
 */




int	pro_exec2()
{

	/* prueba */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra a proceso exec 2 \n\n",gp_tm());
	}
		


	/* prueba */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale de proceso exec 2 \n\n",gp_tm());
	}

}







/*
 * -----------------------------------------------------------------------------------
 *
 *	(MMM)
 *
 *	pro_prueba 1
 *
 *	pruebas ...
 *
 * -----------------------------------------------------------------------------------
 */


#if 0
/*
 *	algo de referencia de las estrcturas y variables 
 */


int	st_load();			/* funcion de carga de listas */
int	q_st;                           /* cantidad de palabras en lista */

typedef	struct	tst	*stptr;
typedef	struct	tst
{	char	*wrd;
	int	num;
	struct	tst	*nx;
}	nodo_lista;

stptr	st_p1,st_p2,*st_n1,*st_n2,**st_nx;


typedef struct  tnode   *nodeptr;
typedef struct  tnode
{       char    *wrd;
        int     num;
        struct  tnode   *nx;
}       node;

nodeptr npx,np1,*npa;

			*npa = (nodeptr ) malloc ( sizeof (node));
			(**npa).wrd = ( char *) malloc(strlen(b1)+1);
			sprintf ( (**npa).wrd,"%s",b1);
			(**npa).num = q_wrd+1;
			(**npa).nx = (nodeptr) NULL;
			npa = (nodeptr *) & (*npa)->nx;


typedef	struct	tst	*stptr;
typedef	struct	tst
{	char	*wrd;
	int	num;
	struct	tst	*nx;
}	nodo_lista;

stptr	st_p1,st_p2,*st_n1,*st_n2,**st_nx;


#endif




int	ff1(stptr);
int	ff2(stptr *);
int	ff3(stptr **);
int	ff4(stptr **, stptr **);
int	ff5(stptr *);
int	ff6(stptr *, stptr *);
int	ff7(stptr **, stptr **);
int	ff8(stptr **);
int	ff9(stptr *);
int	ffa(stptr *);
int	qv;			/* cant de elementos en vector */

char	*sformat1 = "%03d   %-17s: %03d  -  %-17s: |%s| \n";
char	*sformat2 = "%03s   %-17s: %03d  -  %-17s: |%s| \n";
char	*sformat3 = "%03s   %-17s: %03d  -  %-17s: |%s| xx  %-17s: %03d  -  %-17s: |%s| \n";

/*
 *
 *	pruebas de manejo de estructuras
 *
 */

int	pro_prue1()
{


	struct tst	st1;			/* structura de tipo tst ... st1 */

	nlst		st2;                    /* uso el alias, nlst ... para crear st2 */
	nlst		*pst2;			/* puntero a struct tst o nslt */

	char		buf[64];

	stptr		v1[10];
	stptr		v2[10];

	int		i,j,k;

	char		b1[MAXB];
	int		flag,f1;
	int		ql;


	/* prueba */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso prueba 1 \n\n",gp_tm());
	}

	qv = 5;		/* limitamos a 5 */


/*
 *
 *	1) Estructura simple
 *	
 *	usamos una estructura declarada
 *	la inicializamos
 *	imprimimos contenido
 *
 */


	printf ("1) Estructura simple \n");
	memset (&st1,0,sizeof(st1));
	memset (&st1,0,sizeof(struct tst));

	i=1;
	strcpy(buf,"e01_campo 1");

	st1.wrd = malloc (64);
	sprintf(st1.wrd,buf);
	st1.num = 1;

	printf (sformat1,
		i, "st1.num", st1.num, 
                   "st1.wrd", st1.wrd        );

	free (st1.wrd);
	printf (" - - - - \n\n");
	
/*
 *
 *	2) Puntero a estructura simple
 *	
 *	usamos un tipo a puntero a estructura
 *	allocamos memoria con malloc
 *	la inicializamos
 *	imprimimos contenido
 *
 */

	i=2;
	printf ("2) Puntero a estructura simple \n");
	st_p1 = malloc ( sizeof (struct tst) );

	(*st_p1).wrd = malloc (64);
	sprintf ( (*st_p1).wrd, "e02_campo 1");
	(*st_p1).num = 2;

	printf (sformat1,
		i, "(*st_p1).num", (*st_p1).num, 
                   "(*st_p1).wrd", (*st_p1).wrd        );


	free ( (*st_p1).wrd );
	free ( st_p1 );
	printf (" - - - - \n\n");

/*
 *
 *	3) Puntero a puntero de estructura simple
 *	
 *	usamos un tipo a puntero a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 */

	i=3;
	printf ("3) Puntero a puntero de estructura simple \n");
	st_p1 = malloc ( sizeof (struct tst) );
	(*st_p1).wrd = malloc (64);
	sprintf ( (*st_p1).wrd, "e03_campo 1");
	(*st_p1).num = 2;

	st_n1 = &st_p1;

	printf (sformat1,
		i, "(**st_n1).num", (**st_n1).num, 
                   "(**st_n1).wrd", (**st_n1).wrd        );

	free ( (*st_p1).wrd );
	free ( st_p1 );
	printf (" - - - - \n\n");

/*
 *
 *	4) Puntero a puntero de estructura simple + funcion1
 *	
 *	la funcion recibe un puntero a structura
 *
 *	usamos un tipo a puntero a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff1
 *	recibe un puntero a estructura
 *	imprime el contenido
 *
 */


	i=4;
	printf ("4) Puntero a puntero de estructura simple + funcion1 \n");

	st_p1 = malloc ( sizeof (nlst) );
	(*st_p1).wrd = malloc (64);
	sprintf ( (*st_p1).wrd, "e04_campo 11");
	(*st_p1).num = 11;

	st_p2 = malloc ( sizeof (nlst) );
	(*st_p2).wrd = malloc (64);
	sprintf ( (*st_p2).wrd, "e04_campo 21");
	(*st_p2).num = 21;

	st_n1 = &st_p1;
	printf (sformat1,
		i, "(**st_n1).num", (**st_n1).num, 
                   "(**st_n1).wrd", (**st_n1).wrd       );

	st_n1 = &st_p2;
	printf (sformat1,
		i, "(**st_n1).num", (**st_n1).num, 
                   "(**st_n1).wrd", (**st_n1).wrd       );

	ff1(st_p1);
	ff1(st_p2);

	printf (" - - - - \n\n");


/*
 *
 *	5) Puntero a puntero de estructura simple + funcion2
 *	
 *	la funcion recibe un puntero a puntero de stuctura
 *
 *	usamos un tipo a puntero a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff2
 *	recibe un puntero a estructura
 *	imprime el contenido
 */

	i=5;
	printf ("5) Puntero a puntero de estructura simple + funcion2 \n");

	st_n1 = &st_p1;
	ff2(st_n1);
	st_n1 = &st_p2;
	ff2(st_n1);

	printf (" - - - - \n\n");


/*
 *
 *	6) Vector de punteros a estructura simple
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff3
 *	recibe un puntero a a puntero de estructura
 *	imprime el contenido
 *
 *	ff4
 *	idem ff3, pero recibe dos arg iguales
 *	este formato seria el ideal a usar si
 *	queremos comparar contenido de dos cadenas
 *	de info (estructuras como lista de nodos encadenados)
 *	
 */

	i=6;
	printf ("6) Vector de punteros a estructura simple  \n");

	for (j=0; j< qv; j++)
	{

		v1[j] = malloc ( sizeof (nlst ));
		memset (v1[j],0, sizeof (nlst ));
		(*v1[j]).wrd = malloc(64);
		sprintf ( v1[j]->wrd,"e06_campo %d",100+j);
		v1[j]->num = 100+j;

		printf (sformat1,
			i, "v1[j]->num", v1[j]->num, 
               	           "v1[j]->wrd", v1[j]->wrd      );

		v2[j] = malloc ( sizeof (struct tst ));
		memset (v2[j],0, sizeof (struct tst ));
		(*v2[j]).wrd = malloc(64);
		sprintf ( v2[j]->wrd,"e06_campo %d",200+j);
		v2[j]->num = 200+j;

		printf (sformat1,
			i, "v2[j]->num", v2[j]->num, 
               	           "v2[j]->wrd", v2[j]->wrd      );

	}

	printf ("\n");

	for (j=0; j< qv; j++)
	{
		ff1(v1[j]);
		ff1(v2[j]);
	}

	printf ("\n");

	for (j=0; j< qv; j++)
	{
		st_n1 = &v1[j];
		ff2(st_n1);
		st_n1 = &v2[j];
		ff2(st_n1);
	}

	printf (" - - - - \n\n");


/*
 *
 *	7) pasaje de variables a funciones 
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff3
 *	recibe un puntero puntero a puntero de estructura
 *	imprime el contenido
 *
 *	ff4
 *	idem ff3, pero recibe dos arg iguales
 *	este formato seria el ideal a usar si
 *	queremos comparar contenido de dos cadenas
 *	de info (estructuras como lista de nodos encadenados)
 *	
 */

	i=7;
	printf ("7) Pasaje de variables a funciones  \n");

	st_nx = &st_n1;

	st_n1 = &v1[0];
	ff3(st_nx);
	st_n1 = &v2[0];
	ff3(st_nx);


	printf ("\n");
	
	st_n1 = &v1[0];
	st_n2 = &v2[0];

	st_nx = &st_n1;
	st_ny = &st_n2;

	ff4(st_nx,st_ny);
	
	printf (" - - - - \n\n");



/*
 *
 *	8) pasaje de variables a funciones 
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff3
 *	recibe un puntero puntero a puntero de estructura
 *	imprime el contenido
 *
 *	ff4
 *	idem ff3, pero recibe dos arg iguales
 *	este formato seria el ideal a usar si
 *	queremos comparar contenido de dos cadenas
 *	de info (estructuras como lista de nodos encadenados)
 *	
 */

	i=8;
	printf ("8) Cadena de estructuras (lista de nodos )  \n");


	/* cantidad de palabras en lista o cantidad de nodos  */
	ql = 0;

	/* ptr al contenedor de direccion donde empieza la cadena */
	st_n1 = (stptr *) &st_p1;

	/* armo una cadena de nodos, cada nodo es una estructura 
	 * en st_n1 ... queda apuntando al ultimo nodo para agregar
	 */

	ql=0;
	for (j=0; j< qv; j++)
	{
		sprintf (b1,"e08_campo %03d",ql+101);

		*st_n1 = (stptr  ) malloc ( sizeof (nlst));
		(**st_n1).wrd = ( char *) malloc(32);
		sprintf ( (**st_n1).wrd,"%s",b1);
		(**st_n1).num = ql + 101;
		(**st_n1).nx = (stptr) NULL;
		st_n1 = (stptr *) & (*st_n1)->nx;

		ql++;

	}

	printf ("\n");

	
	/* vuelvo st_n1 a donde empieza la cadena */
	st_n1 = (stptr *) &st_p1;

	/* armo una cadena de nodos, cada nodo es una estructura 
	 * en st_n1 ... queda apuntando al ultimo nodo para agregar
	 */


	j=0;
	while ( (*st_n1) != (stptr ) NULL )
	{
		printf (sformat1,
			j, "(**st_n1).num", (**st_n1).num,
			   "(**st_n1).wrd", (**st_n1).wrd      );

		st_n1 = (stptr *) & (*st_n1)->nx;
		j++;
	}

	printf (" - - - - \n\n");




/*
 *
 *	9) pasaje de variables a funciones 
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff3
 *	recibe un puntero puntero a puntero de estructura
 *	imprime el contenido
 *
 *	ff4
 *	idem ff3, pero recibe dos arg iguales
 *	este formato seria el ideal a usar si
 *	queremos comparar contenido de dos cadenas
 *	de info (estructuras como lista de nodos encadenados)
 *	
 */

	i=9;
	printf ("9) Cadena de estructs - armo en funcion y tengo acceso en prog  \n");


	st_n1 = (stptr *) &st_p1;
	ff5(st_n1);

	j=0;
	st_n1 = (stptr *) &st_p1;

	while ( (*st_n1) != (stptr ) NULL )
	{
#if 0
		printf ("%3d  (**st_n1).num: %3d  (**st_n1).wrd: |%s|\n",j,(**st_n1).num,(**st_n1).wrd);
#endif

		printf (sformat1,
			j, "(**st_n1).num", (**st_n1).num,
			   "(**st_n1).wrd", (**st_n1).wrd      );

		st_n1 = (stptr *) & (*st_n1)->nx;
		j++;
	}

	printf (" - - - - \n\n");






/*
 *
 *	10) pasaje de variables a funciones 
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff3
 *	recibe un puntero puntero a puntero de estructura
 *	imprime el contenido
 *
 *	ff4
 *	idem ff3, pero recibe dos arg iguales
 *	este formato seria el ideal a usar si
 *	queremos comparar contenido de dos cadenas
 *	de info (estructuras como lista de nodos encadenados)
 *	
 */

	i=10;
	printf ("10) Cadena de estructs - armo en funcion y tengo acceso en prog  \n");


	st_n2 = (stptr *) &st_p2;
	ff5(st_n2);

	j=0;
	st_n2 = (stptr *) &st_p2;

	while ( (*st_n2) != (stptr ) NULL )
	{
		printf (sformat1,
			j, "(**st_n2).num", (**st_n2).num,
			   "(**st_n2).wrd", (**st_n2).wrd      );

		st_n2 = (stptr *) & (*st_n2)->nx;
		j++;
	}

	printf (" - - - - \n\n");





/*
 *
 *	11) pasaje de variables a funciones 
 *	    similar al anterior, pero con 2 argumentos a funcion
 *	    seria el modo ideal de mandar dos cadenas de estructuras
 *	    a una funcion para que haga algo (comparar ? , merge ? )
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *
 *	ff6
 *	recibe dos punteros a punteros de estructuras
 *	arma una cadena sobre cada puntero
 *	
 */

	i=11;
	printf ("11) Cadena de estructs (2) - armo en funcion y tengo acceso en prog  \n");



	printf ("loop de armado de lista 8 \n");
	st_n1 = (stptr *) &st_p1;
	st_n2 = (stptr *) &st_p2;
	ff6(st_n1,st_n2);

	j=0;
	st_n1 = (stptr *) &st_p1;
	st_n2 = (stptr *) &st_p2;

	/* para simplificar, ambas cadenas tienen la misma cantidad de nodos */
	while ( (*st_n1) != (stptr ) NULL && (*st_n2) != (stptr ) NULL )
	{
		sprintf (buf,"%03d",j);

		printf (sformat3,
			buf, 
			"(**st_n1).num", (**st_n1).num, "(**st_n1).wrd", (**st_n1).wrd,
			"(**st_n2).num", (**st_n2).num, "(**st_n2).wrd", (**st_n2).wrd
			);

		/* avanzo al proximo nodo (para ambas listas ) */
		st_n1 = (stptr *) & (*st_n1)->nx;
		st_n2 = (stptr *) & (*st_n2)->nx;
		j++;
	}

	printf (" - - - - \n\n");



/*
 *
 *	12) pasaje de variables a funciones 
 *	    similar al anterior, pero con 2 argumentos a funcion
 *	    seria el modo ideal de mandar dos cadenas de estructuras
 *	    a una funcion para que haga algo (comparar ? , merge ? )
 *	
 *	paso puntero a puntero a puntero del primer nodo,
 *	cuando vuelvo de la rutina, sigo teniendo acceso
 *	a toda la cadena y al ultimo nodo.
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *
 *	ff7
 *	recibe dos punteros a punteros de estructuras
 *	arma una cadena sobre cada puntero
 *	
 */

	i=12;
	printf ("12) Cadena de estructs (2) - armo en funcion y tengo acceso en prog  \n");


	st_nx = &st_n1;
	st_ny = &st_n2;

	/* guardo la direccion del primer nodo. en st_n1 va a quedar la direccion del ultimo desp */
	st_n2 = st_n1;

	/* creo dos listas de nodos encadenados */
	ff7(st_nx,st_ny);

	j=0;
	while ( (*st_n2) != (stptr ) NULL )
	{
		printf (sformat1,
			j, 
			"(**st_n2).num", (**st_n2).num, "(**st_n2).wrd", (**st_n2).wrd  );

		st_n2 = (stptr *) & (*st_n2)->nx;
		j++;
	}

	printf (" - - - - \n\n");


/*
 *
 *	13) pasaje de variables a funciones 
 *	    Para un puntero a un vector de estructuras a funcion,
 *	    para que inicialice.
 *	    Es lo que usariamos para cargar varios archivos
 *	    diferentes, a un mismo tipo de estructura,
 *	    usando la misma funcion.
 *
 *
 *	
 */

	i=13;
	printf ("13) Inicializo un vector de punteros a estructs en una funcion   \n");

	st_n1 = &v1[0];
	st_nx = &st_n1;

	/* imprimo el estado actual */
	ff3(st_nx);

	/* mando a inicializar */
	ff8(st_nx);
	
	/* imprimo el nuevo estado  */
	ff3(st_nx);

	
	printf (" - - - - \n\n");


/*
 *
 *	14) pasaje de variables a funciones 
 *	    Para un puntero a un vector de estructuras a funcion,
 *	    para que inicialice.
 *	    Es lo que usariamos para cargar varios archivos
 *	    diferentes, a un mismo tipo de estructura,
 *	    usando la misma funcion.
 *
 *	idem anterior, pero con un nivel menos de indireccion
 *
 *	
 */

	i=14;
	printf ("14) Inicializo un vector de punteros a estructs en una funcion   \n");

	st_n1 = &v1[0];

	/* imprime estado */
	ff9(st_n1);

	/* inicializo */
	ffa(st_n1);

	/* imprime estado */
	ff9(st_n1);

	printf (" - - - - \n\n");




	/* prueba */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale proceso prueba 1 \n\n",gp_tm());
	}

	printf ("\n");
}





/*
 *	ff1
 *	recibe un puntero a estructura
 *	imprime contenido
 *
 */

int	ff1(p1)
stptr	p1;
{
	printf (sformat2,
		"ff1", "(*p1).num", (*p1).num,
                       "(*p1).wrd", (*p1).wrd        );
}

/*
 *	ff2
 *	recibe un puntero a puntero de estructura
 *	imprime contenido
 *
 */

int	ff2(q1)
stptr	*q1;
{
	printf (sformat2,
		"ff2", "(**q1).num", (**q1).num,
                       "(**q1).wrd", (**q1).wrd        );

}

/*
 *	ff3
 *	recibe un puntero a puntero a puntero de estructura
 *	imprime contenido
 *
 */

int	ff3(r1)
stptr	**r1;
{
	int 	i,j;

	for (i=0; i< qv; i++)
	{
		printf (sformat2,
			"ff3", "(*(*r1)[i]).num", (*(*r1)[i]).num ,
			       "(*(*r1)[i]).wrd", (*(*r1)[i]).wrd     );
	}
}

/*
 *	ff4
 *	recibe un puntero a puntero a puntero de estructura
 *	imprime contenido
 *	dos argumentos iguales 
 */

int	ff4(r1,r2)
stptr	**r1,**r2;
{
	int 	i,j;

	for (i=0; i< qv; i++)
	{
		printf (sformat3,
			"ff4", 
			"(*(*r1)[i]).num", (*(*r1)[i]).num, "(*(*r1)[i]).wrd", (*(*r1)[i]).wrd,
			"(*(*r2)[i]).num", (*(*r2)[i]).num, "(*(*r2)[i]).wrd", (*(*r2)[i]).wrd
			);
	}

}

/*
 *	ff5
 *	recibe un puntero a puntero de estructura
 *	arma una lista encadenada de elementos
 *	
 *	atencion: ojo con esto !
 *	al terminar ... 
 *	p1 apunta al ultimo eslabon de la cadena
 *	pero al volver de la rutina, ese valor
 *	se pierde !
 *	El llamador solo tiene la direccion del
 *	primer eslabon de la cadena ...
 *	Tendria que recorer toda la cadena para 
 *	poder volver a agregar eslabones.
 */

int	ff5(p1)
stptr	*p1;
{

	int	ql,i;
	char	b1[1024];

	ql=0;
	for (i=0; i< qv; i++)
	{
		sprintf (b1,"ff5_campo %03d",ql+201);

		*p1 = (stptr  ) malloc ( sizeof (nlst));
		(**p1).wrd = ( char *) malloc(20);
		sprintf ( (**p1).wrd,"%s",b1);
		(**p1).num = ql + 201;
		(**p1).nx = (stptr) NULL;
		p1 = (stptr *) & (*p1)->nx;

		ql++;

	}

}


/*
 *	ff6
 *	idem ff5 pero con dos argumentos iguales
 *
 *	recibe un puntero a puntero de estructura
 *	arma una lista encadenada de elementos
 *	
 *	atencion: ojo con esto !
 *	al terminar ... 
 *	p1 apunta al ultimo eslabon de la cadena
 *	pero al volver de la rutina, ese valor
 *	se pierde !
 *	El llamador solo tiene la direccion del
 *	primer eslabon de la cadena ...
 *	Tendria que recorer toda la cadena para 
 *	poder volver a agregar eslabones.
 */


int	ff6(p1,p2)
stptr	*p1,*p2;
{
	int	ql,i;
	char	b1[1024];

	ql=0;
	for (i=0; i< qv; i++)
	{
		sprintf (b1,"ff6_campo %03d",ql+301);

		*p1 = (stptr  ) malloc ( sizeof (nlst));
		(**p1).wrd = ( char *) malloc(20);
		sprintf ( (**p1).wrd,"%s",b1);
		(**p1).num = ql + 301;
		(**p1).nx = (stptr) NULL;
		p1 = (stptr *) & (*p1)->nx;

		ql++;

	}

	ql=0;
	for (i=0; i< qv; i++)
	{
		sprintf (b1,"ff6_campo %03d",ql+401);

		*p2 = (stptr  ) malloc ( sizeof (nlst));
		(**p2).wrd = ( char *) malloc(20);
		sprintf ( (**p2).wrd,"%s",b1);
		(**p2).num = ql + 401;
		(**p2).nx = (stptr) NULL;
		p2 = (stptr *) & (*p2)->nx;

		ql++;

	}

}

/*
 *	ff7
 *	recibe dos argumentos iguales
 *
 *	recibe puntero a puntero puntero de estructura
 *	arma una lista encadenada de elementos
 *	
 *	atencion: ojo con esto !
 *	(la diferencia con ff6, un nivel mas de indireccion)
 *
 *	al terminar ... 
 *	r1 apunta al puntero al ultimo eslabon de la cadena
 *	de manera que al volver de la rutina,
 *	ese valor le queda al caller porque
 *	r1 apunta a un valor que lo tiene registrado
 */


int	ff7(r1,r2)
stptr	**r1,**r2;
{
	int	ql,i;
	char	b1[1024];

	ql=0;
	for (i=0; i< qv; i++)
	{
		sprintf (b1,"ff7_campo %03d",ql+201);

		**r1 = (stptr  ) malloc ( sizeof (nlst));

		(***r1).wrd = ( char *) malloc(20);
		sprintf ( (***r1).wrd,"%s",b1);
		(***r1).num = ql + 201;
		(***r1).nx = (stptr) NULL;
		*r1 = (stptr *) & (**r1)->nx;
		ql++;
	}

}



/*
 *	ff8
 *	recibe un puntero a puntero a puntero de estructura
 *	inicializa contenido
 *
 */

int	ff8(r1)
stptr	**r1;
{
	int 	i,j;

	for (i=0; i< qv; i++)
	{
		sprintf ( (*(*r1)[i]).wrd, "ff8_campo %03d", i);
		(*(*r1)[i]).num = i;
	}
}


/*
 *	ff9
 *	recibe un puntero a puntero de estructura
 *	imprime contenido
 *
 */

int	ff9(q1)
stptr	*q1;
{
	int	i;

	for (i=0; i< qv; i++)
	{
		printf (sformat2,
			"ff9", "(*q1[i]).num", (*q1[i]).num,
       		               "(*q1[i]).wrd", (*q1[i]).wrd        );
	}
}


/*
 *	ffa
 *	recibe un puntero a puntero de estructura
 *	imprime contenido
 *
 */

int	ffa(q1)
stptr	*q1;
{
	int	i;

	for (i=0; i< qv; i++)
	{
		sprintf ( (*q1[i]).wrd, "ffa_campo %03d",i);
		(*q1[i]).num = i;
	}
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prueba 2
 *
 *	pruebas ...
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	prue2
 *
 *	abre archivo con lista de archivos a procesar (fuentes fortran)
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 */


#if 0

#define	MAX_FF		500		* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_FF];

#endif


int	pro_prue2()
{

	int	i,j,k;
	char	d1[MAXB];
	char	d2[MAXB];
	int	ql,qlf;
	int	flag;
	int	q_ptr;

	FILE	*hwi,*hwo;


	char	z[MAXV];
	sprintf (z,"prue2");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffdat )
		gp_uso(11);



	/* cantidad de lineas en el archivo  */
	qf_ff = 0;
	q_ptr = 0;
	ql=0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			fnq1 = &fnp[q_ptr];
			qf_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(903);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).pf = q_ptr;
			(*tb[qf_ff]).uf = q_ptr+qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d |%s|\n",
					(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
			ql++;
		}
	}


	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de lineas cargadas:  %5d \n",ql);
		printf ("\n");
	}

#if 1
	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< q_ptr; i++)
		{
			printf ("i: %5d  |%s| \n",
				i,(*fnp[i]).l );
		}
	}

	printf ("\n");

#endif

	/* proceso todos los files */
	p_src();



	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(602);

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}


	fclose(hwo);


		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}





int	p_src()
{

	int	i,j,k,l;
	int	f1,f2,f3,f4;
	int	pf,uf;

	char	b1[MAXB];


	printf ("proceso file ... \n");

	for (i=0; i< qf_ff; i++)
	{
		if (gp_fverbose("d1"))
		{
			printf ("proceso: %6d %6d |%s|\n",
				(*tb[i]).pf,(*tb[i]).uf,(*tb[i]).n);

		}


		pf = (*tb[i]).pf;
		uf = (*tb[i]).uf;

		for (j = pf; j <= uf; j++)
		{
			/* parseo fila a tokens */
			l_pars(j,&q_tk);


			/*
			 * detectar si la linea es comentarios ...
			 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
			 */

			f4 = 1;
				if (!strcmp("!",tk[0]))
					f4 = 0;
				if (!strcmp("C",tk[0]))
					f4 = 0;
				if (!strcmp("c",tk[0]))
					f4 = 0;

			for (k=0; f4 && k<q_tk; k++)
			{


				if (es_cadena_valida(1,tk[k]))
				{
					memset (b1,0,MAXB);
					f1 = 1;
					f2 = 1;

					while (f1)
					{
						strcat(b1,tk[k]);
						
						k++;
						if (k == q_tk)
							f1 = 0;

						if (f1 && !es_cadena_valida(2,tk[k]) )
							f1 = 0, f2 = 0;
					}

					/*
					 * ojo ... falta hacer un trim si hay blancos al final 
					 *
					 */
					if (f2)
					{
						if (strlen(b1) > 25)
							fprintf (hfout,"|%s|\n",b1);
					}
				} 
			} 
		}
	}
}




int	es_cadena_valida(pos,s)
int	pos;
char	*s;
{
	int	i,j,k,flag;
	int	f1,f2;
	char	c;

	flag=1;
	if (!strcmp("use",pasar_a_minusc(s) ))
		flag = 0;
	if (!strcmp("subroutine",pasar_a_minusc(s) ))
		flag = 0;
	if (!strcmp("entry",pasar_a_minusc(s) ))
		flag = 0;
	if (!strcmp("call",pasar_a_minusc(s) ))
		flag = 0;
	if (!strcmp("end",pasar_a_minusc(s) ))
		flag = 0;
	if (!strcmp("integer",pasar_a_minusc(s) ))
		flag = 0;

	for (i=0; flag && i<strlen(s); i++)
	{
		c = s[i];

		flag = 0;

		if (pos == 1)
		{	
			if ( c >= 'a' && c <= 'z' )
				flag = 1;
			if ( c >= 'A' && c <= 'Z' ) 
				flag = 1;

		}

		if (pos == 2)
		{
			if ( c >= 'a' && c <= 'z' )
				flag = 1;
			if ( c >= 'A' && c <= 'Z' ) 
				flag = 1;
			if ( c >= '0' && c <= '9' )
				flag = 1;
			if ( c == ' ' || c == '_' )
				flag = 1;
		}

	}

	return flag;
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prue 3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	prue3
 *
 *	Descripcion:
 *
 */


/* bloque */
#if 1


int	pro_prue3()
{
	int	i,j,k;
	int	f1,f2,f3;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;

	char	*v1[100];
	char	**s1;
	char	t[100][64];

	char	z[MAXV];
	sprintf (z,"prue3");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

#if 0
	if (!1 || !2 )
		gp_uso(11);
#endif

	/* bloque */
	for (i=0; i<100; i++)
	{
		sprintf (t[i],"Param %3d",i);
		v1[i] = t[i];
	}

	s1 = &v1[0];

	for (i=0; i<100; i++)
	{
		printf ("Param : %3d  = |%s| \n",i,*(s1+i) );
	}	


		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */




/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_proc 1 
 *
 *	procesos adicionales / alternativos
 *
 * -----------------------------------------------------------------------------------
 */

/*	WIP
 *
 *	proc 1
 *
 *	carga el fuente fortran
 *	
 *	busca en las lineas
 *	INCLUDE algo
 *	USE algo
 */ 


int	pro_proc1()
{
	char	z[MAXV];
	sprintf (z,"proc2");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!!ffsrc )
		gp_uso(0);

	/* cargo el source en memoria */
	pf_load();

	/* busco expresiones del tipo 'USE ... ' */
	busco_use();

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_proc 2 
 *
 *	procesos adicionales / alternativos
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	proc 2
 *
 *	carga todos los fuentes fortran a partir de un
 *	archivo listado
 */ 



int	pro_proc2()
{

	char	b1[MAXB];
	char	b2[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;
	int	ql_arc;

	char	farc[MAXF];
	int	ffarc;
	FILE	*hfarc;

	char	z[MAXV];
	sprintf (z,"proc2");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

/* proc2 */
#if 1

	if (!!fflst)
		gp_uso(0);


/*
 *	todo esto aca, esta mal
 *	hay que poner solo las llamadas a cada archivo para procesar 
 *	el esto, en subrutinas
 *
 */

	ffarc=1;

	ql=0;
	while (fgets(b1,MAXB,hflst) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;

					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			strcpy(farc,b1);

			if (gp_fverbose("d1"))
			{
				printf ("%3d |%s|\n",ql,b1);
			}


			if ( ffarc && ((hfarc = fopen (farc,"r")) == NULL) )
			{
				error(201);
			}


			/* proceso file ... aca ??? */

			ql_arc = 0;

			while (fgets(b2,MAXB,hfarc) != NULL)
			{
				/* saco el fin de linea - contemplo 13 x fuentes fortran */
				for ( flag=0, j=strlen(b2); !flag && j; j--)
					if (b2[j] == '\n' )
					{	
						flag=1;

						if ( j && b2[j-1] == 13)
							b2[j-1]=0;
						else
							b2[j]=0;
					}

				printf ("|%s|\n",b2);
				ql_arc++;

			}



			/* cierro file */
			if (ffarc)
				fclose(hfarc);

			ql++;


		}  /* if linea vacia ... */
	}




#endif
/* proc2 */

		

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


/*
 * -----------------------------------------------------------------------------------
 *          
 *
 *	pro_proc 3 
 *
 *	procesos adicionales / alternativos
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	proc 3
 *
 *	carga el fuente fortran
 *	
 *	a ver si podemos generar uno con correccion de continuacion de lineas
 *
 *	./prog  -v -f -opciones=d5 -proc=3 -src=input -out=output
 *
 */ 


int	pro_proc3()
{
	sprintf (gp_p,"proc3");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_e,gp_p,gp_tm());
	}

#if 1
	/* cargo el source en memoria */
	pf_load();



	/* grabo el source a file */
	pf_write();

#endif


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_p,gp_p,gp_tm());
	}
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	carga un archivo a vector de estructuras
 *
 * -----------------------------------------------------------------------------------
 */



/* qf_load */
#if 1

int	qf_load(pfr,q1,l1)
FILE	*pfr;
fnptr	*q1;
int	*l1;
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	char	z[MAXV];
	sprintf (z,"qf_load");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* cantidad de lineas en el archivo  */
	ql=0;

	while (fgets(b1,MAXB,pfr) != NULL)
	{

#if 0
		if (!linea_vacia(b1)  && b1[0] != '#' )
#endif
		if ( 1 )
		{

			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;

					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			if (gp_fverbose("d3"))
				printf ("%3d |%s|\n",ql,b1);

			q1[ql] = (fnptr  ) malloc (sizeof (node));
			if ( q1[ql] == NULL)
				error(901);

			sprintf ( (*q1[ql]).l,"%s",b1);
			(*q1[ql]).f1 = 0;
			(*q1[ql]).f2 = 0;
			(*q1[ql]).f3 = 0;

			ql++;
		}
	}

	*l1 = ql;

	if (gp_fverbose("d2"))
	{
		printf ("Cantidad de lineas en source ql       : %6d\n",ql);
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}

#endif
/* qf_load */














/*
 * -----------------------------------------------------------------------------------
 *
 *	carga de un archivo a vector
 *
 * -----------------------------------------------------------------------------------
 */


/* pf_load */
#if 1

int	pf_load()
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	/* cantidad de lineas en el archivo  */
	qf_lin = 0;
	ql=0;

	while (fgets(b1,MAXB,hfsrc) != NULL)
	{
#if 0
		if (!linea_vacia(b1)  && b1[0] != '#' )
#endif
		if ( 1 )
		{

			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;

					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			if (gp_fverbose("d1"))
			{
				printf ("%3d |%s|\n",ql,b1);
			}

			fnp[ql] = ( fnptr ) malloc ( sizeof (node));
			if ( fnp[ql] == NULL)
				error(902);

			sprintf ( (*fnp[ql]).l,"%s",b1);
			(*fnp[ql]).f1 = 0;
			(*fnp[ql]).f2 = 0;
			(*fnp[ql]).f3 = 0;

			qf_lin++;
			ql++;
		}
	}

	if (gp_fverbose("d2"))
	{
		printf ("Cantidad de lineas en source ql       : %6d\n",ql);
	}


	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< ql; i++)
		{
			printf ("i: %3d  (*fnp[i]).fx  %2d %2d %2d  (**fnp[i]).l: |%s| \n",
				i,(*fnp[i]).f1, (*fnp[i]).f2, (*fnp[i]).f3, (*fnp[i]).l );
		}

		printf ("\n");
	}
}

#endif
/* pf_load */








/*
 * -----------------------------------------------------------------------------------
 *
 *	graba un archivo de vector
 *
 * -----------------------------------------------------------------------------------
 */


/* pf_write */
#if 1



int	pf_write()
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	/* cantidad de lineas en el archivo  */


	for (i=0; i< qf_lin; i++)
	{
		fprintf (hfout,"%s\n", (*fnp[i]).l );
	}

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de lineas en source qf_lin   : %6d\n",qf_lin);
	}


}

#endif
/* pf_write */




















/*
 * -----------------------------------------------------------------------------------
 *
 *	busco_use
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	qf_lin		tiene la cantidad de lineas en el source
 *	(*fnp[i]).l	tiene la linea
 *	(*fnp[i]).f1	tiene valor aux f1
 *	(*fnp[i]).f2	tiene valor aux f2
 *	(*fnp[i]).f3	tiene valor aux f3
 *
 */


int	busco_use()
{


	int	i,j,k;
	char	b1[MAXR];
	char	b2[MAXR];


	printf ("Entro a busco_use, qf_lin  %d\n\n",qf_lin);

	for (i=0; i< qf_lin; i++)
	{
		strcpy(b1,(*fnp[i]).l );
		strcpy(b2,"set original");

		k = 0;
		k = encuentro_use(b1,b2);


		if ( k )
		{
			printf ("Encontre en lin: %6d \n",i);
			printf ("|%s|\n",b1);
			printf ("|%s|\n",b2);
			printf ("- - - \n");
		}
	}

	printf ("Salgo de busco_use \n\n");
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	encuentro_use
 *
 * -----------------------------------------------------------------------------------
 */




int	encuentro_use(s1,s2)
char	*s1;
char	*s2;
{
	char b1[MAXR];
	static char b2[MAXR];

	int	p1,p2,p3;
	int	flag;


	strcpy(b1,pasar_a_minusc(s1));
	strcpy(b2,"_");
	flag = 0;

	for (p1=0; b1[p1]; p1++)
	{

		if (!linea_comentario(b1))
		{


		if (!strncmp(b1+p1,"use",3) )
		{
			flag = 1;
			p1 = p1 + 3;
			p2 = 0;

			while (b1[p1] == ' ' || b1[p1] == 9 )
			{	p1++;
				if (p1 == MAXR )
					error (502);
			}
			
			while ( is_use_valid( b1[p1+p2] ) ) 	/* condicion de palabra valida para nombre de use */
			{
				p2++;
				if (p2 == MAXR)
					error (503);
			}

			strncpy (b2,b1+p1,p2);
			strncpy (s2,b1+p1,p2);

			if (gp_fverbose("d1"))
			{
				printf ("test: \n");
				printf ("b1 |%s| \n",b1);
				printf ("b2 |%s| \n",b2);
				printf ("s2 |%s| \n",s2);
				printf ("p1 %d \n",p1);
				printf ("p2 %d \n",p2);
				printf ("- - - \n");
			}


		}

		}


	}


	return flag;
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	encuentro_use
 *
 * -----------------------------------------------------------------------------------
 */

int	linea_comentario(s)
char	*s;
{
	int flag;

	flag = 0;

	if (s[0] == 'C' || s[0] == 'C')
		flag = 1;

	if (s[0] == '!' )
		flag = 1;

	return flag;
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	is_use_valid
 *
 *	funcion para determinar si caracter es valido en nombre para 'use'
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	is_use_valid(c)
char	c;
{
	int x;

	x=0;
	if ( c >= 'a' && c <= 'z' )
		x=1;
	if ( c >= '0' && c <= 'z' )
		x=1;
	if ( c == '_' || c == '-' )
		x=1;


	return x;
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	(MMM)
 *
 *	pro_tool  1
 *
 *	tools  ...
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *
 *	tool 1
 *
 *	carga dos listas l1 l2, genera listados l2 en l1 l_si l_no
 *	archivos inp / out
 *	inp list_make    (listado de fuentes en make )
 *	in2 list_src     (listado de src en repo )
 *	out l_si         (listado de lineas en l2 que SI estan en l1)
 *	ou2 l_no         (listado de lineas en l2 que NO estan en l1)
 * 	
 *	-m               las comparaciones se hacen en lower case
 *                       (pero los listados se generan con case original )
 */



int	pro_tool1()
{
	int	i,j,k;
	char	b1[MAXB];

	char	z[MAXV];
	sprintf (z,"tool1");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* chequeo de parametros minimos */
	if (!ffinp || !ffin2 || !ffout || !ffou2 )
		gp_uso(0);


	/* cargo lista l1 */
	fnq1 = &fnp[0];
	qf_load(hfinp,fnq1,&qf_lin);

	if (gp_fverbose("d3") )
	{
		for (i=0; i< qf_lin; i++)
			printf ("%3d |%s|\n", i, fnp[i]->l );
	}

	/* cargo lista l2 */
	fnq2 = &fnf[0];
	qf_load(hfin2,fnq2,&qf_fen);

	if (gp_fverbose("d3") )
	{
		for (i=0; i< qf_fen; i++)
			printf ("%3d |%s|\n", i, fnf[i]->l );
	}


	if (gp_fverbose("d3"))
	{
		printf ("\n");
		printf ("Cant de lineas lista 1 %4d\n",qf_lin);
		printf ("Cant de lineas lista 2 %4d\n",qf_fen);
		printf ("\n");
	}

	/* revisa lista 2 ... si hay paths u otras cosas ademas del nombre */
	check_fnames();

	/* revisa las condiciones esta/ no esta l2 en l1 */
	check_c1();

	/* graba resultados */
	write_result();

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}



/*
 * -----------------------------------------------------------------------------------
 *
 *	check_fnames
 *
 *	revisa si hay otras cosas ademas del nombre de archivo
 *
 * -----------------------------------------------------------------------------------
 */


int	check_fnames()
{
	int	i,j,k;
	int	f1,f2;
	char	b1[MAXB];
	char	b2[MAXB];

	char	z[MAXV];
	sprintf (z,"check_names");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}
		
	for (i=0; i< qf_fen; i++)
	{
		strcpy (  (*fnf[i]).l , extract_fname ( (*fnf[i]).l )  );
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}





/*
 * -----------------------------------------------------------------------------------
 *
 * 	extract_fname
 *
 * -----------------------------------------------------------------------------------
 */

char	*extract_fname(s)
char	*s;
{
	static char b[MAXB];
	int 	i,j,k;

	strcpy(b,s);
	for (i=strlen(b) - 1, j=0; i>= 0 && !j; i-- )
		if ( b[i] == ' ' || b[i] == '\\' || b[i] == '/' )
			j=i;

	if (j==0)
		k=0;
	else	
		k=j+1;

	return b+k;
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	write_result
 *
 *	graba resultados de la comparacion de listas
 *
 * -----------------------------------------------------------------------------------
 */

int	write_result()
{
	int	i,j,k;
	int	f1,f2;
	char	b1[MAXB];
	char	b2[MAXB];

	char	z[MAXV];
	sprintf (z,"write_result");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}
		
	for (i=0; i< qf_fen; i++)
	{
		if ( (*fnf[i]).f1 == 1)
			fprintf (hfout,"%s\n", (*fnf[i]).l );
		if ( (*fnf[i]).f1 == 2)
			fprintf (hfou2,"%s\n", (*fnf[i]).l );
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}	





/*
 * -----------------------------------------------------------------------------------
 *
 *	check_c1
 *
 *	verifica condiciones entre dos listas 
 *
 * -----------------------------------------------------------------------------------
 */



int	check_c1()
{
	int	i,j,k;
	int	f1,f2;
	char	b1[MAXB];
	char	b2[MAXB];

	char	z[MAXV];
	sprintf (z,"check_c1");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}
		

	for (i=0; i< qf_fen; i++)
	{
		for (j=0, f1=1; f1 && j< qf_lin ; j++)
		{
			if (gp_minusculas)
			{
				strcpy(b1,pasar_a_minusc ( (*fnp[j]).l ) );
				strcpy(b2,pasar_a_minusc ( (*fnf[i]).l ) );
			}
			else
			{
				strcpy(b1, (*fnp[j]).l );
				strcpy(b2, (*fnf[i]).l );
			}

	printf ("cmp: |%s| |%s| \n",b1,b2);


			if (!strcmp ( b1 , b2 ) )
			{
printf ("--- 1\n");
				(*fnf[i]).f1 = 1;
				f1 = 0;

				if (gp_fverbose("d3"))
					printf ("esta si: |%s| \n", (*fnf[i]).l );
			}
		}

		if (f1 && j == qf_lin)
		{
			(*fnf[i]).f1 = 2;

			if (gp_fverbose("d3"))
				printf ("esta no: |%s| \n", (*fnf[i]).l );
		}
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_tool  2
 *
 *	tools  ...
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	tool 2
 *
 *	descrip
 *
 */

int	pro_tool2()
{
	char	z[MAXV];
	sprintf (z,"tool2");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* chequeo condiciones para seguir */
	if (!ffinp || !ffout )
		gp_uso(0);

	/* cargo mk a memo */
	load_makefile(hfinp);

	/* proceso mk */
	proc_makefile();

	/* elimino dups */
	filter_makefile();

	/* pidio minusculas */
	if (gp_minusculas)
		to_min();


	/* grabo listado de files encontrados */
	write_makefile(hfout);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}







/*
 * -----------------------------------------------------------------------------------
 *
 *	load_makefile  ...
 *
 * -----------------------------------------------------------------------------------
 */

#if 0


/*
 * 	Variables y estructs para cargar todo un archivo en memoria
 *
 */


#define	MAX_FSRC	10000		/* 10000 lineas de codigo ... seran suf ? */
int	qf_lin;				/* lineas en file */

int	pf_load();			/* proceso de carga del file */
int	pf_write();			/* proceso de write del file */

typedef	struct	tfn	*fnptr;
typedef	struct	tfn
{	char	l[MAXR];		/* por ahora, despues malloc */
	int	f1;			/* usos varios */
	int	f2;
	int	f3;
}	node;

fnptr	fnp1,fnp2,*fnpa;

fnptr	fnp[MAX_FSRC];			/* vector de punteros a lineas de source */


#endif

/*
 *
 *	load_makefile
 *
 *	descrip
 *
 */

int	load_makefile(fpr)
FILE	*fpr;
{
	char	b1[HUGE];
	int	flag,f1;
	int	i,j,k;
	int	q_mk;
	int	ql;
	int	mas_l;


	char	z[MAXV];
	sprintf (z,"load_makefile");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


#if 1

	/* cantidad de lineas en file  */
	mas_l=0;
	q_mk = 0;
	ql   = 0;

	qf_lin=0;

	while (fgets(b1,HUGE,fpr) != NULL)
	{

		/* temita de los archivos con line feed */
		if (b1[strlen(b1)-2] == 13)
			b1[strlen(b1)-2] = 32;

		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n')
					b1[j]=0,flag=1;

			if (gp_fverbose("d3"))
			{
				printf ("%3d |%s|\n",ql,b1);
			}


			if (strlen(b1) > mas_l)
				mas_l = strlen(b1);


			fnp[qf_lin] = ( fnptr ) malloc ( sizeof (node));
			if ( fnp[qf_lin] == NULL)
				error(904);

			sprintf ( (*fnp[qf_lin]).l,"%s",b1);
			(*fnp[qf_lin]).f1 = 0;
			(*fnp[qf_lin]).f2 = 0;
			(*fnp[qf_lin]).f3 = 0;
			

			qf_lin++;
			q_mk++;
		}
		ql++;
	}

	if (gp_fverbose("d2"))
	{
		printf ("Cantidad de lineas cargadas : %6d\n",ql);
		printf ("Cantidad de lineas utiles   : %6d\n",q_mk);
		printf ("Linea mas larga encontrada  : %6d\n",mas_l);
		printf ("\n");
	}

#if 1

	if (gp_fverbose("d4"))
	{
		printf ("\n\nComprobando integridad del makefile : \n\n");

		for (i=0; i< qf_lin; i++)
		{
			printf ("%04d |%s|\n",i,(*fnp[i]).l );
		}
		printf ("\n");
	}

#endif



#endif


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	proc_makefile  ...
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	proc_makefile
 *
 *	tomo archivo formato makefile y extraigo todos los files en dependencias
 *	
 *	qf_fen    es la cantidad de files extraigs 
 */

int	proc_makefile()
{
	int	i,j,k;
	int	f1,f2,f3;
	int	p1,p2,p3;
	int	mx;
	char	b1[HUGE];
	char	b2[MAXB];


	char	z[MAXV];
	sprintf (z,"proc_makefile");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	qf_fen = 0;


	for (i=0; i< qf_lin; i++ )
	{
		strcpy(b1,(*fnp[i]).l );
		mx=strlen(b1);
	
		f1=1;
		j=0;

		while (f1)
		{
			if (es_char_v (b1[j]) )
			{
				f2=1;
				p1=0;

				memset(b2,0,sizeof(b2));

				while(f1 && f2)
				{
					if(es_char_v(b1[j]) )
					{
						b2[p1] = b1[j];
						p1++;
						j++;
						if (j == mx)
							f2=0,f1=0;
					}
					else
					{	f2=0;
						if (gp_fverbose("d3") )
						{
							printf ("encontre: |%s| \n",b2);
						}
						if (es_str_v(b2))
						{
#if 0
							fprintf (fpr,"%s\n",b2);	
#endif
							fnf[qf_fen] = ( fnptr ) malloc ( sizeof (node));
							if ( fnf[qf_fen] == NULL)
								error(905);

							sprintf ( (*fnf[qf_fen]).l,"%s",b2);
							(*fnf[qf_fen]).f1 = 1;	
							(*fnf[qf_fen]).f2 = 0;
							(*fnf[qf_fen]).f3 = 0;

							qf_fen++;
							
						}
					}
				}	
			}
			else
			{
				j++;
				if (j == mx)
					f1=0;
			}

		}
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	write_makefile  ...
 *
 * -----------------------------------------------------------------------------------
 */


int	write_makefile(fpr)
FILE	*fpr;
{
	int	i,jmk;

	char	z[MAXV];
	sprintf (z,"write_makefile");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	for (i=0; i< qf_fen; i++)
	{
		if ( (*fnf[i]).f1   )
		{
			fprintf (fpr,"%s\n",(*fnf[i]).l);
		}
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	to_min   ...
 *
 * -----------------------------------------------------------------------------------
 */


int	to_min()
{

	int	i,j,k;
	int	f1,f2;

	char	z[MAXV];
	sprintf (z,"to_min");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* to min */
	for (i=0; i< qf_fen; i++)
	{
		strcpy( (*fnf[i]).l,  pasar_a_minusc( (*fnf[i]).l )  );
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	filter_makefile  ...
 *
 * -----------------------------------------------------------------------------------
 */


int	filter_makefile()
{

	int	i,j,k;
	int	f1,f2;

	char	z[MAXV];
	sprintf (z,"filter_makefile");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


#if 1

	i=0;
	f1=1;

	while (f1)
	{
		if( (*fnf[i]).f1 )
		{
			for (j=i+1; j< qf_fen; j++)
			{
				if (gp_fverbose("d4"))
				{
					printf ("cmp: i %3d j %3d  s1-s2   |%s|  |%s| \n",
						i,j,(*fnf[i]).l,(*fnf[j]).l );
				}

				if ( !strcmp( (*fnf[i]).l , (*fnf[j]).l )  )
					(*fnf[j]).f1 = 0;
			}
		}

		i++;
		if (i == qf_fen)
			f1=0;
	}

#endif


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}


}


/*
 * -----------------------------------------------------------------------------------
 *
 *	es_char_v  ...
 *
 * -----------------------------------------------------------------------------------
 */




int	es_char_v(c)
char	c;
{

	int x;

	x=0;

	if (c >= 'a' && c <= 'z' || c>= 'A' && c <= 'Z' )
		x = 1;

	if (c == '_' || c == '.' )
		x = 1;

	if (c >= '0' && c <= '9' )
		x = 1;

#if 0
	/* agrego los que vienen de analizar fuentes fortran */
	if (c == '\'' || c == '%' || c == '!' || c == '*'  )
		x = TC_CVR;

	if (c == '"' || c == '&' || c == '+' || c == '=' || c == '>' || c == '<' || c == '?' )
		x = TC_CVR;
#endif

	return x;
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	es_str_v  ...
 *
 * -----------------------------------------------------------------------------------
 */



int	es_str_v(s)
char	*s;
{

	int	i,j,k;
	int	p1,p2;
	int	f1,f2;
	int	mx;
	int 	x;
	char	b1[MAXB];
	char	b2[MAXB];

	x=1;
	i=0;
	f1=1;
	strcpy(b1,s);
	mx=strlen(b1);

	/* el primer caracter no puede ser punto */
	j=0;

	if (f1)
	{
		if ( b1[j] == '.')
		{	x=0;
			f1=0;
		}
	}

	/* el ultimo caracter no puede ser punto */
	j=0;

	if (f1)
	{
		if ( b1[mx-1] == '.')
		{	x=0;
			f1=0;
		}
	}

	/* tiene q tener un punto */
	j=0;

	if (f1)
	{
		for (k=0; k< mx; k++)
		{
			if (b1[k] == '.')
			{	j++;
				strcpy(b2,b1+k);
			}
		}

		if (j != 1)
		{	x=0;
			f1=0;
		}
	}



	/* la extension no puede ser de la forma .x (largo 1)  */
	if (f1)
	{
		if ( strlen(b2) == 2 )
		{	x=0;
			f1=0;
		}
	}



	/* la extension debe ser una de ... */
	if (f1)
	{
		f2=0;

		if (!strcmp(b2+1,"for") )
			f2=1;
		if (!strcmp(b2+1,"FOR") )
			f2=1;
		if (!strcmp(b2+1,"f90") )
			f2=1;
		if (!strcmp(b2+1,"F90") )
			f2=1;
		if (!strcmp(b2+1,"f95") )
			f2=1;
		if (!strcmp(b2+1,"F95") )
			f2=1;
		if (!strcmp(b2+1,"mon") )
			f2=1;
		if (!strcmp(b2+1,"MON") )
			f2=1;

		if (!f2)
		{
			x=0;
			f1=0;
		}


	}

	/* si es archivo valido ... */
	if (f1)
	{
		if (gp_fverbose("d3"))
			printf ("extension: |%-40s| |%s|\n",b1,b2);
	}


	return x;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	(MMM)
 *
 *	pro_tool  3
 *
 *	tools  ...
 *
 * -----------------------------------------------------------------------------------
 */

/*




/*
 *
 *	tool 3
 *
 *	descrip
 *
 */

int	pro_tool3()
{
	int	i,j,k;
	char	b1[MAXB];

	char	z[MAXV];
	sprintf (z,"tool3");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	fnq1 = &fnp[0];
	qf_load(hfinp,fnq1,&qf_lin);


	for (i=0; i< qf_lin; i++)
	{
		strcpy(b1,pasar_a_minusc( (*fnp[i]).l ));

		if (gp_fverbose("d1"))
			printf ("%4d |%s| \n", i,b1 );

		fprintf (hfout,"%s\n",b1);
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	tool4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	Descrip
 *
 * 	carga un src en memoria, en vector a pnt de esctructs
 *
 *	- cambia el formato de declaracion de variables
 *
 */


#if 1

int	pro_tool4()
{
	int	i,j,k;
	int	ql_ini,ql_fin;
	char	b1[MAXB];

	char	z[MAXV];
	sprintf (z,"tool4");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* chequeamos reqs */
	if (!ffinp || !ffout || !ffaux )
		gp_uso(11);

	/* cargamos file en memo */
	fnq1 = &fnp[0];
	qf_load(hfinp,fnq1,&ql_ini);


	/* mientras que no cambie la cant de lineas !!! */
	ql_fin=ql_ini;

	
	/* 1 - cambio los formatos de declaracion de variables */
	cfor_vars(&ql_ini,&ql_fin);


	/* grabo file */
	for (i=0; i< ql_fin; i++)
	{
		strcpy(b1, (*fnp[i]).l );

		if (gp_minusculas)
			strcpy( b1 , pasar_a_minusc( (*fnp[i]).l )  );

		fprintf (hfout,"%s\n",b1);
	}




	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}

#endif


/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_vars
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	f1
 *	1 - integer
 *	2 - logical
 * 	3 - real
 *
 */


int	cfor_vars(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k;
	int	p1,p2;
	int	f1,f2,f3;
	int	qi,qf;

	char	b1[MAXB];
	char	b2[MAXB];



	qi = *ql_i;
	qf = *ql_f;

	for (i=0; i< qi; i++)
	{
		strcpy(b1, (*fnp[i]).l );

		l_pars(i,&q_tk);

		if ( f1=tiene_dec_var1() )
		{
			/* caso int, log, real */
			if (f1 == 1 || f1 == 2 || f1 == 3)
			{
				/* si no lo pude arreglar, encontre un caso no contemplado !!*/
				if (! fix_dec_var1 () )
					error(701);
				else
				{
					if (gp_fverbose("d3"))
					{	printf ("fixed %d ! \ncfor:  linea %4d #tk %4d |%s|\n",f1,i,q_tk,b1);
						for (j=0; j<q_tk; j++)
						{
							printf ("TK: %3d %3d f1: %d |%s|\n",j,strlen(tk[j]),f1,tk[j]);
						}
					}
				}
			}

			/* caso character */
			if (f1 == 4 )
			{
				/* si no lo pude arreglar, encontre un caso no contemplado !!*/
				if (! fix_dec_var2 () )
					error(702);
				else
				{
					if (gp_fverbose("d3"))
					{	printf ("fixed %d! \ncfor:  linea %4d #tk %4d |%s|\n",f1,i,q_tk,b1);
						for (j=0; j<q_tk; j++)
						{
							printf ("TK: %3d %3d f1: %d |%s|\n",j,strlen(tk[j]),f1,tk[j]);
						}
					}
				}
			}
				
		}

		if (gp_fverbose("d4"))
		{
			printf ("cfor:  linea %4d #tk %4d |%s|\n",i,q_tk,b1);
			for (j=0; j<q_tk; j++)
			{
				printf ("TK: %3d %3d f1: %d |%s|\n",j,strlen(tk[j]),f1,tk[j]);
			}
		}

		/* armo la linea de nuevo con todos los tokens */
		memset (b2,0,MAXB);
		for (j=0; j< q_tk; j++)
			strcat (b2,tk[j]);
		strcpy ( (*fnp[i]).l, b2);

	}

}



int	fix_dec_var1()
{
	int	i,j,k,i1;
	int	f1;
	int	minus, aster, kind, opt, inten, alloca, save, func, cont;
	int	n_minus, n_aster, n_kind, n_opt, n_inten, n_alloca, n_save, n_func, n_cont;
	int	ult, n_type;
	int	f_aster;

	char	ca;
	char	varb[MAXB];


	alloca = 0;
	func  = 0;
	inten = 0;
	aster = 0;
	save  = 0;
	kind  = 0;
	ult   = 0;
	func  = 0;
	opt   = 0;
	cont  = 0;
	n_type = 0;


	f_aster = 1;

	for (i=0; i< q_tk; i++)
	{	
		if (!strcmp("integer",tk[i]) )
			minus = 1, n_type = i;

		if (!strcmp("INTEGER",tk[i]) )
			minus = 0, n_type = i;

		if (!strcmp("logical",tk[i]) )
			minus = 1, n_type = i;

		if (!strcmp("LOGICAL",tk[i]) )
			minus = 0, n_type = i;

		if (!strcmp("real",tk[i]) )
			minus = 1, n_type = i;

		if (!strcmp("REAL",tk[i]) )
			minus = 0, n_type = i;

		if (!kind && !strcmp(":",tk[i]) && !strcmp(":",tk[i+1]) )
			kind=1,n_kind = i;

		if (!strcmp("*",tk[i]) && ( i - n_type < 4 ) )
			aster = 1, n_aster=i;

		if (!strcmp("optional",tk[i]) || !strcmp("OPTIONAL",tk[i]) )
			opt = 1, n_opt = i, ult=i;

		if (!strcmp("intent",tk[i]) || !strcmp("INTENT",tk[i]) )
		{	inten = 1, n_inten = i, ult=i;
			for (k=i+1; k< i+8; i++)
				if (tk[k][0] == ')')
					ult=k;
		}

		if (!strcmp("allocatable",tk[i]) )
			alloca = 1, n_alloca = i, ult=i;

		if (!strcmp("save",tk[i]) )
			save = 1, n_save = i, ult=i;

		if (!strcmp("function",tk[i]) )
		{	func = 1, n_func = i;
		}

		if (!strcmp("&",tk[i]) )
			cont = 1, n_cont = i;
	}


	strcpy(varb,tk[n_type]);
	if (minus)
		strcpy(varb,pasar_a_minusc(tk[n_type]));



		
	if (gp_fverbose("d4"))
	{
		for ( i1 = 0 ; i1< 8; i1++)
			printf ("Token-1-  %d |%s| \n",i1,tk[i1]);
	}

	i = n_type;
	f1=0;

	/* esta en minuscula y tiene asterisco valor  */
	if ( !f1 && aster )
	{

		ca = tk[n_aster+1][0];

		if (ca == '1' || ca == '2' || ca == '4' || ca == '8' )
		{
			sprintf (tk[i] , "%s (kind=%c)",varb,ca);
			sprintf (tk[i+1]," ");
					
			if (!kind)
			{
				if (ult == 0)
					ult = i+1;

				if (!func)
				{	
					sprintf (tk[ult],"%s"," :: ");
					ca = tk[ult+1][0];
					if (ca == '1' || ca == '2' || ca == '4' || ca == '8' )
						sprintf (tk[ult+1]," ");
				}

				if (func)
				{
					ca = tk[ult+1][0];
					if (ca == '1' || ca == '2' || ca == '4' || ca == '8' )
						sprintf (tk[ult+1]," ");
				}
			}

			f1 = 1;
		}
	}


	/* esta en minuscula y no tiene asterisco valor */
	if ( !f1 && !aster)
	{
		if (!kind)
		{
			if (ult == 0)
				ult = i+1;

			if (!func)
				sprintf (tk[ult],"%s"," :: ");
		}

		f1 = 1;
	}

	return(f1);
}







int	fix_dec_var2()
{
	int	i,j,k,i1;
	int	f1;
	int	minus, aster, kind, opt, inten, alloca, save, func, cont;
	int	n_minus, n_aster, n_kind, n_opt, n_inten, n_alloca, n_save, n_func, n_cont;
	int	ult, n_type;
	int	f_aster;

	char	ca;
	char	nr[16];
	char	varb[MAXB];


	alloca = 0;
	func  = 0;
	inten = 0;
	aster = 0;
	save  = 0;
	kind  = 0;
	ult   = 0;
	func  = 0;
	opt   = 0;
	cont  = 0;
	n_type = 0;


	f_aster = 1;

	for (i=0; i< q_tk; i++)
	{	
		if (!strcmp("character",tk[i]) )
			minus = 1, n_type = i;

		if (!strcmp("CHARACTER",tk[i]) )
			minus = 0, n_type = i;

		if (!kind && !strcmp(":",tk[i]) && !strcmp(":",tk[i+1]) )
			kind=1,n_kind = i;

		if (!n_aster && !strcmp("*",tk[i]) && ( i - n_type < 4 ) )
			aster = 1, n_aster=i;

		if (!strcmp("optional",tk[i]) || !strcmp("OPTIONAL",tk[i]) )
			opt = 1, n_opt = i, ult=i;

		if (!strcmp("intent",tk[i]) || !strcmp("INTENT",tk[i]) )
		{	inten = 1, n_inten = i, ult=i;
			for (k=i+1; k< i+8; i++)
				if (tk[k][0] == ')')
					ult=k;
		}

		if (!strcmp("allocatable",tk[i]) )
			alloca = 1, n_alloca = i, ult=i;

		if (!strcmp("save",tk[i]) )
			save = 1, n_save = i, ult=i;

		if (!strcmp("function",tk[i]) )
		{	func = 1, n_func = i;
		}

		if (!strcmp("&",tk[i]) )
			cont = 1, n_cont = i;
	}


	strcpy(varb,tk[n_type]);
	if (minus)
		strcpy(varb,pasar_a_minusc(tk[n_type]));

		
	if (gp_fverbose("d3"))
	{
		for ( i1 = 0 ; i1< 8; i1++)
			printf ("Token-1-  %d |%s| \n",i1,tk[i1]);
	}

	i = n_type;
	f1=0;

	/* esta en minuscula y tiene asterisco valor  */
	if ( !f1 && aster )
	{
		strcpy(nr,tk[n_aster+1]);

		if (es_num_tk(nr) )
		{
			sprintf (tk[i] , "%s (len=%s)",varb,nr);
			sprintf (tk[i+1]," ");
					
			if (!kind)
			{
				if (ult == 0)
					ult = i+1;

				if (!func)
				{	
					sprintf (tk[ult],"%s"," :: ");
					ca = tk[ult+1][0];
					if (es_num_tk(nr))
						sprintf (tk[ult+1]," ");
				}

				if (func)
				{
					ca = tk[ult+1][0];
					if (es_num_tk(nr))
						sprintf (tk[ult+1]," ");
				}
			}

			f1 = 1;
		}


		if (tk[n_aster+1][0] == '(' && tk[n_aster+2][0] == '*' && tk[n_aster+3][0] == ')' )
		{
			sprintf (tk[n_aster+1]," ");
			tk[n_aster+2][0] = 0;
			tk[n_aster+3][0] = 0;
	

			/* es de la forma character*(*) ... */
			sprintf (tk[i] , "%s (len=*)",varb);
			sprintf (tk[i+1]," ");

			if (!kind)
			{
				if (ult == 0)
					ult = i+1;

				if (!func)
				{	
					sprintf (tk[ult],"%s"," :: ");
					ca = tk[ult+1][0];
					if (es_num_tk(nr))
						sprintf (tk[ult+1]," ");
				}

				if (func)
				{
					ca = tk[ult+1][0];
					if (es_num_tk(nr))
						sprintf (tk[ult+1]," ");
				}
			}

			f1 = 1;
		}
	}


	/* esta en minuscula y no tiene asterisco valor */
	if ( !f1 && !aster)
	{
		if (!kind)
		{
			if (ult == 0)
				ult = i+1;

			if (!func)
				sprintf (tk[ult],"%s"," :: ");
		}

		f1 = 1;
	}


	return(f1);
}

















int	tiene_dec_var1()
{
	int	i,j;
	int	f1,f2;

	f1=0;
	
#if 0
	if (tk[0][0] == '!')
		f1 = 1;
#endif


	for (i=0; !f1 && i<q_tk; i++)
	{
		if ( !strcmp("integer", pasar_a_minusc( tk[i] )) )
			f1 = 1;
		if ( !strcmp("logical", pasar_a_minusc( tk[i] )) )
			f1 = 2;
		if ( !strcmp("real", pasar_a_minusc( tk[i] )) )
			f1 = 3;
		if ( !strcmp("character", pasar_a_minusc( tk[i] )) )
			f1 = 4;
	}

	return f1;
}







/*
 * -----------------------------------------------------------------------------------
 *
 * 	linea_vacia_for 
 *
 *	edicion especial para sources fortran
 *	Determina si una linea esta vacia (generalmente, para lineas leidas de files)
 *	La linea NO esta vacia si contiene al menos 1 caracter distinto de
 *	' ' 	blanco
 *	'\t' 	tab
 *	'\n'	new line
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	linea_vacia_for(s)
char	*s;
{
	int f1;
	int i,flag;

	f1 = 1;
	i=0;
	flag=1;

	if (f1 && s[0] == '!')
		f1=0;

	for (i=0; f1 && flag && i< strlen(s); i++)
		if (s[i] != ' ' && s[i] != '\t' && s[i] != '\n' )
			flag=0 , f1=0;

		
	return flag;
}














/*
 * -----------------------------------------------------------------------------------
 *
 *	tool5
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	Descrip
 *
 * 
 *
 */


int	pro_tool5()
{

	int	px,py;

	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	q_lin;
	int	q_tk;
	int	p1,p2,p3,p4;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	tk[MAXT][MAXB];

	char	z[MAXV];
	sprintf (z,"tool5");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffaux)
		gp_uso(0);

/* tool5 */
#if 1

	/* si encuentro caracteres no considerados para el parser, avisar al final de todo el proceso */
	flag_caracteres = 0;

	q_lin=0;
	memset(b1,32,MAXB);
	memset(b2,0,MAXB);

	while (fgets(b1, MAXB, hfinp) != NULL)
	{
	    /* proceso todas las lineas */
	    if ( 1 )
	    {
#if 1
		/* blancos al final */
		for (i=strlen(b1)-1, f4=1 ; i && f4 ; i-- )
			if (b1[i] == ' ' || b1[i] == '\n' || b1[i] == '\r' )
				b1[i]=0;
			else
				b1[i+1]='\n', b1[i+2]=0, f4=0;
#endif
	
		f1=1;

		if (gp_fverbose("d3"))
		{
			printf ("Linea  : %d \n\n",q_lin);
			printf ("Buffer :|%s|\n",  b1);
		}


		/* comienzo parser de tokens */
		p1=0;
		q_tk=0;

		while ( f1 )
		{
			/* controlamos cantidad de tokens ... */
			if (q_tk > MAXT-10)
			{	error(501);
			} 

			j=tipo_char(b1[p1]);

			switch (j)
			{
				/* otro caracter !!! */
				case TC_RST:
					flag_caracteres=1;
					if (gp_fverbose("d1"))
					{
						printf ("Caracter no definido para parser: |%c| |%d|\n",b1[p1],b1[p1]);
						if (ffaux)
							fprintf (hfaux,"Caracter no definido %d %c\n",b1[p1],b1[p1]);
							
					}
					p1++; 
					break; 

				/* letras */
				case TC_LET:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_LET || \
					        (j == TC_NUM && !char_demed(b1[p1-1])  ) )
						tk[q_tk][p2++]=b1[p1++];
					tk[q_tk][p2]=0;
					q_tk++;
					break;

				/* numeros tenemos que contemplar 3.3 o 3,3 !! */
				case TC_NUM:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_NUM || \
						(tipo_char(b1[p1]) == TC_PNT && tipo_char(b1[p1+1]) == TC_NUM ) || \
						( (b1[p1]) == ',' && tipo_char(b1[p1+1]) == TC_NUM ) )
					{	tk[q_tk][p2]=b1[p1];
						p1++;
						p2++;
					}
					tk[q_tk][p2]=0;
					q_tk++;
					break;

				/* choto */
				case TC_CHO:
					/* opcion: no los guardo */
					tk[q_tk][0]=' ';
					tk[q_tk][1]=0;
					p1++;
					f1=0;
					break;

#if 0
					while ( (j=tipo_char(b1[p1])) == TC_CHO)
					{	printf ("estoy perdido en choto %d \n",p1);
					 	p1++;	
					}

#endif
					break;


				/* blanco o tab */
				case TC_BLA:
#if 0
					/* opcion: no los guardo */
					while ( (j=tipo_char(b1[p1])) == TC_BLA)
					       p1++;	
#endif
					/* opcion: los guardo */
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;

					break;

				/* coma */
				case TC_CCE:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* punto */
				case TC_PNT:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis abre */
				case TC_PAA:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis cierra */
				case TC_PAC:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				case TC_EOL:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					p1++;
					f1=0;
					break;

				/* caracteres varios */
				case TC_CVR:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				default:
					printf ("Default, algo salio mal  !!!\n\n");
					f1=0;
					break;
			}

		} /* while */


		if (gp_fverbose("d3"))
			printf ("termine de parsear linea \n");


		/* verifico si hay que sacar output  en minusculas */
		if (gp_minusculas)
		{
			for (j=0; j< q_tk; j++)
				strcpy(tk[j],pasar_a_minusc(tk[j]));
		}


		/* salida en formato token columnar */
		if (gp_fsentencia == 0)
		{

			/* pidio nivel de descripcion en salida ... agrego la sentencia */
			if (gp_niveldes)
				fprintf (hfout,"%s\n",b1);

			/* grabo los tokens encontrados */
			for (j=0; j< q_tk; j++)
			{
				switch (gp_niveldes)
				{

					case 0:
						fprintf (hfout,"%s\n",tk[j]);
						break;

					case 1:
						fprintf (hfout,"%3d,%s\n",j,tk[j]);
						break;

					default:
						fprintf (hfout,"%s\n",tk[j]);
						break;
				}


				if (gp_fverbose("d3"))
					printf ("%3d,%s\n",j,tk[j]);

			}

		}



		/* salida en formato sentencia */
		if (gp_fsentencia == 1)
		{
			/* grabo los tokens encontrados */
			for (j=0; j< q_tk; j++)
			{
				fprintf (hfout,"%s",tk[j]);

				if (gp_fverbose("d3"))
					printf ("%3d,%s\n",j,tk[j]);
			}


			/* se termino la linea */
			fprintf (hfout,"\n");
		}


		for (j=0; j< q_tk; j++)
		{
			strcat (b2,tk[j]);
		}
		strcat (b2,"\n");

		if(gp_fverbose("d3"))
			printf ("strcat: |%s| \n",b2);

		if (gp_fverbose("d3"))
		{
			printf ("\n");
		}



		/* 
		 * Termine todo lo que tenia que hacer con esta linea,
		 * sumo lineas 
		 *
		 */

		memset(b1,32,MAXB);
		memset(b2,0,MAXB);
		q_lin++;


	    } /* if ... no esta vacia la linea */

	}  /* while fgets ... */

	
	if (gp_fverbose("d2"))
	{
		printf ("Cant de lineas procesadas %d\n",q_lin);
		printf ("\n\n\n");
	}



#endif
/* tool5 */



	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}


}




/*
 * -----------------------------------------------------------------------------------
 *
 *	tool6
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	Descrip
 *
 * 	carga un src en memoria, en vector a pnt de esctructs
 *
 *	- cambia lineas de continuacion
 *
 */


#if 1

int	pro_tool6()
{
	int	i,j,k;
	int	ql_ini,ql_fin;
	int	lml;

	char	b1[MAXB];

	char	z[MAXV];
	sprintf (z,"tool6");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/*
	 * en aux graba caracteres no contemplados por el parser
	 *
	 */
	if (!ffinp || !ffout || !ffaux)
		gp_uso(12);

	/* cargamos file en memo */
	fnq1 = &fnp[0];
	qf_load(hfinp,fnq1,&ql_ini);

	lml=0;
	for (i=0; i<ql_ini; i++)
	{
		strcpy(b1, (*fnp[i]).l );
		if ( b1[0] != 'C' && b1[0] != 'c' && b1[0] != '!')
		{	
			if ( (k = strlen( b1) ) > lml)
				lml = k;
		}
	}
	if (gp_fverbose("d1"))
	{
		printf ("Linea mas larga: %4d\n\n",lml);
	}

	/* mientras que no cambie la cant de lineas !!! */
	ql_fin=ql_ini;

	
#if 1
	/* 2 - cambio las lineas de continuacion   */
	cfor_lcon(&ql_ini,&ql_fin);
#endif


	/* grabo file */
	for (i=0; i< ql_fin; i++)
	{
		strcpy(b1, (*fnp[i]).l );

		if (gp_minusculas)
			strcpy( b1 , pasar_a_minusc( (*fnp[i]).l )  );

		fprintf (hfout,"%s\n",b1);
	}




	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}

#endif




/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_lcon
 *
 * -----------------------------------------------------------------------------------
 */


int	cfor_lcon(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k;
	int	p1,p2;
	int	f1,f2,f3,f4;
	int	qi,qf;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];


	qi = *ql_i;
	qf = *ql_f;
	memset(b4,' ',MAXB);

	/* ultima linea -1, la ultima linea no puede tener continuacion ... */
	for (i=0; i< qi - 1; i++)
	{
		/* copio linea y linea siguiente */
		strcpy(b1, (*fnp[i]).l );
		strcpy(b2, (*fnp[i+1]).l );

		l_pars(i,&q_tk);

		if(gp_fverbose("d4"))
		{
			for (j=0; j<q_tk; j++)
			{
				printf ("TK: %3d %3d |%s|\n",j,strlen(tk[j]),tk[j]);
			}
			printf ("voy b2: |%s| \n",b2);
		}

		if ( tiene_mas(b2) )
		{
			/* OJO! si tiene mas, pero la linea anterior 
			 * es comentario ... no soportamos ese cambio
			 * parar con error 
			 */

#if 0
			/* si tiene linea comentada con simb cont, le pongo continuacion igual */
			if ( b1[0] == 'C' || b1[0] == 'c' || b1[0] == '!')
			{	printf ("Linea %d tiene comentario y siguiente con + \n",i);
				error(801);
			}
#endif

			if (gp_fverbose("d3"))
			{
				printf ("cfor: mas detectado \n");
				printf ("cfor: %4d #tk %4d |%s|\n",i,q_tk,b1);
				printf ("cfor: %4d     %4d |%s|\n",i+1,0,b2);
			}

			/* tengo que arreglar ambas lineas */
printf (" / / / / b3:1  |%s| q_tk: %d \n",b3,q_tk);
			memset (b3,0,MAXB);
			for (j=0; j< q_tk; j++)
				strcat (b3,tk[j]);
printf (" / / / / b3:2  |%s| q_tk: %d \n",b3,q_tk);

			if (tiene_coment_intermedio (b3) )
			{
printf ("tiene ! intermedio ! |%s| \n",b3);

				for (j=2, f4=1; f4 && j<q_tk; j++)
				{
					if (!strcmp(tk[j],"!"))
					{
						sprintf (tk[j],"& ! ");
printf ("quedo: |%s| \n",tk[j]);
						f4=0;
					}
				}
			}
			else
			{
/* EEE */
							
			memset(tk[q_tk],0,MAXB);
printf (" / / / / b3:3  |%s| q_tk: %d \n",b3,q_tk);
printf ("strelen(b3) : %d\n",strlen(b3));

			strncpy(tk[q_tk++],b4,90-strlen(b3));
printf (" / / / / tk sig1;  |%s| \n",tk[q_tk-1]);
			sprintf (tk[q_tk++],"&");
printf (" / / / / tk sig2;  |%s| \n",tk[q_tk-1]);
			
			}


			/* me falta la linea siguiente con el + */
			/* q&d ... no time */
			f1=1;
			for (j=0; f1 && j<strlen(b2); j++)
			{	
				if (b2[j]=='+')
				{		
					b2[j]=' ';
					f1=0;
				}
			}

				
		}


		/* armo la linea de nuevo con todos los tokens */
printf (" / / / / b3:3  |%s| \n",b3);
		memset (b3,0,MAXB);
		for (j=0; j< q_tk; j++)
			strcat (b3,tk[j]);
		strcpy ( (*fnp[i]).l, b3);
		if (gp_fverbose("d2"))
			printf ("fix: |%s|\n",b3);

printf (" / / / / b3:4  |%s| \n",b3);

		/* copio la segunda linea, sin el mas */
		strcpy ( (*fnp[i+1]).l, b2);
	}
}



int	tiene_coment_intermedio (s)
char	*s;
{
	int 	i,j,k;
	int	f1;

	f1=0;
	for (i=0; i<strlen(s); i++)
		if (s[i] == '!')
			f1 = 1;
	return f1;
}


int	tiene_mas(s)
char	*s;
{
	int	i,j,k;	
	int	f1,f2,f3;
	char	b1[MAXB];

	f1=0;
	f2=1;

	strcpy(b1,s);

	for (i=0; !f1 && f2 && i<strlen(s); i++)
	{
		if (b1[i] != ' ')
		{	if (b1[i] == '+' && b1[i+1] == ' ')
			{
				f1 = 1;
			}
			else
				f2 = 0;
		}
	}

	return f1;
}








/*
 * -----------------------------------------------------------------------------------
 *
 *	A partir de aca ... rutinas generales
 *	No agregar nada particular a un programa determinado (no insistas)
 *
 * -----------------------------------------------------------------------------------
 */




/*
 * -----------------------------------------------------------------------------------
 *
 *	abro_files
 *
 * -----------------------------------------------------------------------------------
 */

int	abro_files()
{

	char	z[MAXV];
	sprintf (z,"abro_files");


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if ( ffinp && ((hfinp = fopen (finp,"r")) == NULL) )
	{
		error(101);
	}

	if ( ffin2 && ((hfin2 = fopen (fin2,"r")) == NULL) )
	{
		error(102);
	}

	if ( ffout && ((hfout = fopen (fout,"w")) == NULL) )
	{
		error(103);
	}

	if ( ffou2 && ((hfou2 = fopen (fou2,"w")) == NULL) )
	{
		error(104);
	}

	if ( ffaux && ((hfaux = fopen (faux,"w")) == NULL) )
	{
		error(105);
	}
	
	if ( fflog && ((hflog = fopen (flog,"w")) == NULL) )
	{
		error(106);
	}

	if ( ffsrc && ((hfsrc = fopen (fsrc,"r")) == NULL) )
	{
		error(111);
	}

	if ( fflst && ((hflst = fopen (flst,"r")) == NULL) )
	{
		error(112);
	}




	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}


}



/*
 * -----------------------------------------------------------------------------------
 *
 *	cierro_files
 *
 *	
 *
 * -----------------------------------------------------------------------------------
 */


int	cierro_files()
{

	sprintf (gp_p,"cierro_files");
	gp_cnt=0;

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[gp_cnt++],gp_p);
	}


	if ( ffinp)
		fclose(hfinp);

	if ( ffin2)
		fclose(hfin2);

	if ( ffout)
		fclose(hfout);

	if ( ffou2)
		fclose(hfou2);

	if ( ffaux)
		fclose(hfaux);

	if ( fflog)
		fclose(hflog);

	if ( ffsrc)
		fclose(hfsrc);

	if ( fflst)
		fclose(hflst);



	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[gp_cnt++],gp_p);
	}



}



/*
 * -----------------------------------------------------------------------------------
 *
 *	load_file
 *
 *	carga un archivo
 *	formato de vector de punteros a estructuras
 *
 * -----------------------------------------------------------------------------------
 */


int	load_file(fpr)
FILE	*fpr;
{
	char	z[MAXV];
	sprintf (z,"load_file");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


/*
 *
 *	load_file
 *
 *	carga un archivo sobre lista de nodos encadenados.
 *	no utilizamos esta version ...
 *	hay memoria, y es menos complicado con vector
 *	de punteros a estructuras malloqueadas... 
 *
 */

#if 0

int	load_file(fpr)
FILE	*fpr;
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	sprintf (gp_p,"load_file");
	gp_cnt=0;


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[gp_cnt++],gp_p);
	}




	/* cantidad de palabras en el diccionario */
	q_wrd = 0;

	/* ptr al contenedor de direccion del ultimo nodo de la cadena */
	npa = (pnodeptr *) &np1;

	ql=0;
	while (fgets(b1,MAXB,fpr) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n')
					b1[j]=0,flag=1;

			if (gp_verbose)
			{
				printf ("%3d |%s|\n",ql,b1);
			}

			*npa = (pnodeptr ) malloc ( sizeof (knode));
			(**npa).wrd = ( char *) malloc(strlen(b1)+1);
			sprintf ( (**npa).wrd,"%s",b1);
			(**npa).num = q_wrd+1;
			(**npa).nx = (pnodeptr) NULL;
			npa = (pnodeptr *) & (*npa)->nx;

			q_wrd++;
			ql++;
		}
	}

	if (gp_verbose)
	{
		printf ("Cantidad de lineas cargadas : %6d\n",q_wrd);
	}


	if (gp_verbose)
	{
		printf ("\n\nComprobando integridad del diccionario : \n\n");
	
		i=0;
		npa = (pnodeptr *) &np1;

		while ( (*npa) != (pnodeptr) NULL )
		{
			printf ("i: %3d  (**npa).num: %3d  (**npa).wrd: |%s|\n",i,(**npa).num,(**npa).wrd);
			npa = (pnodeptr *) & (*npa)->nx;
			i++;
		}
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[gp_cnt++],gp_p);
	}
}

#endif






















/*
 * -----------------------------------------------------------------------------------
 *
 *	mostrar_cargas
 *
 *	mostrar resultados de cargas, si los hubo
 *
 * -----------------------------------------------------------------------------------
 */

int	mostrar_cargas()
{

	int px,py;

	/* proceso  */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra a mostrar cargas \n\n",gp_tm());
	}

	/* si hubo cargas, las muestro aca */

	/* proceso  */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale de mostrar cargas \n\n",gp_tm());
	}

}















/*
 * -----------------------------------------------------------------------------------
 *
 *	l_pars
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	line parser
 *	parsea una linea de src en un char ... a un vector de tokens
 * 
 *
 */

/* line parser */
#if 1

int	l_pars(line_number,qt)
int	line_number;
int	*qt;
{

	int	px,py;

	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	q_lin;
	int	q_tk;
	int	p1,p2,p3,p4;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];

	char	z[MAXV];
	sprintf (z,"l_pars");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}



	/* si encuentro caracteres no considerados para el parser, avisar al final de todo el proceso */
	flag_caracteres = 0;

	q_lin=0;
	memset(b1,32,MAXB);
	memset(b2,0,MAXB);

	strcpy (b1, (*fnp[ line_number ]).l );

	/* si no hay tokens ... devuelvo cant de tokens en 0 */
	*qt=0;

	if (!linea_vacia(b1))
	{

#if 1
		/* blancos al final */
		for (i=strlen(b1)-1, f4=1 ; i && f4 ; i-- )
			if (b1[i] == ' ' || b1[i] == '\n' || b1[i] == '\r' )
				b1[i]=0;
			else
				b1[i+1]='\n', b1[i+2]=0, f4=0;
#endif
	
		f1=1;

		if (gp_fverbose("d3"))
		{
			printf ("Linea  : %d \n\n",line_number);
			printf ("Buffer :|%s|\n",  b1);
		}


		/* comienzo parser de tokens */
		p1=0;
		q_tk=0;

		while ( f1 )
		{
			/* controlamos cantidad de tokens ... */
			if (q_tk > MAXT-10)
			{	error(501);
			} 

			j=tipo_char(b1[p1]);

			switch (j)
			{
				/* otro caracter !!! */
				case TC_RST:
					flag_caracteres=1;
					if (gp_fverbose("d1"))
					{
						printf ("Caracter no definido para parser: |%c| |%d|\n",b1[p1],b1[p1]);
						if (ffaux)
							fprintf (hfaux,"Caracter no definido %d %c\n",b1[p1],b1[p1]);
					}
					p1++; 
					break; 

				/* letras */
				case TC_LET:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_LET || \
					        (j == TC_NUM && !char_demed(b1[p1-1])  ) )
						tk[q_tk][p2++]=b1[p1++];
					tk[q_tk][p2]=0;
					q_tk++;
					break;

				/* numeros tenemos que contemplar 3.3 o 3,3 !! */
				case TC_NUM:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_NUM || \
						(tipo_char(b1[p1]) == TC_PNT && tipo_char(b1[p1+1]) == TC_NUM ) || \
						( (b1[p1]) == ',' && tipo_char(b1[p1+1]) == TC_NUM ) )
					{	tk[q_tk][p2]=b1[p1];
						p1++;
						p2++;
					}
					tk[q_tk][p2]=0;
					q_tk++;
					break;

				/* choto */
				case TC_CHO:
					/* opcion: no los guardo */
					tk[q_tk][0]=' ';
					tk[q_tk][1]=0;
					p1++;
					f1=0;
					break;

#if 0
					while ( (j=tipo_char(b1[p1])) == TC_CHO)
					{	printf ("estoy perdido en choto %d \n",p1);
					 	p1++;	
					}

#endif
					break;


				/* blanco o tab */
				case TC_BLA:
#if 0
					/* opcion: no los guardo */
					while ( (j=tipo_char(b1[p1])) == TC_BLA)
					       p1++;	
#endif
					/* opcion: los guardo */
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;

					break;

				/* coma */
				case TC_CCE:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* punto */
				case TC_PNT:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis abre */
				case TC_PAA:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis cierra */
				case TC_PAC:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				case TC_EOL:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					p1++;
					f1=0;
					break;

				/* caracteres varios */
				case TC_CVR:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				default:
					printf ("Default, algo salio mal  !!!\n\n");
					f1=0;
					break;
			}

		} /* while */


		if (gp_fverbose("d3"))
			printf ("termine de parsear linea \n");


		/* verifico si hay que sacar output  en minusculas */
		if (gp_minusculas)
		{
			for (j=0; j< q_tk; j++)
				strcpy(tk[j],pasar_a_minusc(tk[j]));
		}


#if 0
		for (j=0; j< q_tk; j++)
		{
			strcat (b2,tk[j]);
		}
		strcat (b2,"\n");

		if(gp_fverbose("d3"))
			printf ("strcat: |%s| \n",b2);
#endif

		/* Termine todo lo que tenia que hacer con esta line */


		*qt = q_tk;


	} /* if linea vacia */


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}










#endif
/* line parser */


/*
 * -----------------------------------------------------------------------------------
 *
 *	Rutinas de uso comun
 *
 * -----------------------------------------------------------------------------------
 */








/*
 * -----------------------------------------------------------------------------------
 *
 *	gna
 *
 *	genera numero al azar
 *	numero = rand () % (N-M+1) + M;   // Este está entre M y N
 *
 * -----------------------------------------------------------------------------------
 */


int	gna(min,max)
int	min;
int	max;
{
	int nro;

	nro = rand() % (max-min+1) + min;

	return nro;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	gnf
 *
 *	genera numero al azar
 *	numero = rand () % (N-M+1) + M;   // Este está entre M y N
 *
 * -----------------------------------------------------------------------------------
 */


char	*gnf(mes)
int	mes;
{
	static	char	fecha[16];
	int	a,m,d;

	a = 2018;

	m = mes+1;

	if ( m == 2 )		
		d = gna(1,28);

	if ( m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)
		d = gna(1,31);

	if ( m == 4 || m == 6 || m == 9 || m == 11 )
		d = gna(1,30);


	sprintf (fecha,"%04d%02d%02d",a,m,d);

	return fecha;
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	gnh
 *
 *	genera hora al azar
 *	numero = rand () % (N-M+1) + M;   // Este está entre M y N
 *
 * -----------------------------------------------------------------------------------
 */


char	*gnh(desde,hasta)
int	desde;
int	hasta;
{
	static	char	hora[16];
	int	h,m,s;

	h = gna(desde,hasta);
	m = gna(0,59);
	s = gna(0,59);

	sprintf (hora,"%02d%02d%02d",h,m,s);

	return hora;
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	gsf 
 *
 *	fecha en formato mas 'readabily' para imprimir
 *
 * -----------------------------------------------------------------------------------
 */



char	*gsf(f)
char	*f;
{
	static	char	fecha[16];
	int	a,m,d;

	char	s[16];

	
	memset (s,0,16);
	strncpy(s,f,4);
	a = atoi(s);

	memset (s,0,16);
	strncpy(s,f+4,2);
	m = atoi(s);

	memset (s,0,16);
	strncpy(s,f+6,2);
	d = atoi(s);


	sprintf (fecha,"%04d/%02d/%02d",a,m,d);

	return fecha;
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	gsh
 *
 *	Genera string con hora para imprimir
 *
 *
 * -----------------------------------------------------------------------------------
 */


char	*gsh(sh)
char	*sh;
{
	static	char	hora[16];
	int	h,m,s;

	int	h1,m1;
	int	ext;

	char	b[16];

	
	memset (b,0,16);
	strncpy(b,sh,2);
	h = atoi(b);

	memset (b,0,16);
	strncpy(b,sh+2,2);
	m = atoi(b);

	memset (b,0,16);
	strncpy(b,sh+4,2);
	s = atoi(b);

	sprintf (hora,"%02d:%02d:%02d",h,m,s);

	return hora;
}










/*
 * -----------------------------------------------------------------------------------
 *
 *	csv_parser
 *
 *	
 *	parser string tipo csv
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	
 *	fx		funcion a llamar con cada campo procesado
 *
 */


int	csv_parser(str,fx)
char	*str;
int	(*fx)(int,int,char *);
{
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];

	int	p1,p2;
	int	k;
	int	f1;


	strcpy(b1,str);

	k=0;

		
	f1=1;

	if (f1)
	{
		p1=0;
		p2=0;
		k=0;

		while (b1[p1] )
		{	
			b2[k++] = b1[p1];

			if (b1[p1] == ',' || b1[p1] == '\n')
			{	
				b2[k-1]=0;

				strcpy(b3,b2);
				if (gp_fverbose("d4"))
					printf ("=%02d= %s\n",p2,b3);

				fx(0,p2,b3);

				p2++;
				k=0;
			}

			p1++;
		}
		
		/* termino la linea */
		fx(1,p2," ");
	}
}









/*
 * -----------------------------------------------------------------------------------
 *
 *	(MMM)
 *
 *	Rutinas generales gp_xxxxxx
 *
 *
 * -----------------------------------------------------------------------------------
 */


/* general gp algo */
#if 1




/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_print
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_print()
{
	int i;

	printf ("\n");
	printf ("Cantidad de par   : %2d\n",gp_fq(GP_GET,0) - 1 );
	for (i=0; i<4; i++)
		printf ("Cantidad de par %d : %2d\n",i+1,gp_tpar[i]);
	printf ("\n");

}



/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_parser
 *
 *	parser general de parametros de input al programa
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_parser()
{

	int i,j,fl;
	char	prm[MAXV];

	char	var1[MAXB];   /* provisorio !! */
	FILE	*hwi;



	/* excepcion con verbose, por si lo pusieron al final de la linea, por defecto d5 !!! */

	for (i=0; i < gp_fq(GP_GET,0); i++  )
	{	if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'v' )
#if 0
	/* si hay parametro -vac=algo ...
	 * esto no funca
	 * aca hay que agregar el 'y no tiene igual. ... '
	 */
		if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'v' && !tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
#endif
		{	gp_verbose = 1;
			strcpy(gp_opciones,"d5");
		}

		if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"opciones",6) )
		{	strcpy(gp_opciones,desde_igual( gp_fp(GP_GET,i,(char **)0)));
		}

	}

#if 0

	/* Experimentos .... si puso archivo de configuracion
	 * para todo ???
	 * o solo para cuestiones particulares ??
	 *
	 */

	/* si selecciono archivo de configuracion */
	for (i=0, fl=1; i < gp_fq(GP_GET,0); i++)
	{
		printf ("arg %2d |%s| \n", i, gp_fp(GP_GET,i,(char **) 0 ));
		/* parameter type 3 ... "-someoption=somename" */
		if ( i && fl && *( gp_fp(GP_GET,i,(char **)0) + 0) == '-' && 
		                *( gp_fp(GP_GET,i,(char **)0) + 1) != '-' && tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{
			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"cfg",3) )
			{
				strcpy(var1,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffcfg=1;
			}
		}


		if (ffcfg)
		{
			if ( ffcfg && ((hwi = fopen (var1,"r")) == NULL) )
				error(999);

			fnq1 = &fnp[0];
			qf_load(hwi,fnq1,&qf_lin);
			
			printf ("Cant pars %d\n",qf_lin);
			for (j=0; j< qf_lin; j++)
			{	
				printf ("Par %2d |%s| \n",j,(*fnp[j]).l);

			}

			fclose(hwi);
		}
	}

#endif




	for (i=0; i < gp_fq(GP_GET,0);  )
	{

		fl = 1;

		/* parameter type 1 ... "name" */
		if ( i && fl &&  *( gp_fp(GP_GET,i,(char **)0) + 0) != '-')
		{
			fl = 0;
			gp_q_partype1++;
			gp_tpar[0]++;

			if (gp_q_partype1 == 1)
			{
				strcpy(finp, gp_fp(GP_GET,i,(char **)0));
				ffinp=1;
			}

			if (gp_q_partype1 == 2)
			{
				strcpy(fout, gp_fp(GP_GET,i,(char **)0));
				ffout=1;
			}

			if (gp_fverbose("d5"))
			{
				printf ("Param tipo 1: %s\n", gp_fp(GP_GET,i,(char **)0 ) );
			}
		}





		/* parameter type 2 ... "-something" */
		if ( i && fl && *( gp_fp(GP_GET,i,(char **)0) + 0) == '-' && 
		                *( gp_fp(GP_GET,i,(char **)0) + 1) != '-' && !tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{
			fl = 0;
			gp_q_partype2++;
			gp_tpar[1]++;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'v'  )
				gp_verbose = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'h'  )
				gp_help = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'm'  )
				gp_minusculas = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'f'  )
				gp_fsentencia = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'e'  )
				gp_eol = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'i'  )
				gp_reidx = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'p'  )
				gp_pause = 1;

			if (gp_fverbose("d5"))
			{
				printf ("Param tipo 2: %s\n", gp_fp(GP_GET,i,(char **)0 ) );
			}
		}

		/* parameter type 3 ... "-someoption=somename" */
		if ( i && fl && *( gp_fp(GP_GET,i,(char **)0) + 0) == '-' && 
		                *( gp_fp(GP_GET,i,(char **)0) + 1) != '-' && tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{
			fl = 0;
			gp_q_partype3++;
			gp_tpar[2]++;

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"inp",3) )
			{
				strcpy(finp,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffinp=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"in2",3) )
			{
				strcpy(fin2,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffin2=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"out",3) )
			{
				strcpy(fout,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffout=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou2",3) )
			{
				strcpy(fou2,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou2=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"aux",3) )
			{	
				strcpy(faux,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffaux=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"log",3) )
			{	
				strcpy(flog,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				fflog=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"src",3) )
			{	
				strcpy(fsrc,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffsrc=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"lst",3) )
			{	
				strcpy(flst,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				fflst=1;
			}





			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"nvd",3) )
			{	gp_niveldes = *desde_igual( gp_fp(GP_GET,i,(char **)0)) - '0';
			}


			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"opciones",6) )
			{	
				strcpy(gp_opciones,desde_igual( gp_fp(GP_GET,i,(char **)0)));
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"dato",4) )
			{	
				strcpy(gp_dato,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffdat=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"prue",4) )
			{	
				strcpy(gp_pruebas,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffprb=gp_pruebas[0]-'0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"exec",4) )
			{	
				strcpy(gp_exec,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffexc=gp_exec[0]-'0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"proc",4) )
			{	
				strcpy(gp_proc,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffpro=gp_proc[0]-'0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"tool",4) )
			{	
				strcpy(gp_tool,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				fftoo=gp_tool[0]-'0';
			}



			if (gp_fverbose("d5"))
			{
				printf ("Param tipo 3: %s\n", gp_fp(GP_GET,i,(char **)0 ) );
			}
		}



		/* parameter type 4 ... "--something" */
		if ( i && fl && *( gp_fp(GP_GET,i,(char **)0) + 0) == '-' && 
		                *( gp_fp(GP_GET,i,(char **)0) + 1) == '-' && !tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{
			fl = 0;
			gp_q_partype4++;
			gp_tpar[3]++;

			if (gp_q_partype4 == 1)
			{
				strcpy(var1, gp_fp(GP_GET,i,(char **)0) + 2);
			}

			if (gp_fverbose("d5"))
			{
				printf ("Param tipo 4: %s\n", gp_fp(GP_GET,i,(char **)0 ) );
			}


		}






		/* program name */
		if (i==0)
		{	
			if (gp_fverbose("d5"))
			{
				printf ("Programa    : %s\n", gp_fp(GP_GET,i,(char **)0 ) );
			}
		}

		/* next ... */
		i++;
	}

	/* futuro ...
	 * Hacer una gramatica para la combinacion de comandos
	 * posibles !!
	 *
	 * Por ahora, revisamos combinaciones basicas que
	 * son necesarias entre parametros ...
	 *
	 */

	/* si no pone parametros ... mandar a uso */
	if ( (gp_q_partype1 + gp_q_partype2 + gp_q_partype3 + gp_q_partype4 == 0 ) || fl )
		gp_help = 1;

#if 0
	/* si no indico ningun  archivo ... mandar a uso */
	if ( gp_q_partype1 + gp_q_partype3 == 0 )
		gp_help = 1;
#endif 

	if (gp_fverbose("d5"))
	{
		printf ("\n\n");
	}
} 




/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_igual
 *
 * -----------------------------------------------------------------------------------
 */


int	tiene_igual(s)
char	*s;
{
	int i,flag;

	for (i=0, flag=0; i< strlen(s) && !flag; i++ )
		if ( *(s+i) == '=')
			flag=1;

	return flag;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	desde_igual
 *
 * -----------------------------------------------------------------------------------
 */


char	*desde_igual(s)
char	*s;
{
	int i,flag;

	for (i=0, flag=0; i< strlen(s) && !flag; i++ )
		if ( *(s+i) == '=')
			flag=1;

	return s+i;
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_init
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_init(vpar_q,vpar_p)
int	vpar_q;
char	**vpar_p;
{
	int i;

	gp_fq(GP_SET,vpar_q);
	gp_fp(GP_SET,0,vpar_p);
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_fp
 *
 * -----------------------------------------------------------------------------------
 */

char	*gp_fp(action,offset,value)
int	action;
int	offset;
char	**value;
{
	static char **gp_value;

	if (action == GP_SET)
	{	
		gp_value=value;
	}

	return 	*(gp_value+offset);
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_fq(action,value)
 *	
 *	int action
 *	int value
 *	
 *	value:
 *	on fist call, value is a number (typically argc )
 *	after first call, value can be any number, and is not used
 *
 *	returns:
 *	value
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_fq(action,value)
int	action;
int	value;
{
	static int gp_value=0;

	if (action == GP_SET)
	{	
		gp_value=value;
	}

	return gp_value;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_test 
 *
 *	
 *
 * -----------------------------------------------------------------------------------
 */


int	gp_test()
{
	int i;

	printf ("Cantidad de par   : %2d\n",gp_fq(GP_GET,0) - 1 );
	for (i=0; i<4; i++)
		printf ("Cantidad de par %d : %2d\n",i+1,gp_tpar[i]);

	printf ("\n");
	for (i=0; i< gp_fq(GP_GET,0); i++)
		printf ("Parametro  %2d     : %s\n",i,gp_fp(GP_GET,i,(char **)0)  );
			
	printf ("\n");
	printf ("\n");
}

/*
 * -----------------------------------------------------------------------------------
 *
 * 	linea_vacia
 *
 *	Determina si una linea esta vacia (generalmente, para lineas leidas de files)
 *	La linea NO esta vacia si contiene al menos 1 caracter distinto de
 *	' ' 	blanco
 *	'\t' 	tab
 *	'\n'	new line
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	linea_vacia(s)
char	*s;
{
	int i,flag;

	i=0;
	flag=1;

	for (i=0; flag && i< strlen(s); i++)
		if (s[i] != ' ' && s[i] != '\t' && s[i] != '\n' )
			flag=0;

	return flag;
}



/*
 * -----------------------------------------------------------------------------------
 *
 * 	error
 *
 *	display de errores
 *	en esta version es display de un numero
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	error(value)
int	value;
{
	printf ("Error: %d\n",value);
	if (!value)
		printf ("usar -h para help\n");
	exit(0);
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	tipo_char
 *
 *	ATENCION:
 *	Falta contemplar que hacemos con el caracter '_' !!!!
 *	Por ahora lo incluyo en letras, porque si no, me separa el tag de las palabras
 *	Si voy a parsear corpus sin tag ... lo tengo que sacar de la lista !!!
 *
 *	0	fin de linea
 *	1	blanco o tab
 *	2	otra cosa ( x ahora ... )
 *
 * -----------------------------------------------------------------------------------
 */



int	tipo_char(c)
char	c;
{
	int x;

	x=TC_RST;

	/* para archivos fortran de hita */
	if (c == '\n' || c == 13 )
		x = TC_EOL;

	if (c == ' ' || c == '\t' )
		x = TC_BLA;

	if (c == '_' )
		x = TC_GBJ;

	if (c == ',' || c == ';' || c == ':' || c == '-' || c == '/' || c == '\\' )
		x = TC_CCE;

	if (c == '.' )
		x = TC_PNT;

	if (c == '(' || c == '[' || c == '{' )
		x = TC_PAA;

	if (c == ')' || c == ']' || c == '}' )
		x = TC_PAC;

	if (c >= 'a' && c <= 'z' || c>= 'A' && c <= 'Z' )
		x = TC_LET;

	/* fortran */
	if (c >= 'a' && c <= 'z' || c>= 'A' && c <= 'Z' || c == '_')
		x = TC_LET;

	if (c >= '0' && c <= '9' )
		x = TC_NUM;

	/* agrego los que vienen de analizar fuentes fortran */
	if (c == '\'' || c == '%' || c == '!' || c == '*' )
		x = TC_CVR;

	if (c == '"' || c == '&' || c == '+' || c == '=' || c == '>' || c == '<' || c == '?' )
		x = TC_CVR;

	/* agrego los que vienen de analizar fuentes fortran */
	if (c == '$' || c == '#' || c == '^' || c == '@' )
		x = TC_CVR;







	if (c == 0)
		x = TC_CHO;

	return x;
}





/*
 * -----------------------------------------------------------------------------------
 *
 * 	es_numero
 *
 *	funcion para determinar si caracter es numero
 *
 *
 * -----------------------------------------------------------------------------------
 */




int	es_numero(c)
char	c;
{
	int x;

	x=0;
	if ( c >= '0' && c <= '9' )
		x=1;

	return x;
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	es_num_tk
 *
 *	funcion para determinar si string es numero
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	es_num_tk(s)
char	*s;
{
	int i,x;

	x=1;
	for (i=0; x && i<strlen(s); i++)
	{	if (s[i] < '0' || s[i] > '9' )
			x=0;

	}

	return x;
}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	es_puntuacion
 *
 *	funcion para determinar si es simbolo de puntuacion
 *
 *
 * -----------------------------------------------------------------------------------
 */




int	es_puntuacion(s)
char	*s;
{
	int i,x;

	x=1;

	for (i=0; x && i<strlen(s); i++)
	{	if (s[i] != '.' && s[i] != ',' && s[i] != ':' && s[i] != ';' )
			x=0;
	}

	return x;

}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	es_word
 *
 *	funcion para determinar si es word o no
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	es_word(s)
char	*s;
{
	int i,x;

	x=1;

	for (i=0; x && i<strlen(s); i++)
	{	if (s[i] < 'A' || s[i] > 'z' || ( s[i] > 'Z' && s[i] < 'a')  )
			x=0;
	}

	return x;

}

/*
 * -----------------------------------------------------------------------------------
 *
 * 	char_demed
 *
 *	caracteres utilizados normalmente en frases de medidas 
 *	1 cm x 1 cm ( 'x' ) y similares
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	char_demed(c)
char	c;
{
	int	x;

	x=0;
	if ( c == 'e' || c == 'E' || c == 'x' || c == 'X' )
		x=1;

	return x;
}



/*
 * -----------------------------------------------------------------------------------
 *
 * 	uso
 *
 *	texto para describir el uso de la herramienta
 *
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_uso(x)
int	x;
{
	char	w[MAXV];
	char	z[MAXV];
	
	sprintf (z, gp_fp(GP_GET,0,(char **)0)  );
	memset (w,0,MAXV);
	strncpy (w,"                                        ",strlen(z));

	printf ("Usage: \n\n");
	printf ("                                                                                                  \n");
	printf ("%s -h                                  help                                                       \n",z);
	printf ("%s -v                                  verbose ... muestra cierta informacion de proceso          \n",z);
	printf ("%s -v -opciones=AxByCz...              info: A,B,C = (D)ebug, (I)nformative, (E) extra x=(0-5)    \n",z);
	printf ("%s                                     no imprime, 1 basico, 2 y 3 debug, 4 full debug        \n",w);
	printf ("                                                                                                  \n");
	printf ("Tools:                                                                                            \n");
	printf ("                                                                                                  \n");
	printf ("tool2:         extrae fuentes utilizados por archivo makefile                                     \n");
	printf ("%s -v -opciones=d5 -tool=2 -inp=makefile -out=l1                                                  \n",z);
	printf ("%s -v -opciones=d5 -tool=2 -inp=makefile -out=l1 -m             (in lower case)                   \n",z);
	printf ("                                                                                                  \n");
	printf ("tool3:         carga file con listado de archivos y genera mismo pasado a minusculas              \n");
 	printf ("%s -v -opciones=d5 -tool=3 -inp=file  -out=file_to_min                                            \n",z);
	printf ("                                                                                                  \n");
	printf ("tool5:         carga un fuente - procesos varios - genera nuewvo fuente                           \n");
	printf ("%s -v -opciones=d5 -tool=5 -inp=f_org -out=f_new -aux=parser.err -f                               \n",z);
	printf ("%s -v -opciones=d5 -tool=5 -inp=f_org -out=f_new -aux=parser.err     (version graba tokens )      \n",z);
	printf ("                                                                                                  \n");
	printf ("tool6:         carga un fuente - arregla lineas de con fortran - genera nuevo fuente              \n");
	printf ("%s -v -opciones=d5 -tool=6 -inp=f_org -out=f_new -aux=parser.err                                  \n",z);
	printf ("                                                                                                  \n");


	if (gp_fverbose("d1"))
		printf ("gp_uso(%d)\n",x);

	printf ("\n\n\n");

	exit(x);
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	gp_default
 *
 * 	inicializa parametros de funcionamiento default
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_default()
{
	gp_help=0;
	gp_verbose=0;

	ffcfg=0;
	ffinp=0;
	ffin2=0;
	ffout=0;
	ffou2=0;
	ffaux=0;
	fflog=0;

	ffsrc=0;
	fflst=0;

	ffdat=0;

	sprintf (gp_opciones, "%s","______");
	sprintf (gp_dato    , "%s","______");
	sprintf (gp_pruebas , "%s","______");
	sprintf (gp_exec    , "%s","______");
	sprintf (gp_proc    , "%s","______");
	sprintf (gp_tool    , "%s","______");

	memset(gp_tpar,0,sizeof(gp_tpar));

}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	pasar_a_minusc
 *
 * -----------------------------------------------------------------------------------
 */

char	*pasar_a_minusc(s)
char	*s;
{
	static char b[MAXB];

	int i,j,k;

	strcpy(b,s);

	for (i=0; i<MAXB && b[i]; i++)
	{
		if ( b[i] >= 'A' && b[i] <= 'Z' )
			b[i] += 32;
	}

	return b;
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	gp_fverbose
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	gp_fverbose
 *
 *
 *	Devuelve 1 (si) o 0 (no) si aplica la condicion de verbose
 *	
 *	Las opciones involucradas son:
 *	-v               activa el verbose
 *	-opciones=str    string de condiciones de verbose  (requiere -v )
 *	                 
 *	El string de opciones es del tipo "Ln  (L)=I,D,E  (n)=0,1,2,3,4,5   "
 *	Donde:
 *	L       Es la letra (I informativo, D debug, y E ... no me acuerdo
 *	n       Es el nivel
 *	        0 no imprime nada   ( es lo mismo que no poner nada !!! )
 *	        1    imprime cosas basicos, por que rutinas paso etc
 *	        2,3  niveles de debug intermedios
 *	        4    full debug
 *	        5    incluye debug de gp_parse
 *
 * 
 *
 *	
 */


int	gp_fverbose(situacion)
char	*situacion;
{
	int	i,j,k;
	int	sit;

	static	int	f_p=1;
	static	int	f_i=0;
	static	int	f_e=0;
	static	int	f_d=0;

	if ( f_p == 1 && gp_opciones[0] != '_' )
	{	f_p = 0;

		for (i=0; i<strlen(gp_opciones)-1; i=i+2)
		{
			if (*(gp_opciones+i) == 'i' || *(gp_opciones+i) == 'I' )
				f_i = (int) *(gp_opciones+i+1) - '0';
			if (*(gp_opciones+i) == 'e' || *(gp_opciones+i) == 'E' )
				f_e = (int) *(gp_opciones+i+1) - '0';
			if (*(gp_opciones+i) == 'd' || *(gp_opciones+i) == 'D' )
				f_d = (int) *(gp_opciones+i+1) - '0';
		}
	}


	sit = 0;

	if (gp_verbose)
	{

		/* determino si hay que devolver 0 o 1
		 * en base a situacion informada, y 
		 * conjunto de flags y '-opcion=vN'   
		 */
	
		for (sit=0, i=0; i<strlen(situacion); i=i+2)
		{
#if 0
			printf ("X1 f_P: %d i:%d situaci: %s  opcion: %s f_d: %d sit: %d\n",
				f_p,i,situacion,gp_opciones,f_d,sit);
#endif
	
			if (*(situacion+i) == 'i')
				if ( *(situacion+i+1)-'0' <= f_i )
					sit = 1;
	
			if (*(situacion+i) == 'e')
				if ( *(situacion+i+1)-'0' <= f_e )
					sit = 1;
	
			if (*(situacion+i) == 'd')
				if ( *(situacion+i+1)-'0' <= f_d )
					sit = 1;
#if 0
			printf ("X2 f_P: %d i:%d situaci: %s  opcion: %s f_d: %d sit: %d\n",
				f_p,i,situacion,gp_opciones,f_d,sit);
#endif
		}
	}

	return	sit;
}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	gp_tm
 *
 * -----------------------------------------------------------------------------------
 */




char	*gp_tm()
{
	time_t t;
  	struct tm *tm;

	t=time(NULL);
	tm=localtime(&t);
	strftime(gp_fyh, MAXR , "[%Y/%m/%d %H:%M:%S]", tm);

	return(gp_fyh);
}



/*
 * -----------------------------------------------------------------------------------
 *
 * 	df
 *
 * -----------------------------------------------------------------------------------
 */


char	*df(s)
char	*s;
{
	static	char	nf[MAXF];

	int	i,j,k,f1;

	for ( f1=1, i=0; f1 && i<strlen(s); i++)
		if (s[i] != '.')
			nf[i]=s[i];
		else
			nf[i]=0,f1=0;

	strcat(nf,".dif");


	return nf;
}


#endif
/* general gp algo */




/*
 * -----------------------------------------------------------------------------------
 *	end of source
 *	end of source
 *	end of source
 * -----------------------------------------------------------------------------------
 */








/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prue 3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	prue3
 *
 *	Descripcion:
 *
 */


/* bloque */
#if 0


int	pro_prue3()
{
	int	i,j,k;
	int	f1,f2,f3;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;



	char	z[MAXV];
	sprintf (z,"prue3");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!1 || !2 )
		gp_uso(11);

	/* bloque */
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */



