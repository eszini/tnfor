/*
 *	search:
 *      =============================================================
 *	"header"		header del programa
 *	"ejemplos"		ejemplos de uso (lineas de comando )
 *	"documentacion" 	alguna documentacion
 *	"to do"			pendings ...
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
 *	Sun Jun 16 19:20:43 -03 2024
 *	agregue que pueda cambiar inicializaciones tipo var/xxx/ por var = xxx
 *
 *	Mon Jun 10 01:30:37 -03 2024
 *	ajustes a armame_dos_lineas para separar lineas largas
 *
 *	Thu Jun  6 03:13:28 -03 2024
 *	mas mejoras en chgtyp
 *	agregue estadisticas para saber que cambios hizo
 *
 *	Mon Jun  3 19:23:14 -03 2024
 *	mejore que contemple integer *2, intent(int)
 *	
 *	
 *	Fri May 31 04:32:58 -03 2024
 *	agregue proc4 - lee todo el conjunto de fuentes, pero sobre cadenas de structs
 *	lo que necesito, es poder extender el largo de un archivo sin correr
 *	mas de 500,000 lines de codigo en un vector 
 *
 *	Mon May 27 22:07:23 -03 2024
 *	agregue sort a la lista de archivos de salida
 *
 *	Sun May 26 23:41:27 -03 2024
 *	mejore prue2 ... procesa todos los files en list_src, genera nueva
 *	version en new_repo, y tiene infraestructura armada para ...
 *	- sacar long names (l_names )
 *	- otras cosas que se quieran agregar a p_src2
 *
 *	Mon May 20 00:41:05 -03 2024
 *	agregue --chgmas .. saca los + de las declaraciones de variables
 *	con esto, se puede llevar un f77 a que compile como f95
 *	sin salir del fixed form
 *
 *
 *	Sat May 18 05:11:54 -03 2024
 *	en algunas lineas de fortran77, si primer car de lin es '*' no chilla !!
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
 *	para ponernos de acuerdo:
 *	0 - no muestra nada
 *	1 - solo informacion resultado que deba mostrarse
 *	2 - un poco de debug, 
 *          x que rutinas paso
 *          lineas cargadas 
 *          alguna info de control util
 *      3 - mas nivel de debug
 *
 *
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
 *	exec1
 *
 *	Toma archivo de input y genera tabla con cantidad de caraceres
 *	en todos el file
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	prue1:
 *
 *	Conjunto de 14 ejercicios sobre manejo de estructuras
 *
 *	./prog  -v -f -opciones=d5 -prue=1
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	prue2:
 *
 *	./tfor -v -opciones=d5 -prue=2 -inp=list_src -out=l_names -aux=p.err -dato=new_repo 
 *
 * 	Carga todos los archivos indicados en list_src, los carga en memoria
 * 	(como vector a punteros de estructuras)
 *	permite procesos varios, y vuelve a generar con cabios en el directorio new_repo
 *
 *	en p.err pone caracteres que no reconoce el parser de lineas
 *	en l_names (testing) pone nombres largos 
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
 *	Arregla las lineas de comentarios
 *	el -t cambia tabs por 4 blancos
 *
 *	./tfor -v -opciones=d5 -tool=6 -t -inp=t1.for -out=t2.for --chgcomm
 *
 *	Arregla las lineas de continuacion en fortran
 *
 *	./tfor -v -opciones=d5 -tool=6 -inp=t1.for -out=t2.for --chglco > log
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	proc4:
 *
 *	Carga todos los sources en memo
 *	Utiliza cadenas de structs
 *	De esta manera, si tengo que extender un file por cambios
 *	no tengo que estar moviendo las mas de 500,000 lineas de codigo
 *
 *	./tfor -v -opciones=d5 -proc=4 -inp=l1 -out=l_names -aux=run.log -dato=repo2  > log
 *
 *
 */


/*
 *	//documentacion //
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
 *	//to do //
 */


/*
 *	cfor_lcon da error con esta linea, pone el ampersand antes del !
 *	error....   CALL MG_LOCATE_WRITE(15,35,'error reading definition file!',
 *
 *	--chgcom
 *	stadisticas 
 *
 *	--chgmas
 *	ej. ANDECOMP.FOR
 *
 *	poner contador cuando se aplanan las declaracions de vars
 *
 *	no funciona p4_mas cuando hay lineas con comentarios
 *	en medio de las lineas con mas 
 *
 *	Un archivo tipo .err ... fferr para guardar todos los 
 *	errores. Justo a cambios en error() .. que sea con 
 *	numero y un mensaje
 *
 *
 *	---- otros ----
 *
 *
 *	poner en orden los codigos de error !!!
 *
 *	errores
 *	manejar mejor, permitir imprimir algun string, 
 *      rutina en la que estaba etc
 *
 *	bulk process
 *	compatilizar las corridas individuales via 
 *	tool6, y las de tod el conjunto via prue2 
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
#include <stdbool.h>
#include <ctype.h>
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
#define	HUG1	2048	/* huge buffer */
#define	HUG2	4096	/* huge buffer */
#define MSTR	24000	/* monster buffer */

#define TRUE 1
#define FALSE 0

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
int	gp_tabs=0;		/* reemplazar tabs x 4 blancos si no */
int	gp_rellena=0;		/* & en cont de linea 0 ĺo pegam 1 rellena a columna 90 */
int	gp_fsentencia=0;	/* archivo de salida en formato sentencias 0 no 1 si */
int	gp_eol=0;		/* fuerzo string EOL al final de la linea ... a veces hay . en medio de la linea */
int	gp_reidx;		/* re indexar archivo de transacciones */
int	gp_pause;		/* pausa al mostrar ... */
int	gp_niveldes=0;		/* nivel de descripcion que se vuelca en archivo de salida (solo con fsentencia = 0 ) */
				/* 0 normal 1 sentencia y numero de token 2 .... agrego cosas del diccionario si se usa */
int	gp_cborrar=0;		/* comentarios a borrar 0 nada, 1 al final de lineas 2 lineas comentadas 3 todos     */
int	gp_suboption=0;		/* sub opciones para pro_xxxx algo  valores 1 2 3 ... etc  */
int	gp_debug=0;		/* flag extra para debug especifico */

int	gp_help=0;		/* help 0 no 1 si */
int	gp_vers=0;		/* version 0 no 1 si */
int	gp_version(int);		/* version del fuente */
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

char	gp_reportes[MAXV];	/* reportes a mostrar */


/*
 *
 *	Funciones, variables y estructuras globales en el programa
 *
 */

int	abro_files();
int	cierro_files();
int	mostrar_cargas();
int	mostrar_reportes(char *);



int	gna(int,int);
char	*gnf(int);			/* genera fecha al azar */
char	*gnh(int,int);			/* genera hora al azar */
char	*gsf(char *);			/* string para imprimir fecha */
char	*gsh(char *);			/* string para imprimir hora */



char	*desde_igual(char *s);
char	*pasar_a_minusc(char *s);
char	*minusculas(char *s);
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

int	pro_prue1();	/* ejercicios de estructuras, vectores, puntores, malloc */
int	pro_prue2();	/* carga todos los fuentes en memoria, en vector de ptr - version vieja */
int	pro_prue3();	/* ejercicio de acceso a una matriz, referenciada por un pointer */
int	pro_prue4();
int	pro_prue5();
int	pro_prue6();
int	pro_prue7();
int	pro_prue8();
int	pro_prue9();

int	pro_exec1();	/* cuenta caracteres en un archivo */
int	pro_exec2();	/* carga todos los fuentes en memoria, en vector de ptr - version vieja */
int	pro_exec3();	/* carga todos los fuentes en memoria, en vector de ptr - para procesos - version vieja */
int	pro_exec4();	/* carga todos los src en mem en vec de ptr para procesos - version nueva - entrys */
int	pro_exec5();	/* carga file con definicion de variables de fort con blancos inermedios y genera lista */
int	pro_exec6();	/* carga todos los src en mem en vec de ptr para procesos - version nueva - var con blanks */
int	pro_exec7();	/* pone bonita la salida de check1 y check2 ... */
int	pro_exec8();	/* carga todo el codebase en memo - busca variables fortran */
int	pro_exec9();	/* carga todo el codebase en memo - busca allocate */

int	pro_proc1();
int	pro_proc2();
int	pro_proc3();
int	pro_proc4();

int	pro_tool1();
int	pro_tool2();
int	pro_tool3();
int	pro_tool4();
int	pro_tool5();
int	pro_tool6();
int	pro_tool7();
int	pro_tool8();
int	pro_tool9();




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
char	fin3[MAXF];	/* archivo inp para entrada segun necesidad */
char	fout[MAXF];
char	fou2[MAXF];
char	fou3[MAXF];
char	fou4[MAXF];
char	fou5[MAXF];
char	fou6[MAXF];
char	faux[MAXF];	/* archivo aux para salidas segun necesidad */
char	flog[MAXF];	/* archivo log para salidas segun necesidad */
char	fsta[MAXF];	/* archivo stats segun necesidad */
char	fckf[MAXF];	/* archivo de mensajes de check segun necesidad */

char	fsrc[MAXF];	/* archivo de src de entrada */
char	flst[MAXF];	/* archivo de listado de src de entrada */

int	ffinp;
int	ffin2;
int	ffin3;
int	ffout;
int	ffou2;
int	ffou3;
int	ffou4;
int	ffou5;
int	ffou6;
int	ffaux;		/* archivo aux para output segun necesidad */
int	fflog;		/* archivo log para output segun necesidad */
int	ffcfg;		/* archivo de configuracion */
int	ffsta;		/* estadisticas */
int	ffckf;		/* check file messages */

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
FILE	*hfin3;
FILE	*hfout;
FILE	*hfou2;
FILE	*hfou3;
FILE	*hfou4;
FILE	*hfou5;
FILE	*hfou6;
FILE	*hfaux;
FILE	*hflog;
FILE	*hfsta;
FILE	*hfckf;

FILE	*hfsrc;
FILE	*hflst;

FILE	**fptr;


int	flag_caracteres;
int	flag_before_exit;

int	agregar_ffaux(char *);
int	agregar_ffsta(char *);
int	agregar_ffckf(char *);
int	agregar_fflog(char *);


/*
 * 	algunos settings especificos de tfor - conv de for files 
 *
 */

int	ffchg_dec  = 0;		/* delete comentarios            */
int	ffchg_com  = 0;		/* cambia caracter de comentario */
int	ffchg_typ  = 0;		/* cambia type selectors */
int	ffchg_mas  = 0;		/* saca la continuacion de linea con mas */
int	ffchg_ini  = 0;		/* cambia el tipo de init de vars        */
int	ffchg_lco  = 0;		/* cambia lineas de continuacion */
int	ffchg_vcb  = 0;		/* cambia variables con blancos intermedios */
int	ffchg_alo  = 0;		/* cambia allocate , agrega check  */



/*
 *	Estructuras y variables para la lista de variables fortran con blancos
 *
 *
 */

#define	MAX_VCB		600	/* maxima cantidad de variables con blancos en codebase */
#define	MAX_VSB		600	/* maxima cantidad de variables con blancos en codebase */

int	q_vcb;
char	vcb[MAX_VCB][MAXV];
char	vcb_o[MAX_VCB][MAXV];
char	vcb_p[MAX_VCB][MAXB];
char	vcb_c[MAX_VCB][MAXB];
char	vcb_d[MAX_VCB][MAXB];

int	q_vsb;
char	vsb[MAX_VSB][MAXV];



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
 *	(version cadena de structs )
 *
 *	ldf	linea de file
 */

#define	MAX_QSRC	500   		/* cantidad de files ... 500  */

typedef	struct	ldf	*ldf_ptr;
typedef	struct	ldf
{	char	l[HUG1];		/* por ahora, despues malloc */
	int	f1;			/* usos varios */
	int	f2;
	int	f3;
	struct	ldf	*nx;
}	ldf_node;

ldf_ptr	ldfp1,ldfp2,*ldfq1,*ldfq2;

ldf_ptr	vldf[MAX_QSRC];			/* vector a punteros primer struct de c fuente */




/*
 * 	Variables y estructs para cargar todo un archivo en memoria
 *	const y estructs especificas para todo el tema de estabilizacion de fortran
 *	(version vector de punteros a structs )
 */


#define	MAX_FSRC	400000		/* 10000 lineas de codigo ... seran suf ? */
int	qf_lin;				/* lineas en file */
int	qf_fen;				/* strings con nombre de archivos validos encontrados */
int	qf_src;				/* cantidad total de lineas de src en vector */

typedef	struct	tfn	*fnptr;
typedef	struct	tfn
{	char	l[HUG2];		/* por ahora, despues malloc */
	int	f1;			/* usos varios */
	int	f2;
	int	f3;
}	node;

fnptr	fnp1,fnp2,*fnq1,*fnq2,*fnpa;

fnptr	fnp[MAX_FSRC];			/* vector de punteros a lineas de source */
fnptr	fnf[MAX_FSRC];			/* vector de punteros a lineas de archivos encontrados */

int	pf_load();			/* proceso de carga del file */
int	qfv_load(FILE *,fnptr *,int *);	/* proceso de carga del file - como vector de ptrs a struct */
int	qf_addline(fnptr *,int );	/* agrega una linea */
int	qf_move(fnptr *, int);		/* hace un  'lugar' en el vector */
int	pf_write();			/* proceso de write del file */

int	qfc_load(FILE *,int,int *);	/* proceso de carga del file - como cadena de ptrs a struct */
int	qfc_write(FILE *,int);


/*
 * 	MAX_QSRC		500		
 */

int	qf_ff;				/* cantidad de archivos cargados en memo */

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf,ql;		/* primera - ultima fila, q lineas */
	int	f1,f2,f3;		/* flags prop general */
	int	q_entrys;		/* cantidad de entrys */
	int	q_prog_calling;		/* cant de programas (dif) que llaman a entrys */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];


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
int	cfor_comm(int *,int *);
int	cfor_vars(int *,int *);
int	cfor_lcon(int *,int *);
int	cfor_mas(int *,int *);
int	cfor_ini(int *,int *);
int	cfor_vcb(int *,int *);
int	cfor_dec(int *,int *);
int	cfor_alo(int *,int *);
int	check_file(int *);
int	l_pars(int, int *);
int	tiene_dec_var1();
int	tiene_mas(char *);
int	tiene_amper(char *);
int	def_var_continua(char *s);
int	fix_dec_var1();
int	fix_dec_var2();
int	p_src1();
int	p_src2();
int	p_src3();
int	ex3_p1();
int	ex3_p2();
int	ex3_p3();
int	ex3_p4();
int	ex4_p1();
int	ex6_p1();
int	ex6_p2();
int	ex6_p3();
int	ex6_p4();
int	ex8_p1();
int	ex8_p2();
int	ex8_p3();
int	ex9_p1();
int	es_cadena_valida(int,char *);
int	es_cadena_interesante(char *);
int	es_cadena_int_src3(char *,int *);
int	tiene_coment_intermedio (char *);
int	es_linea_comentario(char *);
int	es_linea_comentario2(char *);
int	armame_dos_lineas(char *, char *, char *);
int	preparame_dos_lineas(char *, char *, char *);
int	correme_una_linea(int, int);
int	pasa_filtro(char *);
int	ordenar_makefile();
int	ps_src1();
int	largo_linea(char *);
int	tiene_include_valido(char *,char *,char *);
int	tiene_include_v2(char *, char *);
int	es_nombre_de_include(char *s);
int	lne(int );
char	*trim_blanks(char *);
char	*limpiar_mas(char *);
char	*analisis_comentario(char *);
bool	tiene_include_v3(char *, char *);
bool	tiene_include_v4(char *, char *);
int	tiene_entry_v1(char *, char *);
int	tiene_call_a_entry(char *, char *);
int	tiene_function_a_entry(char *, char *);
int	tiene_coment_final(char *, int*);
int 	tiene_entry(const char *); 
int	complejidad_mon(int, int, int *, int *);
int	es_mon(char *);
int	len_linea_src(char *,int *,int *,int *);
int	lml_src_sc(int *,int *,int , int );
int	primer_char(char *, int *);
char	*f_name(int);
char	*e_name(int);
int	tiene_integer(char *);
int	tiene_character(char *);
int	rutina_v(char *, int *, char **);
int	rutina_w(char *, int *, int *,char **);
int	compare_vcb(const void *, const void *);
int	primer_caracter_es_mas(char *);
int	tiene_allocate(char *);
char	*trim_beg(char *);
char	*trim_end(char *);

#if 0
int	compare_vcb(char *, char *);
#endif


int	es_integer2(char *s);		/* ojo. especificas de pro_exec5 */
int	es_character1(char *s);


#if 0
int	info_cmp1(char *, char *);
int	info_cmp2(char *, char *);
#endif

int 	info_cmp1(const void *, const void *);
int 	info_cmp2(const void *, const void *);

#if 0
void v_pars(char *, int *, int *, char **, int *, DictionaryEntry *, int );
#endif

void v_pars(char *, int *, int *, char **);

#define	MAX_VARS	100

int	n_var;
int	v_size[MAX_VARS];
char	*v_name[MAX_VARS];

int	borrar_stats();


/*
 *	variables para estadisticas en los procesos 
 *	estandarizacion del bendito fortran
 *	stats
 */


int	sq_lineas_com;			/* q de lineas de comentarios cambiadas */
int	sq_lineas_desdobladas;		/* q de lineas que hubo que dividir */
int	sq_integer;			/* q de integer cambiados */
int	sq_real;			/* q de real cambiados */
int	sq_logical;			/* q de logical cambiados */
int	sq_character;			/* q de character cambiados */
int	sq_variables_no_convertidas;	/* importante ! variables que no se pud convertir */
int	sq_lcont;			/* lineas de continuacion modificadas */
int	sq_vinit_simple;		/* var inits cambiados - simple ...en una linea */
int	sq_lineas_con_mas_elim;		/* lineas con cont de linea mas eliminadas */
int	sq_comentarios_elim;   		/* lineas de codigo comentadas  eliminadas */
int	sq_coment_al_final_elim;	/* comentarios al final de lineas de codigo validas eliminadas */
int	sq_q_alloc;			/* cantidad total de allocs */
int	sq_q_alloc_for_slc;		/* cantidad total de allocs en f77 sin lineas de continuacion */
int	sq_q_alloc_for_clc;		/* cantidad total de allocs en f77 con lineas de continuacion */
int	sq_q_alloc_f90_slc;		/* cantidad total de allocs en f90 sin lineas de continuacion */
int	sq_q_alloc_f90_clc;		/* cantidad total de allocs en f90 con lineas de continuacion */
int	sq_q_alloc_for_sav;		/* cantidad total de lineas allocate con single var */
int	sq_q_alloc_for_mav;		/* cantidad total de lineas allocate con multiple var */
int	sq_q_alloc_for_slc_sav;		/* cant tot de lineas sin cont con allocate single var */
int	sq_q_alloc_for_clc_sav;		/* cant tot de lineas con cont con allocate single var */
int	sq_q_alloc_for_slc_mav;		/* cant tot de lineas sin cont con allocate multi var */
int	sq_q_alloc_for_clc_mav;		/* cant tot de lineas con cont con allocate multi var */
int	sq_q_alloc_for_sta;		/* cantidad total de lineas allocate con stat ya incorporado */


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





#define	LF77	72			/* maximo de caracteres en una lunia de F77 */


char	*argum[3] = { "tfor","-v","-opciones=d5" };

char	**mi_argv = &argum[0];
int	mi_argc = 3;


char	*mis_args[100];
int	mis_argc;
char	**mis_argv = &mis_args[0];





/*
 * me alcanza para hacer un diccionario con los comentarios ?
 *
 */


char	cpt[18000][MAXB];

	

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
	gp_version(gp_vers);

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
		mostrar_reportes("default");
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

/*
 * stats
 *
 * imprime / guarda estadisticas 
 *
 */


int	mostrar_reportes(s)
char	*s;
{
	int	f1;
	int	n;

	char	z[MAXV];
	sprintf (z,"mostrar_reportes");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	n = 0;
	if (!strcmp(s,"default"))
		n = 1;
	if (!strcmp(s,"tool6"))
		n = 10;
	if (!strcmp(s,"exec9"))
		n = 11;



	if (n == 1)
	{
		printf ("hola Erno ...               \n");
		printf ("que gusto verte por aca ... \n");
	}


	if (n == 10 )
	{

		printf ("\n");
		printf ("File: %s\n",finp);
		printf ("Cantidad de lineas con coment camb  %6d \n", sq_lineas_com);
		printf ("Cantidad de lineas con com elim     %6d \n", sq_comentarios_elim);
		printf ("Cantidad de lineas divididas        %6d \n", sq_lineas_desdobladas);
		printf ("Cantidad de integer cambiados       %6d \n", sq_integer);
		printf ("Cantidad de real cambiados          %6d \n", sq_real);
		printf ("Cantidad de logical cambiados       %6d \n", sq_logical);
		printf ("Cantidad de character cambiados     %6d \n", sq_character);
		printf ("Cantidad de lin cont cambiadas      %6d \n", sq_lcont);
		printf ("Variables_no_convertidas            %6d \n", sq_variables_no_convertidas);
		printf ("Var inits simples convertidos       %6d \n", sq_vinit_simple);
		printf ("Lineas con cont mas convertidas     %6d \n", sq_lineas_con_mas_elim);
		printf ("Comentarios al final de lineas elim %6d \n", sq_coment_al_final_elim);	
		printf ("Cant de alloc totales               %6d \n", sq_q_alloc);
		printf ("Cant de alloc totales for slc       %6d \n", sq_q_alloc_for_slc);
		printf ("Cant de alloc totales for clc       %6d \n", sq_q_alloc_for_clc);
		printf ("Cant de alloc totales f90 slc       %6d \n", sq_q_alloc_f90_slc);
		printf ("Cant de alloc totales f90 clc       %6d \n", sq_q_alloc_f90_clc);
		printf ("Cant de alloc totales for sav       %6d \n", sq_q_alloc_for_sav);		/* single var allocated */
		printf ("Cant de alloc totales for mav       %6d \n", sq_q_alloc_for_mav);		/* multip var allocated */

		if (ffsta)
		{

		fprintf (hfsta,"%s","\n");
		fprintf (hfsta,"File: %s\n",finp);
		fprintf (hfsta,"Cantidad de lineas con coment camb  %6d \n", sq_lineas_com);
		fprintf (hfsta,"Cantidad de lineas con com elim     %6d \n", sq_comentarios_elim);
		fprintf (hfsta,"Cantidad de lineas divididas        %6d \n", sq_lineas_desdobladas);
		fprintf (hfsta,"Cantidad de integer cambiados       %6d \n", sq_integer);
		fprintf (hfsta,"Cantidad de real cambiados          %6d \n", sq_real);
		fprintf (hfsta,"Cantidad de logical cambiados       %6d \n", sq_logical);
		fprintf (hfsta,"Cantidad de character cambiados     %6d \n", sq_character);
		fprintf (hfsta,"Cantidad de lin cont cambiadas      %6d \n", sq_lcont);
		fprintf (hfsta,"Variables_no_convertidas            %6d \n", sq_variables_no_convertidas);
		fprintf (hfsta,"Var inits simples convertidos       %6d \n", sq_vinit_simple);
		fprintf (hfsta,"Lineas con cont mas convertidas     %6d \n", sq_lineas_con_mas_elim);
		fprintf (hfsta,"Comentarios al final de lineas elim %6d \n", sq_coment_al_final_elim);	
		fprintf (hfsta,"Cant de alloc totales               %6d \n", sq_q_alloc);
		fprintf (hfsta,"Cant de alloc totales for slc       %6d \n", sq_q_alloc_for_slc);
		fprintf (hfsta,"Cant de alloc totales for clc       %6d \n", sq_q_alloc_for_clc);
		fprintf (hfsta,"Cant de alloc totales f90 slc       %6d \n", sq_q_alloc_f90_slc);
		fprintf (hfsta,"Cant de alloc totales f90 clc       %6d \n", sq_q_alloc_f90_clc);
		fprintf (hfsta,"Cant de alloc totales for sav       %6d \n", sq_q_alloc_for_sav);		/* single var allocated */
		fprintf (hfsta,"Cant de alloc totales for mav       %6d \n", sq_q_alloc_for_mav);		/* multip var allocated */

		}
	}


	if (n == 11 )
	{
		printf ("Cant de alloc totales               %6d \n", sq_q_alloc);
		printf ("Cant de alloc totales for slc       %6d \n", sq_q_alloc_for_slc);
		printf ("Cant de alloc totales for clc       %6d \n", sq_q_alloc_for_clc);
		printf ("Cant de alloc totales f90 slc       %6d \n", sq_q_alloc_f90_slc);
		printf ("Cant de alloc totales f90 clc       %6d \n", sq_q_alloc_f90_clc);
		printf ("Cant de alloc totales for sav       %6d \n", sq_q_alloc_for_sav);		/* single var allocated */
		printf ("Cant de alloc totales for mav       %6d \n", sq_q_alloc_for_mav);		/* multip var allocated */
		printf ("Cant de alloc totales con stat      %6d \n", sq_q_alloc_for_sta);		/* for con stat ya hecho */
		printf ("Cant de alloc tot sin cont sing var %6d \n", sq_q_alloc_for_slc_sav);
		printf ("Cant de alloc tot con cont sing var %6d \n", sq_q_alloc_for_clc_sav);
		printf ("Cant de alloc tot sin cont mult var %6d \n", sq_q_alloc_for_slc_mav);
		printf ("Cant de alloc tot con cont mult var %6d \n", sq_q_alloc_for_clc_mav);


		if (ffsta)
		{
		fprintf (hfsta,"Cant de alloc totales               %6d \n", sq_q_alloc);
		fprintf (hfsta,"Cant de alloc totales for slc       %6d \n", sq_q_alloc_for_slc);
		fprintf (hfsta,"Cant de alloc totales for clc       %6d \n", sq_q_alloc_for_clc);
		fprintf (hfsta,"Cant de alloc totales f90 slc       %6d \n", sq_q_alloc_f90_slc);
		fprintf (hfsta,"Cant de alloc totales f90 clc       %6d \n", sq_q_alloc_f90_clc);
		fprintf (hfsta,"Cant de alloc totales for sav       %6d \n", sq_q_alloc_for_sav);		/* single var allocated */
		fprintf (hfsta,"Cant de alloc totales for mav       %6d \n", sq_q_alloc_for_mav);		/* multip var allocated */
		fprintf (hfsta,"Cant de alloc totales con stat      %6d \n", sq_q_alloc_for_sta);		/* for con stat ya hecho */
		fprintf (hfsta,"Cant de alloc tot sin cont sing var %6d \n", sq_q_alloc_for_slc_sav);
		fprintf (hfsta,"Cant de alloc tot con cont sing var %6d \n", sq_q_alloc_for_clc_sav);
		fprintf (hfsta,"Cant de alloc tot sin cont mult var %6d \n", sq_q_alloc_for_slc_mav);
		fprintf (hfsta,"Cant de alloc tot con cont mult var %6d \n", sq_q_alloc_for_clc_mav);
		}
	}

	if (n == 0)
	{
		printf ("hola Erno ...               \n");
		printf ("que tengas un buen dia  ... \n");
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}



}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	reportes 
 *
 * 	init de variables
 *
 * -----------------------------------------------------------------------------------
 */


int	borrar_stats()
{

	sq_lineas_com            = 0;
	sq_comentarios_elim      = 0;
	sq_lineas_desdobladas    = 0;
	sq_integer               = 0;	
	sq_real	                 = 0;	
	sq_logical	         = 0;
	sq_character             = 0;
	sq_lcont                 = 0;
	sq_vinit_simple          = 0;
	sq_lineas_con_mas_elim   = 0;
	sq_coment_al_final_elim  = 0;	

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



	/* borro estadisticas */
	borrar_stats();


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
		if (ffprb == 4)
			pro_prue4();
		if (ffprb == 5)
			pro_prue5();
		if (ffprb == 6)
			pro_prue6();
		if (ffprb == 7)
			pro_prue7();
		if (ffprb == 8)
			pro_prue8();
		if (ffprb == 9)
			pro_prue9();
	}	

	if ( ffexc)
	{
		if (ffexc == 1)	
			pro_exec1();
		if (ffexc == 2)
			pro_exec2();
		if (ffexc == 3)
			pro_exec3();
		if (ffexc == 4)
			pro_exec4();
		if (ffexc == 5)
			pro_exec5();
		if (ffexc == 6)
			pro_exec6();
		if (ffexc == 7)
			pro_exec7();
		if (ffexc == 8)
			pro_exec8();
		if (ffexc == 9)
			pro_exec9();
	}

	if ( ffpro)
	{
		if (ffpro == 1)	
			pro_proc1();
		if (ffpro == 2)
			pro_proc2();
		if (ffpro == 3)
			pro_proc3();
		if (ffpro == 4)
			pro_proc4();
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
		if (fftoo == 7)
			pro_tool7();
		if (fftoo == 8)
			pro_tool8();
		if (fftoo == 9)
			pro_tool9();
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
 *	el viejo y querido parser de tesis version I !!
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

	char	z[MAXV];
	sprintf (z,"bienvenido al parser !!!");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp)
		gp_uso(101);

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

	    /* opcion - proceso todas las lineas  */
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
						printf ("Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
						if (ffaux)
							fprintf (hfaux,"Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
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
				{
					if (ffout)
						fprintf (hfout,"%s\n",b1);
				}	
	
				/* grabo los tokens encontrados */
				for (j=0; j< q_tk; j++)
				{
					switch (gp_niveldes)
					{
	
						case 0:
							if (ffout)
								fprintf (hfout,"%s\n",tk[j]);
							break;
	
						case 1:
							if (ffout)
								fprintf (hfout,"%3d,%s\n",j,tk[j]);
							break;
	
						default:
							if (ffout)
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
				{	if (ffout)
						fprintf (hfout,"%s\n","EOL");
				}
	#endif
			} /* if gp sentencia */
	
	
	
		/* salida en formato sentencia */
		if (gp_fsentencia == 1)
		{


			/* grabo los tokens encontrados */
#if 0
			if (ffout)
				fprintf (hfout,"%s\n",b1);
#endif

			for (j=0; j< q_tk; j++)
			{
#if 0
				if (ffout)
					fprintf (hfout,"%3d,%s\n",j,tk[j]);
#endif
				if (ffout)
					fprintf (hfout,"%s",tk[j]);

				if (gp_fverbose("d1"))
					printf ("%3d,%s\n",j,tk[j]);

			}

#if 0
			/* agrego termino EOL para indentificar donde termino
			 * esto ya que tal vez haya un . en mitad de linea 
			 */
			if (gp_eol)
			{	if (ffout)
					fprintf (hfout,"%s\n","EOL");
			}
			else
			{	if (ffout)
					fprintf (hfout,"\n");
			}
#endif


			/* se termino la linea */
			if (ffout)
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
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}
	
	
	
	
/*
 * -----------------------------------------------------------------------------------
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
 *	cuenta caracteres de un archivo
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
	
	int	tabla1[256];
	int	tabla2[256];

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	tk[MAXT][MAXB];

	char	mostrar[16];

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso exec 1 \n\n",gp_tm());
	}
		

	if (!ffinp || !ffout) 
		gp_uso(102);


	/* es uno o dos archivos */
	f4 = 0;
	if (ffin2)
		f4 = 1;

	/* init de valores */
	for (i=0; i<256; i++)
	{	tabla1[i]=0;
		tabla2[i]=0;
	}
	

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
			tabla1[b1[p1]]++;
			
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
	
	
	
	/* cuento chars de segundo archivo */
	if (f4)
	{
		q_lin=0;
		while (fgets(b1, MAXB, hfin2) != NULL)
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
				tabla2[b1[p1]]++;
				
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
	}


	/* grabo resultados */

	for (j=0; j< 256; j++)
	{
		memset (mostrar,0,16);
		sprintf (mostrar,"%c",j);
		if (j == 8)
			sprintf (mostrar,"%s","Backspace");
		if (j == 9)
			sprintf (mostrar,"%s","TAB");
		if (j == 10)
			sprintf (mostrar,"%s","Line Feed");
		if (j == 11)
			sprintf (mostrar,"%s","Vertical Tab");
		if (j == 12)
			sprintf (mostrar,"%s","Form Feed");
		if (j == 13)
			sprintf (mostrar,"%s","Carriage Return");
		if (j == 27)
			sprintf (mostrar,"%s","Escape");
		if (j ==127)
			sprintf (mostrar,"%s","Delete");


		if (!f4)
			sprintf (b1,"Char %3d %7d   |%s|\n",j,tabla1[j],mostrar);
		else
			sprintf (b1,"%sChar %3d %7d %7d     |%s| \n", (abs(tabla1[j]-tabla2[j]) ? "X " : "  "),j,tabla1[j],tabla2[j] ,mostrar);


		fprintf (hfout,"%s",b1);

		if (gp_fverbose("d1"))
			printf ("%s",b1);
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale proceso exec 1 \n\n",gp_tm());
	}


}




/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 2
 *
 *	proceso de series de archivos indicados por lista 
 *
 * -----------------------------------------------------------------------------------
 */



#if 0

int	pro_exec2()
{

	/* exec */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra a proceso exec 2 \n\n",gp_tm());
	}
		

	/* exec */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale de proceso exec 2 \n\n",gp_tm());
	}

}

#endif








/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 2
 *
 *	proceso de series de archivos indicados por lista 
 *
 * -----------------------------------------------------------------------------------
 */

#if 1

/*
 *
 *	viene de prue2
 *
 *	abre archivo con lista de archivos a procesar (fuentes fortran)
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 */


#if 0

#define	MAX_QSRC		500		* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec2()
{

	int	i,j,k;
	char	d1[MAXB];
	char	d2[MAXB];
	int	ql,qlf;
	int	flag;
	int	q_ptr;

	FILE	*hwi,*hwo;


	char	z[MAXV];
	sprintf (z,"exec2");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffdat )
		gp_uso(103);



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
			qfv_load(hwi,fnq1,&qlf);

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
			(*tb[qf_ff]).ql = qlf;

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
#if 1
	ps_src1();
#endif


	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(604);

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



#endif



/*
 * -----------------------------------------------------------------------------------
 *
 *	ps_src1  
 *
 *	proceso un file
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	procesar todos los fuentes
 *
 *	OJO: esto quedo trunco ...
 *	proc4 ... carga todos los files como cadenas a estructuras ...
 *	y despues, vimos que procesar archivos con esa estructura ...
 *	se torna muy complicado...
 *	no es facil acceder a lin[var] ... lin[var+1] etc 
 */


int	ps_src1()
{

	int	ql_ini,ql_fin;
	int	i,j,k,l;
	int	f1,f2,f3,f4;
	int	pf,uf,ln;

	char	b1[MAXB];
	char	b2[MAXB];

	char	s1[MAXB];
	char	s2[MAXB];

	/* chequeo que esta todo bien */
	if (gp_fverbose("d3"))
	{
		printf ("\n\n\n");
	
		for (i=0; i< qf_ff; i++)
		{
			printf ("FF: %3d  %5d  |%s|\n", i, (*tb[i]).ql, (*tb[i]).n );
		}

		printf ("\n\n\n");
	}




	for (i=0; i< qf_ff; i++)
	{
		strcpy(b1, (*tb[i]).n);
		pf = (*tb[i]).pf;
		uf = (*tb[i]).uf;

		for (ln=pf; ln<=uf; ln++)
		{

			l_pars(ln,&q_tk );
			strcpy (b2, (*fnp[ln]).l );

			if (tiene_include_valido(b2,s1,s2))
			{
#if 0
				fprintf (hfout,"%-30.30s %-30.30s \n",b1,s1  );
#endif
				fprintf (hfout,"%-30.30s %-30.30s \n",s1,b1  );

			}

		}

	}

}



int	tiene_include_valido(s,l1,l2)
char	*s;
char	*l1;
char	*l2;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	n_inc;
	int	n_ap1,n_ap2;
	char	b1[MAXB];


	/* mientras true, hay esperanza */
	f1 = 1;

	/* si es comentario, no va */
	if (f1)
	{
		if (es_linea_comentario(s))
			f1 = 0;
	}

	/* si no figura la palabra include, no va */
	if (f1)
	{
		for (i=0, f2=0; !f2 && i<q_tk; i++)
		{
			if (!strcmp("include",pasar_a_minusc(tk[i])))
				f2 = 1, n_inc=i;
		}

		if (!f2)
			f1 = 0;	
	}

	/* si antes del include hay algo distinto a blancos, no va */
	if (f1)
	{
		for (i=0; f1 && i< n_inc && i< q_tk; i++)
			if (strcmp(" ",tk[i]))
				f1 = 0;

	}


	/* si despues del include, no hay blancos y un "'",  no va */
	if (f1)
	{	
		i = n_inc+1;
		while (tk[i][0] == ' ' && i< q_tk )
			i++;

		if (tk[i][0] == '\'')
			n_ap1 = i;
		else	
			f1 = 0;

	}


	/* si encontro un "'" ... veamos */
	if (f1)
	{
		if (tk[n_ap1+2][0] == '.')
			sprintf (b1,"%s.%s",tk[n_ap1+1],tk[n_ap1+3]);
		else
			f1 = 0;
	}


	if (f1)
	{
		if (es_nombre_de_include(b1) )
			strcpy(l1,b1);
		else
			f1 = 0;
	}


	return (f1);
}


int	es_nombre_de_include(s)
char	*s;
{

	int	i,j,k;
	int	f1,f2;
	char	c;


	f1 = 1;

	for (i=0; f1 && i<strlen(s); i++)
	{
		c = s[i];

		if (! ( c == '.' || c == '_' || ( c >= 'A' && c <= 'Z' ) || ( c >= 'a' && c <= 'z' ) || ( c >= '0' && c <= '9') ))
			f1 = 0;
	}

	return (f1);
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 3
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec3()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec3");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffdat )
		gp_uso(104);



	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
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
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

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
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
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

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	ex3_p2();
	ex3_p3();
	ex3_p4();


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



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 4
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec4()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec4");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffaux )
		gp_uso(105);



	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
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
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

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
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
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

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	ex4_p1();


	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(606);

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




/*
 * -----------------------------------------------------------------------------------
 *
 *	ex3_p2
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec3
 * hace algo con todas las lineas cargadas en memoria
 *
 * hfaux    source - include (resumen completo)
 */

int	ex3_p2()
{
	int 	i,j,k;
	int	f1,f2;
	int	f_mon,m_nl,m_nc;
	int	n_f;
	int	pf,uf,ql;
	int	n_info;
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	info[30][MAXB];
	int	s_info[30];
	char	nro[16];
	char	nro2[16];

	for (i=0; i<30; i++)
		s_info[i]=0;


	/* para todos los archivos  */
	for (j=0; j < qf_ff; j++)
	{
		f_mon = 0;

		strcpy (b3, (*tb[j]).n );
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;
		ql = uf - pf + 1;

		if (es_mon(b3))
			f_mon = 1;

		n_info = 0;
		sprintf (info[n_info],"%-34.34s",b3);

		if (f_mon)
		{	complejidad_mon(pf,uf,&m_nl,&m_nc);
			sprintf (nro2,"%3d %2d",m_nl,m_nc);
			sprintf (info[n_info],"%-34.34s                             %3d %2d",b3,m_nl,m_nc);
			info[n_info][60] = 'M';
		}

			

		for (i = pf; i <= uf; i++)
		{
			strcpy(b1,(*fnp[i]).l );

			if (!es_linea_comentario(b1) && tiene_include_v4(b1,b2))
			{
#if 0
				fprintf (hflog,"%-34.34s  %s\n", b3, b2);
				fprintf (hfout,"%-15.15s  %s\n", b2, b3);
#endif

				n_info++;
				sprintf (info[n_info],"%-34.34s %s",b3,b2);
			}



		}

		fprintf (hfaux,"%s\n","-------------------------------------------------------------------------");

		if (n_info == 0)
			fprintf (hfaux,"%3d %s\n",lne(j),info[n_info]);
		else
		{
			s_info[1]=1;
			if (n_info > 1)
			{
				qsort ( &info[1],n_info,MAXB,info_cmp1 );

				for (k=1; k <= n_info; k++)
				{

					if ( k == 1 )
					{
						s_info[k] = 1;
					}

					if ( k != 1 )
					{
						if ( !strcmp(info[k],info[k-1]) )
						{
							s_info[k] = s_info[k-1] + 1;
							s_info[k-1] = 0;
						}
						else
							s_info[k] = 1;
					}
				}

					
			}

			for (k=1, f2=1; k <= n_info; k++)
			{
				if (s_info[k])
				{	
					strcpy(nro,"   ");
					if (f2)
					{	sprintf (nro,"%3d",lne(j) );
						f2 =0;
					}

					if (f_mon)
						info[k][60]='M';

					fprintf (hfaux,"%s %-50.50s (%2d)\n",nro,info[k],s_info[k]);
				}
			}
		}

	}

	fprintf (hfaux,"%s\n","-------------------------------------------------------------------------");
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	ex3_p3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec3
 * hace algo con todas las lineas cargadas en memoria
 *
 * hfout ... include - file
 *
 */

int	ex3_p3()
{
	int 	i,j,k;
	int	f1,f2;
	int	f_mon,m_nl,m_nc;
	int	n_f;
	int	pf,uf;
	int	n_info;
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	info[700][MAXB];
	int	s_info[30];
	char	nro[16];
	char	nro2[16];

	for (i=0; i<30; i++)
		s_info[i]=0;


	n_info = 0;

	/* para todos los archivos  */
	for (j=0; j < qf_ff; j++)
	{
		f_mon = 0;

		strcpy (b3, (*tb[j]).n );
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

			
		for (i = pf; i <= uf; i++)
		{
			strcpy(b1,(*fnp[i]).l );

			if (!es_linea_comentario(b1) && tiene_include_v4(b1,b2))
			{

				sprintf (info[n_info],"%-16.16s %s",b2,b3);
				n_info++;
			}



		}

#if 0

		fprintf (hfout,"%s\n","-------------------------------------------------------------------------");

		if (n_info == 0)
			fprintf (hfaux,"%3d %s\n",lne(j),info[n_info]);
		else
		{
			s_info[1]=1;
			if (n_info > 1)
			{
				qsort ( &info[1],n_info,MAXB,info_cmp );

				for (k=1; k <= n_info; k++)
				{

					if ( k == 1 )
					{
						s_info[k] = 1;
					}

					if ( k != 1 )
					{
						if ( !strcmp(info[k],info[k-1]) )
						{
							s_info[k] = s_info[k-1] + 1;
							s_info[k-1] = 0;
						}
						else
							s_info[k] = 1;
					}
				}

					
			}

			for (k=1, f2=1; k <= n_info; k++)
			{
				if (s_info[k])
				{	
					strcpy(nro,"   ");
					if (f2)
					{	sprintf (nro,"%3d",lne(j) );
						f2 =0;
					}

					if (f_mon)
						info[k][60]='M';

					fprintf (hfaux,"%s %-50.50s (%2d)\n",nro,info[k],s_info[k]);
				}
			}
		}

#endif

	}

	qsort ( &info[0],n_info,MAXB,info_cmp2);

	for (k=0; k < n_info; k++)
	{

		fprintf (hfout,"%s\n",info[k]);
	}

}







/*
 * -----------------------------------------------------------------------------------
 *
 *	ex3_p4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec3
 * hace algo con todas las lineas cargadas en memoria
 *
 * hfout ... file - include
 *
 */

int	ex3_p4()
{
	int 	i,j,k;
	int	f1,f2;
	int	f_mon,m_nl,m_nc;
	int	n_f;
	int	pf,uf;
	int	n_info;
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	info[700][MAXB];
	int	s_info[30];
	char	nro[16];
	char	nro2[16];

	for (i=0; i<30; i++)
		s_info[i]=0;


	n_info = 0;

	/* para todos los archivos  */
	for (j=0; j < qf_ff; j++)
	{
		f_mon = 0;

		strcpy (b3, (*tb[j]).n );
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

#if 0
		if (es_mon(b3))
			f_mon = 1;

		n_info = 0;
		sprintf (info[n_info],"%-34.34s",b3);

		if (f_mon)
		{	complejidad_mon(pf,uf,&m_nl,&m_nc);
			sprintf (nro2,"%3d %2d",m_nl,m_nc);
			sprintf (info[n_info],"%-34.34s                             %3d %2d",b3,m_nl,m_nc);
			info[n_info][60] = 'M';
		}
#endif

			
		for (i = pf; i <= uf; i++)
		{
			strcpy(b1,(*fnp[i]).l );

			if (!es_linea_comentario(b1) && tiene_include_v4(b1,b2))
			{
#if 0
				fprintf (hflog,"%-34.34s  %s\n", b3, b2);
				fprintf (hfout,"%-15.15s  %s\n", b2, b3);
#endif

				sprintf (info[n_info],"%-36.36s %s",b3,b2);
				n_info++;
			}



		}

#if 0



		fprintf (hfout,"%s\n","-------------------------------------------------------------------------");

		if (n_info == 0)
			fprintf (hfaux,"%3d %s\n",lne(j),info[n_info]);
		else
		{
			s_info[1]=1;
			if (n_info > 1)
			{
				qsort ( &info[1],n_info,MAXB,info_cmp );

				for (k=1; k <= n_info; k++)
				{

					if ( k == 1 )
					{
						s_info[k] = 1;
					}

					if ( k != 1 )
					{
						if ( !strcmp(info[k],info[k-1]) )
						{
							s_info[k] = s_info[k-1] + 1;
							s_info[k-1] = 0;
						}
						else
							s_info[k] = 1;
					}
				}

					
			}

			for (k=1, f2=1; k <= n_info; k++)
			{
				if (s_info[k])
				{	
					strcpy(nro,"   ");
					if (f2)
					{	sprintf (nro,"%3d",lne(j) );
						f2 =0;
					}

					if (f_mon)
						info[k][60]='M';

					fprintf (hfaux,"%s %-50.50s (%2d)\n",nro,info[k],s_info[k]);
				}
			}
		}
#endif 

	}

	qsort ( &info[0],n_info,MAXB,info_cmp2);

	for (k=0; k < n_info; k++)
	{

		fprintf (hflog,"%s\n",info[k]);
	}

}




/*
 * -----------------------------------------------------------------------------------
 *
 *	ex4_p1
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec4
 * hace algo con todas las lineas cargadas en memoria
 *
 * hfaux    source - entrys ....
 *
 */

int	ex4_p1()
{
	int 	i,j,k,l;
	int	f1,f2;
	int	p1,p2;
	int	f_mon,m_nl,m_nc;
	int	n_f;
	int	pf,uf,uuf;
	int	n_info;
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	b6[MAXB];
	char	info[300][MAXB];
	int	s_info[300];
	char	nro[128];
	char	nro2[16];

	for (i=0; i<30; i++)
		s_info[i]=0;


	uuf = (*tb[qf_ff-1]).uf;

	/* para todos los archivos  */
	for (j=0; j < qf_ff; j++)
	{
		f_mon = 0;

		strcpy (b3, (*tb[j]).n );
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

		if (es_mon(b3))
			f_mon = 1;

printf ("ex4_p1: proceso file %2d |%s| \n",strlen(b3),b3);

		n_info = 0;
		sprintf (info[n_info],"%-34.34s",b3);

			

		for (i = pf; i <= uf; i++)
		{
			strcpy(b1,(*fnp[i]).l );

			if (!es_linea_comentario(b1) && tiene_entry_v1(b1,b2) && !primer_caracter_es_mas(b1) )
			{
				n_info++;
				strcpy(info[n_info],b2);

#if 0
				sprintf (info[n_info],"%-20.20s %3d %s"," ",n_info,b2);
#endif
printf ("file: %-30.30s entry: %s \n",b3,b2);
			}



		}

		fprintf (hfaux,"%s\n","-------------------------------------------------------------------------");


		strcpy(nro," ");
		if (n_info)
			sprintf (nro,"%-20.20s (%3d)"," ",n_info);
		strcat(info[0],nro);

		fprintf (hfaux,"%3d %s\n",lne(j),info[0]);
		if (n_info)
		{
			for (k=1; k <= n_info; k++)
			{
#if 0
				fprintf (hfaux,"%s\n",info[k]);
#endif
				fprintf (hfaux,"%-3.3s %3d %s\n"," ",k,info[k]);

#if 1
				/* loop machazo */
printf ("busco call |%s| \n",info[k]);

				for (l=0; l< uuf; l++)
				{
					strcpy(b4,(*fnp[l]).l );
					if (!es_linea_comentario(b4) && tiene_call_a_entry(b4,info[k]) )
					{
						primer_char(b4,&p1);
						strcpy(b5,f_name(l));
						fprintf (hfaux,"%-8.8s%-20.20s |%s\n"," ",b5,b4+p1);
					}

					strcpy(b4,(*fnp[l]).l );
					if (!es_linea_comentario(b4) && tiene_funcion_a_entry(b4,info[k]) )
					{
						strcpy(b6,minusculas(b4));

						if (!tiene_entry(b6) )
						{
						primer_char(b4,&p1);
						strcpy(b5,f_name(l));
						fprintf (hfaux,"%-8.8s%-20.20s |%s\n"," ",b5,b4+p1);
						}
					}





				}
#endif

			}
		}


#if 0
		if (n_info == 0)
			fprintf (hfaux,"%3d %s\n",lne(j),info[n_info]);
		else
		{
			s_info[1]=1;
			if (n_info > 1)
			{
				qsort ( &info[1],n_info,MAXB,info_cmp1 );

				for (k=1; k <= n_info; k++)
				{

					if ( k == 1 )
					{
						s_info[k] = 1;
					}

					if ( k != 1 )
					{
						if ( !strcmp(info[k],info[k-1]) )
						{
							s_info[k] = s_info[k-1] + 1;
							s_info[k-1] = 0;
						}
						else
							s_info[k] = 1;
					}
				}

					
			}

			for (k=1, f2=1; k <= n_info; k++)
			{
				if (s_info[k])
				{	
					strcpy(nro,"   ");
					if (f2)
					{	sprintf (nro,"%3d",lne(j) );
						f2 =0;
					}

					if (f_mon)
						info[k][60]='M';

					fprintf (hfaux,"%s %-50.50s (%2d)\n",nro,info[k],s_info[k]);
				}
			}
		}
#endif

	}

	fprintf (hfaux,"%s\n","-------------------------------------------------------------------------");
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 5
 *
 *	lista variables de fortran que tienen blancos intermedios 
 *
 * -----------------------------------------------------------------------------------
 */



/* 
 *	pro_exec5
 *	analiza include files como mthnmcom y namescom
 *	lista variables de fortran que tienen blancos intermedios 
 *
 */


int	pro_exec5()
{
	int	px,py;
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	q_lin,q_var;
	int	q_ptr;
	int	q_tk;
	int	p1,p2,p3,p4;
	int	pf,uf;
	int	sw1 = 0;		/* 0 grabo archivo normal, 1 grabo con marcas */
	int	es_int;
	int	es_chr;
	int	qv1,qv2;
	int	q1,q2;
	int	f_sigo;
	
	int	tabla1[256];
	int	tabla2[256];

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	b6[MAXB];
	char	tk[MAXT][MAXB];

	char	mostrar[16];

	char	z[MAXV];
	sprintf (z,"exec5");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}
		

	/* que la vida sea mas leve */
	if (!ffaux)
		agregar_ffaux("parser.err");

	if (!ffinp || !ffout || !fflog || !ffou3 || !ffou4) 
		gp_uso(102);



	/* cantidad de lineas  cargadas  */
	q_ptr = 0;

	fnq1 = &fnp[q_ptr];
	qfv_load(hfinp,fnq1,&q_lin);

	printf ("Cantidad de lineas cargadas   :  %5d \n",q_lin);
	printf ("\n");


#if 1


	q_var = 0;
	pf = 0;
	uf = q_lin -1;
	sprintf (b3,"      integer(kind=2),parameter:: ");
	sprintf (b5,"      character(len=1),parameter:: ");
	sprintf (b6,"      integer(kind=4),parameter:: ");


	qv1 = 0;
	qv2 = 0;

	/* reviso cada linea marco sobre las que hay que trabjar */
	for (j = pf; j <= uf; j++)
	{
		/* la linea j */
		strcpy (b1, (*fnp[ j ]).l );
		strcpy (b2, (*fnp[ j ]).l );
		(*fnp[ j ]).f1 = 0;

		/* parseo fila a tokens ... si tiene sentido  */
		l_pars(j,&q_tk);


		/*
		 * detectar si la linea es comentarios ...
		 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
		 */

		f4 = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f4 = 0;

		/* solo proceso lineas que no son comentario */
		if (f4 && ( tiene_integer(b1) || tiene_character(b1) ) )
		{	
			es_int = 0;
			es_chr = 0;

			/* atencion ! big tema aca !!!
			 * usamos el mismo proceso para dos files con diferentes variables !!!
			 * un file tiene integer (kind=4) ... no se detecto !! 
			 */

			if (tiene_integer(b1))
				es_int = 1;
			if (tiene_character(b1))
				es_chr = 1;

			/* ojo
			 * asi como esta, funciona solo para len=1 y kind=2 
			 *
			 * por el moemento, si no es asi, dejamos error ...
			 */


#if 0
			if (tiene_integer(b1) && !es_integer2(b1))
				error(6101);
			if (tiene_character(b1) && !es_character1(b1))
				error(6102);
#endif

			(*fnp[ j ]).f1 = 1;
			(*fnp[ j ]).f2 = 0;

			for (i=0, f2 = 1, f3 = 0, p1 = 0; f2 && i < strlen(b1); i++)
			{
				if (!strncmp(b1+i,"::",2))
				{	strcpy(b4,b1+i+3);
					f2 = 0;
					f3 = 1;
				}
			}

			if (f3 == 0)
				error(5001);


			if (es_int)
				strcpy(b1,b3);
			if (es_chr)
				strcpy(b1,b5);

			strcat(b1,b4);

			/* tiene blancos intermedios ... */
			for (i=0, f2 = 1, f3 = 0; f2 && i< strlen(b4); i++)
			{
				if (b4[i] == '=')
				{
					f2 = 0;
					f3 = 1;

					b4[i] = 0;
					if (b4[i-1] == ' ')
						b4[i-1] = 0;

			printf ("XXX b4: |%s| \n",b4);


				}
			}

			if (f3 == 0)
				error(5002);

			for (i=0, f2=0, f3=0; i<strlen(b4); i++)
				if ( b4[i] == ' ')
				{
					f3 = 1;
					f2++;
					(*fnp[ j ]).f2 = 1;

				}

			/* grabo la variable pura */
			if (f3 && ffou2)
			{
				fprintf (hfou2,"|%s|\n",b4);

				sprintf (vcb[qv1],"|%s|",b4);
				qv1++;
			}

			/* grabo la lista de variables con _ */
			if (!f3 && fflog)
			{
				fprintf (hflog,"|%s|\n",b4);
			}
			
			q_var++;
		}

		strcpy  ( (*fnp[ j ]).l, b1 );
	}


	printf ("Cantidad de lineas no comentarios  :  %5d \n",q_var);
	printf ("\n");



#endif



#if 1
	/* grabo resultados */
	sw1 = 0;

	/* reviso cada linea */
	for (j = pf; j <= uf; j++)
	{
		strcpy (b1, (*fnp[ j ]).l );

		if ( (*fnp[ j ]).f1 == 0)
		{
			fprintf (hfout,"%s\n",b1);
		}

		if ( (*fnp[ j ]).f1 == 1 && (*fnp[ j ]).f2 == 0)
		{
			if (sw1)
			{
				b1[0] = 'x';
			}
			fprintf (hfout,"%s\n",b1);
		}
		 
		if ( (*fnp[ j ]).f1 == 1 && (*fnp[ j ]).f2 == 1)
		{
			if (sw1)
			{
				b1[0] = 'x';
				b1[1] = '2';
			}
			fprintf (hfout,"%s\n",b1);
		}
		 

		if (gp_fverbose("d1"))
			printf ("%s\n",b1);
	}
#endif

	/* ordeno las variables vcb segun la cantidad de blancos que tengan
	 * y grabo
	 */

	if (ffou3)
	{
		qv2 = 0;

		for (i=6; i; i--)
		{
			for (j=0; j<qv1; j++)
			{
				for (k=0, q1=0; k<strlen(vcb[j]) ; k++)
					if (vcb[j][k] == ' ')
						q1++;
				
				if (i == q1)
				{
					strcpy(vcb_o[qv2],vcb[j]);
					qv2++;
				}	
			}
		}


		for (j=0; j<qv1; j++)
		{
			fprintf (hfou3,"%s\n",vcb_o[j]);
		}
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}


}





int	es_integer2(s)
char	*s;
{
	int	i;
	int	f_resultado, f_sigo;
	char	target[MAXB];

	strcpy(target,"kind=2");

	f_resultado = 0;
	for (i=0, f_sigo=1; f_sigo && i<strlen(s); i++)
	{
		if (!strncmp(s+i,target,strlen(target)))
			f_sigo = 0, f_resultado = 1;
	}

	return (f_resultado);
}



int	es_character1(s)
char	*s;
{
	int	i;
	int	f_resultado, f_sigo;
	char	target[MAXB];

	strcpy(target,"len=1");

	f_resultado = 0;
	for (i=0, f_sigo=1; f_sigo && i<strlen(s); i++)
	{
		if (!strncmp(s+i,target,strlen(target)))
			f_sigo = 0, f_resultado = 1;
	}

	return (f_resultado);
}











int	tiene_integer(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	f_tiene;
	char	b1[MAXB];

	strcpy(b1,pasar_a_minusc(s));

	for (i=0, f1=1, f_tiene=0; f1 && i < strlen(b1); i++)
	{
		if (!strncmp(b1+i,"integer",7))
		{
			f1 = 0;
			f_tiene = 1;
		}
	}

	return ( f_tiene );
}

	

int	tiene_character(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	f_tiene;
	char	b1[MAXB];

	strcpy(b1,pasar_a_minusc(s));

	for (i=0, f1=1, f_tiene=0; f1 && i < strlen(b1); i++)
	{
		if (!strncmp(b1+i,"character",9))
		{
			f1 = 0;
			f_tiene = 1;
		}
	}

	return ( f_tiene );
}

	




/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 6
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 6
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 *
 *	proceso para determinar variables con blancos intermedios
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec6()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec6");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffin2 || !ffin3 || !ffaux || !ffout || !ffou4 || !ffou5 || !ffou6  )
		gp_uso(105);



 	/* cargo las variables vcb */	
	q_vcb = 0;

	while (fgets(b1,MAXV,hfin2) != NULL)
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

			/* saco el ultimo | */
			if (b1[strlen(b1)-1] == '|')
				b1[strlen(b1)-1] = 0;
			else
				error(5003);

			/* copio desde el primer caracter despues del primer | */
			strcpy(vcb[q_vcb],b1+1);

printf ("XXX cargue vcb |%s| \n",vcb[q_vcb]);

			q_vcb++;
		}
	}

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de vcb cargadas : %6d\n",q_vcb);
	}



 	/* cargo las variables vsb */	
	q_vsb = 0;

	while (fgets(b1,MAXV,hfin3) != NULL)
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

			/* saco el ultimo | */
			if (b1[strlen(b1)-1] == '|')
				b1[strlen(b1)-1] = 0;
			else
				error(5004);

			/* copio desde el primer caracter despues del primer | */
			strcpy(vsb[q_vsb],b1+1);

printf ("XXX cargue vsb |%s| \n",vsb[q_vsb]);

			q_vsb++;
		}
	}

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de vsb cargadas : %6d\n",q_vsb);
	}





	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
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
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

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
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
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

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	if (gp_suboption == 1 )
	{
		/* hace el cambio de variables con blancos x misma version con _ */
		ex6_p1();
	}


	if (gp_suboption == 2 )
	{
		/* chequeo ... estas variables con blancos ... estan en otros fuentes que no t el include ? */
		ex6_p2();
	}

	if (gp_suboption == 3 )
	{
		/* chequeo ... estas variables con blancos reemp x _, ... estan en los fuentes que  t el include ? */
		ex6_p3();
	}

	if (gp_suboption == 4 )
	{
		/* chequeo ... estas variables con blancos reemp x _, ... estan en los fuentes que  t el include ? */
		ex6_p4();
	}

	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(603);

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






/*
 * -----------------------------------------------------------------------------------
 *
 *	ex6_p1
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec6
 * hace algo con todas las lineas cargadas en memoria
 *
 * v1
 * toma las variables vcb (var con blanks )
 * y reemplaza por la misma con '_'
 * trabaja sobre todos los fuentes cargados en vector 
 * genera version modificada en repo indicado
 *
 */

int	ex6_p1()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5,f_var_ok;
	int	p1,p2;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	qv1,qv2;
	int	qm1,qm2,qm3;

	strcpy(base_name,"empty");

	qv1 = 0;
	qm1 = 0;

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );

		strcpy (b2, pasar_a_minusc(b1));
		if ( !es_linea_comentario(b2) && tiene_coment_final (b2,&p1))
		{
			b2[p1] = 0;
		}
		l2 = strlen(b2);

		f_proceso = 1;
		if (linea_vacia(b2) || es_linea_comentario(b2))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							f_var_ok = 1;

							printf ("VW1 lin : |%s|\n",b2);
							printf ("VW2 var : |%s|\n",b3);
							printf ("VW3 cn1 : (%6d)\n",k1-1);
							printf ("VW4 cn2 : (%6d)\n",k1+strlen(b3));
							printf ("VW5 ca1 : (%c) \n",b2[k1-1]);
							printf ("VW6 ca2 : (%c) \n",b2[k1+strlen(b3)]);
							printf ("VW7 . . .  \n");


							/* la variable esta, pero dentro de un string ! 
							 * esto detecta lo simple :
							 * 'variable' ...
							 * pero faltaria que detecte 
							 * 'la variable esta dentro de mas caracteres '
							 */


							/* 'variable' */
							if (b2[k1-1] == '\'' && b2[k1+strlen(b3)] == '\'')
							{
								f_var_ok = 0;
							}

							/* "variable" */
							if (b2[k1-1] == '\"' && b2[k1+strlen(b3)] == '\"')
							{
								f_var_ok = 0;
							}

							/* 'variable,' */
							if (b2[k1-1] == '\'' && b2[k1+strlen(b3)] == ',' && b2[k1+strlen(b3)+1] == '\'')
							{
								f_var_ok = 0;
							}

							/* ' variable' */
							if (b2[k1-1] == ' ' && b2[k1+strlen(b3)] == '\'')
							{
								f_var_ok = 0;
							}

							/* 'variable ' */
							if (b2[k1-1] == '\'' && b2[k1+strlen(b3)] == ' ')
							{
								f_var_ok = 0;
							}

							/* 'variable-cr' */
							if (b2[k1-1] == '\'' && b2[k1+strlen(b3)] == '-' 
                                                                             && b2[k1+strlen(b3)+1] == 'c' && b2[k1+strlen(b3)+2] == 'r')
							{
								f_var_ok = 0;
							}


							/* 'variable-dr' */
							if (b2[k1-1] == '\'' && b2[k1+strlen(b3)] == '-' 
                                                                             && b2[k1+strlen(b3)+1] == 'd' && b2[k1+strlen(b3)+2] == 'r')
							{
								f_var_ok = 0;
							}




							if (f_var_ok)
							{

							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}

							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV5 lin : (%6d) |%s|\n",lne(nf),b1);
							printf ("VV6 - - - - - - \n");

							if (fflog)
							{
								fprintf (hflog,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),b0);
								fprintf (hflog,"%-30.30s     %05d |%s|\n",prog_name,lne(nf),b1);
								fprintf (hflog,"%s","\n");
							}

							if (ffaux)
							{
								fprintf (hfaux,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),vcb[k]);
							}

							if (ffout)
							{
								strcpy(vcb_o[qv1],vcb[k]);
								qv1++;
							}

							if (ffou4)
							{
								printf ("%4d %-30.30s |%s|\n",qm1,prog_name,vcb[k]);
								sprintf (vcb_p[qm1],"%-30.30s,|%s|",prog_name,vcb[k]);
     sprintf (vcb_c[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
           prog_name,vcb[k],40-strlen(vcb[k]), 40-strlen(vcb[k]), " " );
     sprintf (vcb_d[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
           prog_name,b5,40-strlen(b5), 40-strlen(b5), " " );

								qm1++;


							}



							}
						}
					}
				}		
	
			} /* while f5 */
		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */



	for (i=0; i<qm1; i++)
		printf ("%4d |%s|\n",i,vcb_p[i]);



	qsort(&vcb_o[0],qv1, MAXV, compare_vcb);

#if 1
    // Contador para elementos únicos
    int unique_count = 1;

    // Itera sobre el vector eliminando duplicados
    for (int i = 1; i < qv1; ++i) 
    {
        // Compara el elemento actual con el anterior
        if (strcmp(vcb_o[i], vcb_o[i - 1]) != 0) 
        {
            // Si son diferentes, mueve el elemento único al índice de `unique_count`
            strcpy(vcb_o[unique_count], vcb_o[i]);
            ++unique_count;
        }
    }

    // Actualiza la cantidad de elementos únicos
    qv2 = unique_count;
#endif


	if (ffout)
	{
		for (i=0; i<qv2; i++)
			fprintf (hfout,"|%s|\n",vcb_o[i]);
	}



	qsort(&vcb_p[0],qm1, MAXB, compare_vcb);

#if 1
    // Contador para elementos únicos
    unique_count = 1;

    // Itera sobre el vector eliminando duplicados
    for (int i = 1; i < qm1; ++i) 
    {
        // Compara el elemento actual con el anterior
        if (strcmp(vcb_p[i], vcb_p[i - 1]) != 0) 
        {
            // Si son diferentes, mueve el elemento único al índice de `unique_count`
            strcpy(vcb_p[unique_count], vcb_p[i]);
            ++unique_count;
        }
    }

    // Actualiza la cantidad de elementos únicos
    qm2 = unique_count;
#endif




	if (ffou4)
	{
		for (i=0; i<qm2; i++)
			fprintf (hfou4,"%s\n",vcb_p[i]);
	}



	qsort(&vcb_c[0],qm1, MAXB, compare_vcb);

#if 1
    // Contador para elementos únicos
    unique_count = 1;

    // Itera sobre el vector eliminando duplicados
    for (int i = 1; i < qm1; ++i) 
    {
        // Compara el elemento actual con el anterior
        if (strcmp(vcb_c[i], vcb_c[i - 1]) != 0) 
        {
            // Si son diferentes, mueve el elemento único al índice de `unique_count`
            strcpy(vcb_c[unique_count], vcb_c[i]);
            ++unique_count;
        }
    }

    // Actualiza la cantidad de elementos únicos
    qm2 = unique_count;
#endif

	qsort(&vcb_d[0],qm1, MAXB, compare_vcb);

#if 1
    // Contador para elementos únicos
    unique_count = 1;

    // Itera sobre el vector eliminando duplicados
    for (int i = 1; i < qm1; ++i) 
    {
        // Compara el elemento actual con el anterior
        if (strcmp(vcb_d[i], vcb_d[i - 1]) != 0) 
        {
            // Si son diferentes, mueve el elemento único al índice de `unique_count`
            strcpy(vcb_d[unique_count], vcb_d[i]);
            ++unique_count;
        }
    }

    // Actualiza la cantidad de elementos únicos
    qm3 = unique_count;
#endif


	if (qm2 != qm3 )
		error(5555);




	if (ffou5)
	{
		for (i=0; i<qm2; i++)
			fprintf (hfou5,"%s\n",vcb_c[i]);

	}

	if (ffou6)
	{
		for (i=0; i<qm3; i++)
			fprintf (hfou6,"%s\n",vcb_d[i]);
	}





}



#if 0
int compare_vcb(char *a, char *b) 
{
    return strcmp(a,b);
}
#endif


#if 0
int compare_vcb(a,b) 
const void *a;
const void *b;
{
    // Hacemos un cast a char * porque sabemos que estamos ordenando strings
    const char *ia = *(const char **)a;
    const char *ib = *(const char **)b;

    return strcmp(ia, ib);
}
#endif


#if 1
int	compare_vcb(p1,p2)
const	void	*p1;
const	void	*p2;
{
	int k;

	k = strcmp((const char *)p1,(const char *)p2);
	return k;
}

#endif



/*
 * -----------------------------------------------------------------------------------
 *
 *	ex6_p2
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec6
 * hace algo con todas las lineas cargadas en memoria
 *
 * v2
 * toma las variables vcb (var con blanks )
 * y busca en todos los programas cargados
 * trabaja sobre todos los fuentes cargados en vector 
 * (la idea es buscar estas variables en programas que no tienen 
 * el include mthnmcom.mon )
 *
 */

int	ex6_p2()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	qv1,qv2;


	strcpy(base_name,"empty");

	qv1 = 0;

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );
		strcpy (b2, pasar_a_minusc(b1));
		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}

							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV5 lin : (%6d) |%s|\n",lne(nf),b1);
							printf ("VV6 \n");

							if (fflog)
							{
								fprintf (hfaux,"%-40.40s %05d |%s|\n",prog_name,lne(nf),b0);
								fprintf (hfaux,"%s","\n");
							}

						}
					}
				}		
	
			} /* while f5 */
		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	ex6_p3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec6
 * hace algo con todas las lineas cargadas en memoria
 *
 * v3
 * toma las variables vcb (var con blanks ) 
 * reemp blanks con _
 * y busca en todos los programas cargados
 * trabaja sobre todos los fuentes cargados en vector 
 * (la idea es buscar estas variables en programas que tienen 
 * el include mthnmcom.mon )
 *
 */

int	ex6_p3()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;


	memset(b4,'X',MAXB);
	strcpy(base_name,"empty");

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{

		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;

			printf ("Trabajo con ... |%s|\n",prog_name);
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );
		strcpy(b2,(*fnp[i]).l );
#if 0
		strcpy (b2, pasar_a_minusc(b1));
#endif
		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					/* copio la variable vcb a buscar */
#if 0
					strcpy(b3,pasar_a_minusc(vcb[k]));
#endif
					strcpy(b3,vcb[k]);
					strcpy(b5,vcb[k]);

					/* cambio blanks x underscore */
					for (k2 = 0; k2 < strlen(b3); k2++)
						if (b3[k2] == ' ')
							b3[k2] = '_';
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strncpy(b2+k1,b4,strlen(b3));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV6 \n");

							if (fflog)
							{
								fprintf (hfaux,"%-40.40s  var: |%s| \n",prog_name,b5);
								fprintf (hfaux,"(%5d) |%s|\n",lne(nf),b0);
								fprintf (hfaux,"%s","\n");
							}
						}
					}
				}		
	
			} /* while f5 */
		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	ex6_p4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec6
 * hace algo con todas las lineas cargadas en memoria
 *
 * v4
 * toma las variables vcb (var con blanks ) 
 * reemp blanks con _
 * y busca en todos los programas cargados
 * trabaja sobre todos los fuentes cargados en vector 
 * (la idea es buscar estas variables en programas que tienen 
 * el include mthnmcom.mon )
 *
 */

int	ex6_p4()
{
	
}


#if 0

int	ex6_p4()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;


	memset(b4,'X',MAXB);
	strcpy(base_name,"empty");

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{

		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;

			printf ("Trabajo con ... |%s|\n",prog_name);
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );
		strcpy(b2,(*fnp[i]).l );
#if 0
		strcpy (b2, pasar_a_minusc(b1));
#endif
		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					/* copio la variable vcb a buscar */
#if 0
					strcpy(b3,pasar_a_minusc(vcb[k]));
#endif
					strcpy(b3,vcb[k]);
					strcpy(b5,vcb[k]);

					/* cambio blanks x underscore */
					for (k2 = 0; k2 < strlen(b3); k2++)
						if (b3[k2] == ' ')
							b3[k2] = '_';
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strncpy(b2+k1,b4,strlen(b3));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV6 \n");

							if (fflog)
							{
								fprintf (hfaux,"%-40.40s  var: |%s| \n",prog_name,b5);
								fprintf (hfaux,"(%5d) |%s|\n",lne(nf),b0);
								fprintf (hfaux,"%s","\n");
							}
						}
					}
				}		
	
			} /* while f5 */
		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */
}


#endif


/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 7
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 7
 *
 *	lee archivo "check" (salida de grep de variables en codebase )
 *	y formatea para que se vea lindo
 */


int	pro_exec7()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	int	f_proceso;
	int	f_sigo;
	int	p1,p2;
	char	b1[MAXB];
	char	b2[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec7");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout )
		gp_uso(105);



	/* cantidad de archivos y lineas totales cargadas  */
	ql = 0;

	while (fgets(b1,MAXB,hfinp) != NULL)
	{
		/* saco el fin de linea - contemplo 13 x fuentes fortran */
		for ( flag=0, j=strlen(b1); !flag && j >= 0; j--)
			if (b1[j] == '\n' )
			{	
				flag=1;
				if ( j && b1[j-1] == 13)
					b1[j-1]=0;
				else
					b1[j]=0;
			}

		f_proceso = 1;
		if (linea_vacia(b1) || b1[0] == '#' )
			f_proceso = 0;


		if (f_proceso)
		{

			for (j=0, f_sigo=1; f_sigo && j < strlen(b1); j++)
				if (b1[j]==':')
					p1 = j, f_sigo = 0;

			memset(b2,' ',30);
			b2[30]=0;

			strncpy(b2,b1,p1);
			strcat(b2,"|");
			strcat(b2,b1+p1+1);
			strcat(b2,"|");

			fprintf (hfout,"%s\n",b2);


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
 *	f_name
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	devuelve nombre de file en funcion de la linea de codigo nl
 *
 */
 

char	*f_name(nl)
int	nl;
{
	static char b1[MAXV];

	int	i,j,k;
	int	f1;
	int	pf,uf;
	
	for (j=0, f1=1; f1 && j < qf_ff; j++)
	{
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

		if (pf <= nl && nl <= uf)
		{
			strcpy (b1, (*tb[j]).n );
			f1 = 0;
		}
	}

	return (b1);
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	e_name
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	devuelve extension de file en funcion de la linea de codigo nl
 *
 * 	tipo_ext
 *	0  no lo encontro
 *	1  for
 *	2  f90
 */
 

char	*e_name(nl)
int	nl;
{
	static char b1[MAXV];
	static char b2[MAXV];
	char        b3[MAXB];

	int	i,j,k;
	int	l2;
	int	f1;
	int	pf,uf;
	int	f_sig;
	int	tipo_ext;

	
	for (j=0, f1=1; f1 && j < qf_ff; j++)
	{
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

if (gp_debug)
{
printf ("WWW4:  nl |%d| \n",nl);
printf ("WWW4:  j  |%d| \n",j);
printf ("WWW4:  pf |%d| \n",pf);
printf ("WWW4:  uf |%d| \n",uf);
printf ("WWW4:  n  |%s| \n", (*tb[j]).n );
}


		if (pf <= nl && nl <= uf)
		{
			strcpy (b1, (*tb[j]).n );
			f1 = 0;
if (gp_debug)
{
printf ("WWW4:  if |%s| \n", b1 );
}
		}
	}

	strcpy (b3,pasar_a_minusc(b1));

if (gp_debug)
{
printf ("WWW5: b1 |%s| \n",b1);
printf ("WWW6: b3 |%s| \n",b3);
}

	strcpy (b2,"ntf");
	tipo_ext = 0;
	f_sig = 1;
	l2 = strlen(b3);

	if (f_sig)
	{
		for (i=0; i<l2; i++)
			if (!strncmp(b3+i,"for",3))
			{
				strcpy(b2,"for");
				f_sig = 0;
			}
	}

	if (f_sig)
	{
		for (i=0; i<l2; i++)
			if (!strncmp(b3+i,"f90",3))
			{
				strcpy(b2,"f90");
				f_sig = 0;
			}
	}

		
if (gp_debug)
{
printf ("WWW7: b2 |%s| \n",b2);
}

	return (b2);
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	complejidad_mon
 *
 * -----------------------------------------------------------------------------------
 */


int	complejidad_mon(pf,uf,m_nl,m_nc)
int	pf;
int	uf;
int	*m_nl;
int	*m_nc;
{
	int	i,j,k;
	int	f_sig;
	int	f1,f2,f3;
	int	ql,qc;
	char	b1[MAXB];

	ql = 0;
	qc = 0;

	for (i = pf; i <= uf; i++)
	{
		strcpy(b1,pasar_a_minusc((*fnp[i]).l)  );

		if (!es_linea_comentario(b1))
		{
			ql++;

			for (j=0, f_sig = 1; f_sig && j < strlen(b1); j++)
			{	
				f3 = strncmp("common",b1+j,6);
printf ("complej: %5d f3: %d |%s| \n",j,f3,b1);

				if (!f3 && ( b1[j+6] == '/' || b1[j+7] == '/' || b1[j+8] == '/')  )
				{
		printf ("complejidad: BINGO !!! \n");

					f_sig = 0, qc ++;
				}
			}
		}
	}

	*m_nl = ql;
	*m_nc = qc;
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	es_mon
 *
 * -----------------------------------------------------------------------------------
 */


int	es_mon(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2;
	int	f_sig,f_ret;
	char	b1[MAXB];


	f_ret = 0;
	strcpy (b1,pasar_a_minusc(s));


	for (i=0, f_sig = 1; f_sig && i < strlen(s); i++)
		if (!strcmp(".mon",b1+i))
			f_ret = 1, f_sig = 0;

	return ( f_ret );
}
	


/*
 * -----------------------------------------------------------------------------------
 *
 *	info_cmp1
 *
 * -----------------------------------------------------------------------------------
 */

#if 0
int info_cmp1(const void *p1, const void *p2) 
{
    // Convertir punteros void a punteros char
    const char *a = (const char *)p1;
    const char *b = (const char *)p2;

    return strcmp(a, b);
}
#endif


int	info_cmp1(p1,p2)
const	void	*p1;
const	void	*p2;
{
	int k;

	k = strcmp((const char *)p1,(const char *)p2);

	if (gp_fverbose("d4"))
	{
		printf ("SSS info_cmp %s %s %d !! \n",p1,p2,k);
	}

	return k;
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	info_cmp2
 *
 * -----------------------------------------------------------------------------------
 */


int	info_cmp2(p1,p2)
const	void	*p1;
const	void	*p2;
{
	int k;

	k = strcmp((const char *)p1,(const char *)p2);

	if (gp_fverbose("d4"))
	{
		printf ("SSS info_cmp %s %s %d !! \n",p1,p2,k);
	}

	return k;
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	ex3_p1
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec3
 * hace algo con todas las lineas cargadas en memoria
 *
 */

int	ex3_p1()
{
	int 	i,j;
	int	f1;
	int	n_f;
	char	b1[MAXB];
	char	b2[MAXB];



	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
				n_f = j, f1=0;

		strcpy(b1,(*fnp[i]).l );

		if (!es_linea_comentario(b1) && tiene_include_v4(b1,b2))
		{
printf ("INC encontrado: %s \n",b2);

			fprintf (hflog,"%-30.30s  %s\n", (*tb[n_f]).n, b2);
			fprintf (hfout,"%-15.15s  %s\n", b2, (*tb[n_f]).n);
		}


#if 0
		strcpy(b1,(*fnp[i]).l );
		if ( es_linea_comentario(b1) )
		{
			strcpy(verif,analisis_comentario(b1));
			fprintf (hflog,"%s|%s|\n",verif,b1);
			fprintf (hflog,"%s\n",b1);
		}
#endif
	}

}




bool	tiene_include_v4(s,t) 
char	*s;
char	*t;
{
	// Define la cadena a buscar
	const 	char *keyword = "include";
	char	b1[MAXB];

	bool	r;
	strcpy	(b1,pasar_a_minusc(s));
	char 	*start = strstr(b1, keyword);


	r = false;

	if (start != NULL) 
	{
		// Avanza el puntero más allá de "include"
		start += strlen(keyword);

		// Salta los espacios en blanco
		while (isspace(*start)) 
		{
		    start++;
		}

		// Verifica si el siguiente carácter es un apóstrofe
		if (*start == '\'') 
		{
		    // Salta el apóstrofe inicial
		    start++; 

		    // Encuentra el apóstrofe final
		    char *end = strchr(start, '\'');
		    if (end != NULL) 
		    {
		        // Copia el nombre de archivo sin las comillas en t
		        strncpy(t, start, end - start);
		        t[end - start] = '\0';
		        r =  true;
		    }
		}
	}

	// resultado
	return r;
}




#if 0
#include <stdio.h>
#include <ctype.h>
#endif

int primer_char(char *s, int *p)
{
    int pos = 0;
    while (s[pos])
    {
        if (!isspace(s[pos]))
        {
            *p = pos;
            return 1;
        }
        pos++;
    }
    return 0;
}

#if 0
int main()
{
    char s[] = "    abc 123";
    int p;

    int resultado = primer_char(s, &p);

    if (resultado)
    {
        printf("La posición del primer carácter distinto de blanco es: %d\n", p);
    }
    else
    {
        printf("No se encontró ningún carácter distinto de blanco.\n");
    }

    return 0;
}
#endif















#if 0
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#endif


// Función para verificar si una cadena es un nombre válido en Fortran
int es_nombre_valido2(const char *str)
{
    if (!isalpha(*str)) // El primer carácter debe ser una letra
        return 0;
    
    while (*str && *str != '_')
    {
        if (!isalnum(*str))
            return 0;
        str++;
    }
    
    // Si encontramos un '_', verificar la siguiente parte
    while (*str)
    {
        if (*str == '_')
        {
            str++;
            if (!isalnum(*str)) // Después de '_' debe haber al menos una letra o número
                return 0;
        }
        else if (!isalnum(*str))
        {
            return 0;
        }
        str++;
    }

    return 1;
}

// Función principal para verificar si se encuentra "call nombre_valido" en la cadena s
int tiene_call_a_entry(char *s, char *t)
{
    char b1[MAXB];

    strcpy(b1,pasar_a_minusc(s));

    char *ptr = b1;
    char *call_pos = strstr(ptr, "call");

    if (!call_pos)
        return 0;

    ptr = call_pos + 4; // Avanzar después de "call"
    while (*ptr && isspace(*ptr)) // Saltar espacios en blanco
        ptr++;

    // Verificar si el nombre válido después de "call" coincide con el nombre válido en t
    if (strncmp(ptr, t, strlen(t)) == 0)
    {
        ptr += strlen(t);
        while (*ptr && isspace(*ptr)) // Saltar espacios en blanco después de nombre_valido
            ptr++;
        
        if (*ptr == '\0' || *ptr == '(' || *ptr == '!') // Asegurarse de que no hay caracteres extra después del nombre válido
            return 1;
    }

    return 0;
}

#if 0
int main()
{
    char s[] = "call   str1_str2_str3  ";
    char t[] = "str1_str2_str3";

    int resultado = tiene_call_a_entry(s, t);

    if (resultado)
    {
        printf("Se encontró la expresión 'call %s' en la línea.\n", t);
    }
    else
    {
        printf("No se encontró la expresión 'call %s' en la línea.\n", t);
    }

    return 0;
}
#endif



// Subrutina que verifica si "nombre_valido" es invocada como función en la línea
int tiene_funcion_a_entry(char *linea, char *nombre_valido) {
    char b1[MAXB];
    strcpy(b1, pasar_a_minusc(linea));

    char *ptr = b1;
    char *func_pos = strstr(ptr, nombre_valido);

    while (func_pos) {
        ptr = func_pos + strlen(nombre_valido);

        // Saltar espacios en blanco después del nombre de la función
        while (*ptr && isspace(*ptr))
            ptr++;

        // Verificar si está seguido de un paréntesis de apertura
        if (*ptr == '(') {
            ptr++; // Avanzar después de "("
            int paren_count = 1;

            // Verificar el contenido dentro de los paréntesis
            while (*ptr && paren_count > 0) {
                if (*ptr == '(')
                    paren_count++;
                else if (*ptr == ')')
                    paren_count--;
                ptr++;
            }

            // Si se cerraron todos los paréntesis, es una invocación válida
            if (paren_count == 0)
                return 1;
        }

        // Buscar la próxima aparición de nombre_valido
        func_pos = strstr(func_pos + 1, nombre_valido);
    }

    return 0; // No se encontró una invocación válida
}



int tiene_entry(const char *linea) 
{
    const char *ptr = linea;
    int	f1;


    // Convertir a minúsculas para hacer la búsqueda insensible a mayúsculas
    char buffer[MAXB];
    int i = 0;

    while (*ptr && i < MAXB) 
    {
        buffer[i++] = tolower(*ptr++);
    }
    buffer[i] = '\0';

    // Buscar "entry" en el string
    char *entry_pos = strstr(buffer, "entry");
    while (entry_pos) 
    {
        // Verificar que "entry" no esté precedido ni seguido por caracteres alfanuméricos o guiones bajos
        if ((entry_pos == buffer || !isalnum(*(entry_pos - 1)) && *(entry_pos - 1) != '_') &&
            isspace(*(entry_pos + 5))) 
        {
            return TRUE;
        }
        // Buscar la próxima aparición de "entry"
        entry_pos = strstr(entry_pos + 1, "entry");
    }

    return FALSE;
}

int	primer_caracter_es_mas(s)
char	*s;
{
	int	i,j,k;
	int	f_res, f_cont;

	f_res = 0;


	/* caso     "+   entry at ...." */
	if (!f_res)
	{
		for (i=0, f_cont=1; f_cont && i<strlen(s); i++)
			if (s[i] == '+')
				f_res=1, f_cont=0;
	}

	return f_res;
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_entry_v1
 *
 * -----------------------------------------------------------------------------------
 */


#if 0
int	tiene_entry_v1(s,t)
char	*s;
char	*t;
{





}
#endif



#if 0
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#endif

// Función para verificar si una cadena es un nombre válido en Fortran
int es_nombre_valido(const char *str)
{
    if (!isalpha(*str)) // El primer carácter debe ser una letra
        return 0;
    
    while (*str && *str != '_')
    {
        if (!isalnum(*str))
            return 0;
        str++;
    }
    
    // Si encontramos un '_', verificar la siguiente parte
    while (*str)
    {
        if (*str == '_')
        {
            str++;
            if (!isalnum(*str)) // Después de '_' debe haber al menos una letra o número
                return 0;
        }
        else if (!isalnum(*str))
        {
            return 0;
        }
        str++;
    }

    return 1;
}

// Función principal para buscar la expresión
int tiene_entry_v1(char *s, char *t)
{
    char b1[MAXB];
    strcpy(b1,pasar_a_minusc(s));


    char *ptr = b1;
    char *entry_pos = strstr(ptr, "entry");

    if (!entry_pos)
        return 0;

    ptr = entry_pos + 5; // Avanzar después de "entry"
    while (*ptr && isspace(*ptr)) // Saltar espacios en blanco
        ptr++;

    char nombre_valido[256];
    int i = 0;

    while (*ptr && (isalnum(*ptr) || *ptr == '_'))
    {
        nombre_valido[i++] = *ptr++;
    }

    nombre_valido[i] = '\0';

    if (!es_nombre_valido(nombre_valido))
        return 0;

    while (*ptr && isspace(*ptr)) // Saltar espacios en blanco
        ptr++;

    if (*ptr == '(')
    {
        ptr++;
        while (*ptr && isspace(*ptr)) // Saltar espacios en blanco
            ptr++;
    }

    if (strstr(ptr, "return"))
        return 0;

    strcpy(t, nombre_valido);
    return 1;
}


#if 0
int main()
{
    char s[] = "entry   str1_str2_str3  ";
    char t[256];

    int resultado = tiene_entry_v1(s, t);

    if (resultado)
    {
        printf("Se encontró la expresión. Nombre válido: %s\n", t);
    }
    else
    {
        printf("No se encontró la expresión o contiene 'return'.\n");
    }

    return 0;
}
#endif











#if 0

bool	tiene_include_v3(s,t) 
char	*s;
char	*t;
{
	// Define la cadena a buscar
	const 	char *pattern = "   include  '";
	int 	l1;
	char	b1[MAXB];
	bool	r;

	r = false;
	l1 = strlen(pattern);
	strcpy (b1,pasar_a_minusc(s));

printf ("TIENE1:  r: %d  b1: |%s| \n",r,b1);


	// Busca el patrón en el string 
	char *start = strstr(b1, pattern);

	if (start != NULL) 
	{
		// Encuentra el inicio del nombre de archivo
		start += l1;
		char *end = strchr(start, '\'');
        
		if (end != NULL) 
		{
			// Copia el nombre de archivo sin las comillas en t
			strncpy(t, start, end - start);
			t[end - start] = '\0';
			r = true;
		}
	}

printf ("TIENE2:  r: %d  b1: |%s| \n",r,b1);

	// resultado
	return r;
}
#endif



#if 0

int	tiene_include_v2(s,t)
char	*s;
char	*t;
{
	int	i,j,k;
	int	p1,p2,p3;
	int	f1,f2,f3,f_sig,f_go;
	int	m1,m2;
	int	f_res;
	char	b1[MAXB];
	char	b2[MAXB];

	f_res = 0;
	f_sig = 1;

	/* tiene la palabra include ? */
	if (f_sig)
	{
		f_sig = 0;

		for (i=0 , f_go = 1;  f_go && i < strlen(s); i++)
			if (!strcmp("include", pasar_a_minusc(s+i)))
				p1 = i+7, f3 = 1, f2 = 0;

	}

	return (f_res);
}

#endif

/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_include
 *
 * -----------------------------------------------------------------------------------
 */

#if 0

int	tiene_include(s,t)
char	*s,*t;
{

	int	i,j,k;

	f1 = 0;

	/* tiene la palabra include ... ? */
	for (i=0 , f2=1, f3=0; f2 && i < strlen(s); i++)
		if (!strcmp("include", pasar_a_minusc(s+i)))
			p1 = i+7, f3 = 1, f2 = 0;

	/* despues de include, solo puede haber blancos y un ' */
	if (f3)
	{	
		for(i=p1, f2=1, f4=0; f2 && i < strlen(s); i++)
			
	

	s_count = 0;

	p1 = 0;
	f1 = 1;
	f2 = 1;
	f3 = 1;
	l1 = 0;
	i  = 0;


	while ( f2 )
	{
		c = b1[p1 + i];
		k = 0;
			
		if ( c >= 'a' && c <= 'z' )
			k = 1;

		if ( c >= 'A' && c <= 'Z' )
			k = 1;

		if ( c >= '0' && c <= '9' )
			k = 2;

		if ( c == ' ' )
			k = 3;

		if ( c == '_' )
			k = 4;

		if ( c == '.' )
			k = 5;

		if ( c == '/' )
			k = 6;

printf ("while: i: %d f9: %d k/c: %c%c |%s| \n",i,f9,c,b1[1+i+1],s);

		switch (k)
		{
			case 	6:
				if ( !f9 && i == 0 )
					f9 = 1, i++;

				if ( !f9 && i )
					i++;

				if ( f9 && i > 0 )
					f2 = 0;

				break;

			default:
				if ( c == 0 )
					f2 = 0;
				i++;
				break;
					

#if 0
				/* no es caracter valido en la cadena, terminamos */
			case	0:
				f2 = 0;
				if (f1 == 0)
					f9 = i;
				else
					f9 = 0;
				break;

				/* es letra */
			case	1:
				if (f1)
					f1=0;
				i++;
				break;

				/* es numero , solo si hubo una letra antes */
			case	2:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;


				/* aceptamos casos en los que hay blancos como parte del nombre !! */
			case 	3:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;

				/* aceptamos casos en los que hay _ como parte del nombre !! */
			case 	4:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;
#endif
		}			
	}




}


#endif













		



/*
 * -----------------------------------------------------------------------------------
 *
 *	analisis comentario
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * analiza un comentario para preparar entrenamiento de IA
 *
 * 00 - no hay certeza de lo que es 
 * 01 - es un comentario
 * 05 - es una linea de codigo
 * 
 */


char	*analisis_comentario(s)
char	*s;
{
	static char	verif[8];
	int	i,j,k,f1,f2;
	int	l1,l2,l3,l4;


	memset(verif,0,8);

	strcpy(verif,"00");
	f1 = 0;

	/* lineas tipo rayas, separadores etc */
	if (!f1)
	{
		for (i=0, l1=0, l2=0, l3=0, l4=0; i < strlen(s); i++)
		{		
			if (s[i] == 'C')
				l1++;
			if (s[i] == 'c')
				l2++;
			if (s[i] == '!')
				l3++;
			if (s[i] == '*')
				l4++;
		}

		if (l1 > 20 || l2 > 20 || l3 > 20 || l4 > 20 )
			f1 = 1;
	}

	/* lineas vacias */
	if (!f1)
	{
		if (strlen(s) < 4)
			f1 = 1;
	}



	sprintf (verif,"%2d",f1);
	
	return (verif);
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 3
 *
 *	exec aparte ...
 *
 * -----------------------------------------------------------------------------------
 */



#if 0

int	pro_exec3()
{

	/* prueba */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra a proceso exec 3 \n\n",gp_tm());
	}
		


	/* prueba */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale de proceso exec 3 \n\n",gp_tm());
	}

}

#endif





/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 8
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 8
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 *
 *	proceso para hacer mapa de variables de todo el codebase
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec8()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec8");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp)
		gp_uso(115);



	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
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
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

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
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
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

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	if (gp_suboption == 1 )
	{
		/* sub ocpcion 1 -opt=1 */
		ex8_p1();
	}


	if (gp_suboption == 2 )
	{
		/* sub opcion 2 -opt=2 */
		ex8_p2();
	}

	if (gp_suboption == 3 )
	{
		/* sub opcion 3 -opt=3 */
		ex8_p3();
	}


	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
		{
			printf ("No puedo abrir: |%s| \n",d2);
			error(605);
		}

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





/*
 * -----------------------------------------------------------------------------------
 *
 *	ex8_p1
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec8
 * hace algo con todas las lineas cargadas en memoria
 *
 *
 */

int	ex8_p1()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	p1,p2;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	qv1,qv2;
	int	qm1,qm2,qm3;
	int	n_var;


	strcpy(base_name,"empty");

	qv1 = 0;
	qm1 = 0;

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );

		/* trabajemos con minusculas */
		strcpy (b2, pasar_a_minusc(b1));


		if ( !es_linea_comentario(b2) && tiene_coment_final (b2,&p1))
		{
			b2[p1] = 0;
		}
		l2 = strlen(b2);


		f_proceso = 1;
		if (linea_vacia(b2) || es_linea_comentario(b2))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	

			v_pars(b2,&n_var,v_size,v_name );

			for (i=0; i<n_var; i++)
			{
				memset(b4,0,sizeof(b4));
				strncpy(b4,v_name[i],v_size[i]);
				fprintf (hfout,"%30.30s %2d |%s| \n",prog_name,i,b4);
			}



#if 0
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}

							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV5 lin : (%6d) |%s|\n",lne(nf),b1);
							printf ("VV6 \n");

#if 0
							if (fflog)
							{
								fprintf (hflog,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),b0);
								fprintf (hflog,"%-30.30s     %05d |%s|\n",prog_name,lne(nf),b1);
								fprintf (hflog,"%s","\n");
							}

							if (ffaux)
							{
								fprintf (hfaux,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),vcb[k]);
							}

							if (ffout)
							{
								strcpy(vcb_o[qv1],vcb[k]);
								qv1++;
							}

							if (ffou4)
							{
								printf ("%4d %-30.30s |%s|\n",qm1,prog_name,vcb[k]);
								sprintf (vcb_p[qm1],"%-30.30s,|%s|",prog_name,vcb[k]);
     sprintf (vcb_c[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
           prog_name,vcb[k],40-strlen(vcb[k]), 40-strlen(vcb[k]), " " );
     sprintf (vcb_d[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
           prog_name,b5,40-strlen(b5), 40-strlen(b5), " " );

								qm1++;

							}
#endif
						}
					}
				}		
	
			} /* while f5 */
#endif

		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */



}







/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 9
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 9
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec9()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec4");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffaux )
		gp_uso(105);
	if (!ffsta)
		agregar_ffsta("tool.sta");



	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
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
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

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
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
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

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	if (gp_suboption == 1 )
	{
		/* busca allocate en todos los programas    */
		ex9_p1();
	}



	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(607);

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}


	fclose(hwo);

	mostrar_reportes("exec9");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}








/*
 * -----------------------------------------------------------------------------------
 *
 *	ex9_p1
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec9
 * hace algo con todas las lineas cargadas en memoria
 *
 * detecta si tiene sentencia allocate
 * devuelve true / false, y algunos parametros ...
 *
 */

#if 0
int	ex9_p1()
{

}
#endif


#if 1
int	ex9_p1()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	f_stat;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	tipo_ext;

	char	m0[MSTR];

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	strcpy(base_name,"empty");

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* stats */
	sq_q_alloc         = 0;
	sq_q_alloc_for_slc = 0;
	sq_q_alloc_for_clc = 0;
	sq_q_alloc_f90_slc = 0;
	sq_q_alloc_f90_clc = 0;
	sq_q_alloc_for_sav = 0;
	sq_q_alloc_for_mav = 0;
	sq_q_alloc_for_sta = 0;
	sq_q_alloc_for_slc_sav = 0;		/* cant tot de lineas sin cont con allocate single var */
	sq_q_alloc_for_clc_sav = 0;		/* cant tot de lineas con cont con allocate single var */
	sq_q_alloc_for_slc_mav = 0;		/* cant tot de lineas sin cont con allocate multi var */
	sq_q_alloc_for_clc_mav = 0;		/* cant tot de lineas con cont con allocate multi var */



	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{

		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));
		strcpy(exte_name,e_name(i));

		if (!strncmp(exte_name,"for",3))
			tipo_ext = 1;

		if (!strncmp(exte_name,"f90",3))
			tipo_ext = 2;

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;

			printf ("Trabajo con ... (%s) |%s|\n",exte_name,prog_name);
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1, pasar_a_minusc(b0));
		strcpy(b2, pasar_a_minusc(b0));

		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	

			if (tiene_allocate(b1))
			{
				sq_q_alloc++;

				c2++;

				fprintf (hfout,"%05d %-30.30s (%s) %05d |%s|\n",
                                        c2,
					prog_name,
					exte_name,
					i-pf+1,
					b0);

				if (tipo_ext == 1)
					fprintf (hfaux,"%s\n",b0);

				if (tipo_ext == 2)
					fprintf (hflog,"%s\n",b0);

				/* es .for */
				if(tipo_ext == 1 ) 
				{
					c3++;

					if (!tiene_mas( (*fnp[i+1]).l ) )
					{
						sq_q_alloc_for_slc++;
	
						fprintf (hfou2,"%05d %-30.30s (%s) %05d |%s|\n",
	                                       		c3, prog_name, exte_name, i-pf+1, (*fnp[i]).l);

						strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );
						f_stat = tiene_stat(b1);

						if (!tiene_multiple_vars(b1))  
						{
							/* es un allocate en una linea con una sola variable */
							sq_q_alloc_for_sav++;
							sq_q_alloc_for_slc_sav++;

							/* verifico si ya tiene stat */
							if (!f_stat)
							{

							}
							else
							{
								sq_q_alloc_for_sta++;
							}
						}
						else
						{
							/* es un allocate en una linea con mas de una variable */
							sq_q_alloc_for_mav++;
							sq_q_alloc_for_slc_mav++;
						}
					}
					else
					{
						sq_q_alloc_for_clc++;
						k=0;
						memset(m0,0,MSTR);
						do
						{
							fprintf (hfou2,"%05d %-30.30s (%s) %05d |%s|\n",
		                                       		c3, prog_name, exte_name, i-pf+1+k, (*fnp[i+k]).l);

							if (!es_linea_comentario( (*fnp[i+k]).l ))
							{
							strcpy(b1, pasar_a_minusc( (*fnp[i+k]).l) );
							strcpy(b1, trim_beg(b1));
							strcpy(b1, trim_end(b1));
							strcpy(b1, trim_blanks(b1));
							strcat(m0,b1);
							}
							k++;
						}
						while ( tiene_mas(  (*fnp[i+k]).l ) || 
                                                        (es_linea_comentario( (*fnp[i+k]).l) && tiene_mas( (*fnp[i+k+1]).l))   );

					
printf ("RRR10 |%s| \n",m0);
						if (!tiene_multiple_vars(m0))
						{
							/* es un allocate en varias lineas con una sola variable (poco probable) */
							sq_q_alloc_for_sav++;
							sq_q_alloc_for_clc_sav++;

						}
						else
						{
							/* es un allocate en varias lineas con varias variables  */
							sq_q_alloc_for_mav++;
							sq_q_alloc_for_clc_mav++;

						}
	

/* EEE */					
					}
					
				}

				/* es .f90 */
				if(tipo_ext == 2)
				{
					c4++;

					if (!tiene_amper( (*fnp[i]).l ) )
					{
					sq_q_alloc_f90_slc++;
					fprintf (hfou3,"%05d %-30.30s (%s) %05d |%s|\n",
                                       		c4,
						prog_name,
						exte_name,
						i-pf+1,
						(*fnp[i]).l);
					}
					else
					{
					sq_q_alloc_f90_clc++;
					k=0;
					while ( tiene_amper(  (*fnp[i+k]).l )   )
					{
					fprintf (hfou3,"%05d %-30.30s (%s) %05d |%s|\n",
                                       		c4,
						prog_name,
						exte_name,
						i-pf+1+k,
						(*fnp[i+k]).l);
					k++;
					}
					
					fprintf (hfou3,"%05d %-30.30s (%s) %05d |%s|\n",
                                       		c4,
						prog_name,
						exte_name,
						i-pf+1+k,
						(*fnp[i+k]).l);
					}
				}


			}

		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */
}


#endif


/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_multiple_vars
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * 	tiene_multiple_vars recibe un string donde todo el allocate fue
 *	metido en una sola linea 
 *	encuentra donde empieza y termina el area de allocate
 *	y devuele true si tiene mas de una variable allocada
 *	y false si es una sola variable
 *	tambien arma un vector de punteros a string,
 *	con cada variable allocada 
 */

int	tiene_multiple_vars(s)
char	*s;
{
	char	b0[MSTR];

	int	f_res;
	int	f_sigo;
	int	f_open_par;
	int	f_opt;

	int	p1,p2;
	int	i,j,k;
	int	l1,l2;
	int	c1,c2,c3;

	int	f1;

	int	st1,st2,st3,st4;

	/* copio string para trabajar local */
	strcpy(b0,s);
	l2 = strlen(b0);

	/* recorto si hay comentarios al final de la linea
	 * hay un solo caso con allocate  ... !real *4! .... 
	 */
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '!' && b0[i-1] != '4')
			f_sigo = 0, b0[i] = 0;
	}

	/* largo definitivo de la sentencia */
	l2 = strlen(b0);
	

	f_res = 0;
	p1    = 0;
	p2    = l2 -1;

	st1 = 0;
	st2 = 0;
	st3 = 0;

		
	/* la primer palabra tiene que ser allocate */
	st1 = 0;
	for (i=0, f_sigo=1 ; f_sigo && i<=p2; i++)
	{	if (b0[i] == ' ' || b0[i] == '\t')
			p1++;
		else
		{	
			if (!strncmp(b0+i,"allocate",8))
			{
				f_sigo = 0;
				st1 = 1;
				p1 = i+8;
			}
		}
	}
	
printf ("RRR0 paso 1 - rimer palabra es allocate          \n");
printf ("RRR1 s           |%s| \n",s);
printf ("RRR2 b1          |%s| \n",b0);
printf ("RRR3 st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);


	/* puede haber blancos entre allocate y ( ... los quito */
	while (b0[p1] == ' ')
	{
		l2 = strlen(b0)-1;
		for (k=p1; k<l2; k++)
			b0[k] = b0[k+1];
		b0[l2]=0;
		p2--;
	}


printf ("RRR0 paso 2 - si hay blancos corro la linea          \n");
printf ("RRR1 s           |%s| \n",s);
printf ("RRR2 b1          |%s| \n",b0);
printf ("RRR3 st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);

	/* el ultimo caracter salvo blancos, es un parentesis que cierra */
	st2 = 0;
	l2 = strlen(b0);
	for (i=l2-1, f_sigo=1; f_sigo && i; i-- )
		if (b0[i] == ' ')
			p2--;
		else
		{
			if (b0[i] == ')')
			{
				f_sigo = 0;
				st2 = 1;
				p2  = i;
			}
			else
			{
				f_sigo = 0;
			}
		}

printf ("RRR0 paso 3 - el ultimo caracter salvo blanco es )           \n");
printf ("RRR1 s           |%s| \n",s);
printf ("RRR2 b1          |%s| \n",b0);
printf ("RRR3 st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
printf ("RRR4 p1 p2       |%2d,%2d|  \n",p1,p2);
printf ("RRR5 b0[p1]      |%c| \n",b0[p1]);
printf ("RRR6 b0[p2]      |%c| \n",b0[p2]);


	/* caracteres especificos de allocate entre ( y ) */
	st3 = 1;
	for (i=p1; i<=p2; i++)
	{
		if (!((b0[i] >= 'a' && b0[i] <= 'z') || 
                      (b0[i] >= '0' && b0[i] <= '9') || 
		      b0[i] == '=' ||						/* por lineas con el bendito stat= */
                      b0[i] == ' ' || b0[i] == ':' || b0[i] == ',' || 
                      b0[i] == '-' || b0[i] == '(' || b0[i] == ')' || 
	              b0[i] == '_' || b0[i] == '*' || b0[i] == '+'    )  )
		{
			st3 = 0;
		}
	}

	
printf ("RRR0 paso 4 - caracteres especificos entre ( y )          \n");
printf ("RRR1 s           |%s| \n",s);
printf ("RRR9 b1          |%s| \n",b0);
printf ("RRR9 st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
printf ("RRR4 p1 p2       |%2d,%2d|  \n",p1,p2);
printf ("RRR5 b0[p1]      |%c| \n",b0[p1]);
printf ("RRR6 b0[p2]      |%c| \n",b0[p2]);



	f_sigo = 1;
	c2  = 0;
	c3  = 0;
	st4 = 0;
	i   = p1;

	while (f_sigo)
	{
		f_opt = 1;

		if (f_opt && c2 == 0 && b0[i] == '(' )
		{
			c2 = 1;
			f_opt = 0;
		}

#if 0
		if (f_opt && c2 == 1 && b0[i] == ')' )
		{
			c2 = 0;
			f_opt = 0;
		}
#endif

		if (f_opt && c2 >= 1 && b0[i] == '(' )
		{
			c2++;
			f_opt = 0;
		}

		if (f_opt && c2 > 1 && b0[i] == ')' )
		{
			c2--;
			f_opt = 0;
		}

		if (f_opt && c2 == 1 && b0[i] == ',')
		{
			c3++;
			f_opt = 0;
		}
 
		i++;

		if (i> p2)
		{
			f_sigo = 0;
		}

	}

	if (c3)
	{
		if (!tiene_stat(b0))
			st4 = 1;
	}


printf ("RRR0 paso 5 - hay comas sin estar entre    ( y )  \n");
printf ("RRRA s           |%s| \n",s);
printf ("RRR2 b1          |%s| \n",b0);
printf ("RRRA st1 st2 st3 |%d,%d,%d,%d| \n",st1,st2,st3,st4);
printf ("RRR4 p1 p2       |%2d,%2d|  \n",p1,p2);
printf ("RRR5 b0[p1]      |%c| \n",b0[p1]);
printf ("RRR6 b0[p2]      |%c| \n",b0[p2]);


	if (st1 && st2 && st3 && st4)
		f_res = 1;

	
			
	
printf ("RRR0 paso 5           \n");
printf ("RRR7 f_res      |%d| \n",f_res);


	if (f_res == 0)
		printf ("RR11 |%s|\n",s);

	if (f_res == 1)
		printf ("RR12 |%s|\n",s);



	return (f_res);

}


char	*trim_beg(s)
char	*s;
{
	static	char	b0[MAXB];
	int	i,j,k;
	int	p1;
	int	l1;
	int	f_sig;

	l1=strlen(s);

	p1 = 0;

	for (i=0, f_sig=1; f_sig && i<l1; i++)
		if (s[i] != ' ' && s[i] != '\t' && s[i] != '+')
			p1=i, f_sig=0;

	strcpy(b0,s+p1);
	
	return (b0);
}

	
char	*trim_end(s)
char	*s;
{
	static	char	b0[MAXB];
	int	i,j,k;
	int	p1;
	int	l1,l2;
	int	f_sigo;

	l2=strlen(s);
	strcpy(b0,s);


	/* recorto si hay comentarios al final de la linea
	 * hay un solo caso con allocate  ... !real *4! .... 
	 */
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '!' && b0[i-1] != '4')
			f_sigo = 0, b0[i] = 0;
	}
	
	return (b0);
}



int	tiene_allocate(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 8;
	l1 = strlen(s);

#if 0
	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
#endif
	for (i=0, f_res=0         ;          i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate",k1))
		{
			f_try = 1;

			if (f_try && s[i+k1] == 'd' )
				f_try = 0;

			if (f_try && s[i+k1] == 'a' )
				f_try = 0;

			if (f_try && s[i+k1] == '_' )
				f_try = 0;

			if (f_try && tiene_deallocate(s))
				f_try = 0;

			if (f_try && tiene_call(s))
				f_try = 0;

			if (f_try && tiene_pentry(s))
				f_try = 0;

			if (f_try && tiene__allocate(s))
				f_try = 0;

			if (f_try && tiene_er_message(s))
				f_try = 0;

			if (f_try && tiene_amazingly(s))
				f_try = 0;

			if (f_try && tiene_3possible(s))
				f_try = 0;

			if (f_try && tiene_allocatex(s))
				f_try = 0;

			if (f_try && tiene_remainder(s))
				f_try = 0;

			if (f_try && tiene_proport(s))
				f_try = 0;

			if (f_try && tiene_room(s))
				f_try = 0;

			if (f_try && tiene_arrays(s))
				f_try = 0;

			if (f_try)
			{	f_res=1;
#if 0
				f_sig=0;
#endif
			}
		}
	}

	return (f_res);
}
	





int	tiene_deallocate(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 10;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"deallocate",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_call(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 4;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"call",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_pentry(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 5;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"entry",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}


int	tiene__allocate(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 9;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"_allocate",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}


int	tiene_er_message(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 10;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"er_message",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}



int	tiene_amazingly(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 9;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"amazingly",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}




int	tiene_3possible(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 10;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"3 possible",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_allocatex(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 10;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate x",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_remainder(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 18;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate remainder",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_proport(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 16;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate proport",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_room(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 13;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate room",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_arrays(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 15;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate arrays",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}



int	tiene_stat(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 5;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"stat=",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}












/*
 * -----------------------------------------------------------------------------------
 *
 *	v_pars
 *
 *	parser de lineas, devolviendo variables ... en lugar de tokens 
 *
 * -----------------------------------------------------------------------------------
 */



void v_pars(s, n, vn, vp) 
char	*s;
int	*n;
int	*vn;
char	**vp;
{

    int start;
    int len;
    int i;

    // Inicializar el contador de variables
    *n = 0; 
    len = strlen(s);
    i = 0;

    while (i < len) 
    {
        // Saltar espacios en blanco
        while (i < len && isspace(s[i])) 
        {
            i++;
        }

        // Si encontramos una letra, es el inicio de una variable
        if (i < len && isalpha(s[i])) 
        {
            // Guardar el puntero al inicio de la variable
            vp[*n] = &s[i]; 
            start = i;

            // Avanzar hasta el final de la variable
            while (i < len && (isalnum(s[i]) || s[i] == ' ' || s[i] == '_')) 
            {
                i++;
            }

            // Guardar la longitud de la variable
            vn[*n] = i - start; 

            (*n)++; 
        } 
        else 
        {
            i++;
        }
    }



	printf ("v_pars .... \n");
	printf ("n:    %2d \n",*n);
	for (i=0; i< *n; i++)
	{
		printf ("i: %2d  len: %2d  str: |%s| \n",i,vn[i],vp[i]);
	}

}



/*
 * -----------------------------------------------------------------------------------
 *
 *	ex8_p2
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec8
 * hace algo con todas las lineas cargadas en memoria
 *
 *
 */

int	ex8_p2()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	p1,p2;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	qv1,qv2;
	int	qm1,qm2,qm3;

	strcpy(base_name,"empty");

	qv1 = 0;
	qm1 = 0;

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );

		strcpy (b2, pasar_a_minusc(b1));
		if ( !es_linea_comentario(b2) && tiene_coment_final (b2,&p1))
		{
			b2[p1] = 0;
		}
		l2 = strlen(b2);

		f_proceso = 1;
		if (linea_vacia(b2) || es_linea_comentario(b2))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}

							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV5 lin : (%6d) |%s|\n",lne(nf),b1);
							printf ("VV6 \n");

#if 0
							if (fflog)
							{
								fprintf (hflog,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),b0);
								fprintf (hflog,"%-30.30s     %05d |%s|\n",prog_name,lne(nf),b1);
								fprintf (hflog,"%s","\n");
							}

							if (ffaux)
							{
								fprintf (hfaux,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),vcb[k]);
							}

							if (ffout)
							{
								strcpy(vcb_o[qv1],vcb[k]);
								qv1++;
							}

							if (ffou4)
							{
								printf ("%4d %-30.30s |%s|\n",qm1,prog_name,vcb[k]);
								sprintf (vcb_p[qm1],"%-30.30s,|%s|",prog_name,vcb[k]);
     sprintf (vcb_c[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
           prog_name,vcb[k],40-strlen(vcb[k]), 40-strlen(vcb[k]), " " );
     sprintf (vcb_d[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
           prog_name,b5,40-strlen(b5), 40-strlen(b5), " " );

								qm1++;

							}
#endif
						}
					}
				}		
	
			} /* while f5 */
		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */



}







/*
 * -----------------------------------------------------------------------------------
 *
 *	ex8_p3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec8
 * hace algo con todas las lineas cargadas en memoria
 *
 *
 */

int	ex8_p3()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	p1,p2;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	qv1,qv2;
	int	qm1,qm2,qm3;
	int	n_var;

	static char	b2[MAXB];

	strcpy(base_name,"empty");

	qv1 = 0;
	qm1 = 0;

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );

		/* trabajemos con minusculas */
		strcpy (b2, pasar_a_minusc(b1));


		if ( !es_linea_comentario(b2) && tiene_coment_final (b2,&p1))
		{
			b2[p1] = 0;
		}
		l2 = strlen(b2);


		f_proceso = 1;
		if (linea_vacia(b2) || es_linea_comentario(b2))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	

			v_pars(b2,&n_var,v_size,v_name );

			printf ("VW0:  - - - - - - -\n");
			printf ("VW1:  b2     : |%s|\n",b2);
			printf ("VW2:  n_var  : %2d \n",n_var);
			printf ("VW3:  \n");
			for (j=0; j<n_var; j++)
			{
				printf ("VW4:  %2d %2d \n", j,v_size[j]);
				memset(b4,0,sizeof(b4));
				strncpy(b4,v_name[j],v_size[j]);
				printf ("VW5:  %2d %2d |%s| \n", j,v_size[j],v_name[j]);
				printf ("VW5:  %2d %2d |%s| \n", j,v_size[j],b4);
			}
			printf ("VW9:  \n");


#if 0
			for (i=0; i<n_var; i++)
			{
				memset(b4,0,sizeof(b4));
				strncpy(b4,v_name[i],v_size[i]);
				fprintf (hfout,"%30.30s %2d |%s| \n",prog_name,i,b4);
			}
#endif



#if 0
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}

							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV5 lin : (%6d) |%s|\n",lne(nf),b1);
							printf ("VV6 \n");

#if 0
							if (fflog)
							{
								fprintf (hflog,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),b0);
								fprintf (hflog,"%-30.30s     %05d |%s|\n",prog_name,lne(nf),b1);
								fprintf (hflog,"%s","\n");
							}

							if (ffaux)
							{
								fprintf (hfaux,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),vcb[k]);
							}

							if (ffout)
							{
								strcpy(vcb_o[qv1],vcb[k]);
								qv1++;
							}

							if (ffou4)
							{
								printf ("%4d %-30.30s |%s|\n",qm1,prog_name,vcb[k]);
								sprintf (vcb_p[qm1],"%-30.30s,|%s|",prog_name,vcb[k]);
     sprintf (vcb_c[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
           prog_name,vcb[k],40-strlen(vcb[k]), 40-strlen(vcb[k]), " " );
     sprintf (vcb_d[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
           prog_name,b5,40-strlen(b5), 40-strlen(b5), " " );

								qm1++;

							}
#endif
						}
					}
				}		
	
			} /* while f5 */
#endif

		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */



}












/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prueba 1
 *
 *	pruebas ...
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	ejercicios con estructuras, vectores, punteros
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
	char	b1[MAXB];

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
	char	b1[MAXB];

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
	char	b1[MAXB];

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
 *
 *	carga todos los archivos en el vector de memoria,
 *	o sea, TODOS los fuentes, estan en memoria
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

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

	if (!ffinp || !ffout || !ffdat )
		gp_uso(106);



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
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

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
#if 0
	p_src1();
	p_src2();
#endif
	p_src3();



	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(608);

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



/*
 * -----------------------------------------------------------------------------------
 *
 *	p_src3         
 *
 *	busca patrones de la forma   var = / xxxxxx /
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	busca patrones de inicializacion vieja de variables
 *	de la forma var = / xxxx /
 *
 */


int	p_src3()
{

	int	i,j,k,k1,l;
	int	f1,f2,f3,f4,f5;
	int	pf,uf;
	int	m1,m2;
	int	l1;
	int	p1;
	int	fl_n;

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];

	char	d1[MAXR];
	char	d2[MAXR];


	memset (b3,'X',MAXB);

	/* por cada uno de los fuentes cargados */
	for (fl_n=0; fl_n< qf_ff; fl_n++)
	{
		if (gp_fverbose("d1"))
		{
			printf ("proceso: %6d %6d |%s|\n",
				(*tb[fl_n]).pf,(*tb[fl_n]).uf,(*tb[fl_n]).n);

		}


		/* primera y ultima fila del source */
		pf = (*tb[fl_n]).pf;
		uf = (*tb[fl_n]).uf;

		/* reviso cada linea */
		for (j = pf; j <= uf; j++)
		{
			/* la linea j */
			strcpy (b1, (*fnp[ j ]).l );
			strcpy (b0, (*fnp[ j ]).l );

			/* parseo fila a tokens ... si tiene sentido  */
			l_pars(j,&q_tk);


			/*
			 * detectar si la linea es comentarios ...
			 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
			 */

			f4 = 1;
			if (es_linea_comentario(b1))
				f4 = 0;
	
			/* solo proceso lineas que no son comentario */
			if (f4)
			{	
				f5 = 1;
				while ( f5 )
				{

					strcpy(b2,b1);

					f5 = 0;
					if (p1 = es_cadena_int_src3(b1,&m1))
					{	f5 = 1;

						memset (d1,0,MAXR);
						strncpy (d1,b1+p1,m1);
		printf ("YYY: |%s| \n",d1);
						memset (d2,0,MAXR);
						strcpy(d2,d1);
						d2[0] = '=';
						d2[m1-1] = ' ';
		printf ("YYY: |%s| \n",d2);


						strncpy(b1+p1,b3,m1);
						strncpy(b1+p1,d2,m1);

						sprintf (b4,"f:%-30.30s l:%06d |%s| \n", (*tb[fl_n]).n,j,b2 );
						sprintf (b5,"f:%-30.30s l:%06d |%s| \n", (*tb[fl_n]).n,j,b1 );
						
						if (gp_fverbose("d3"))
						{
							printf ("TTX1\n");
							printf ("TTX2%s",b4);
							printf ("TTX3%s",b5);
							printf ("TTX4\n");
						}

						if (ffout)
						{
							fprintf (hfout,"%s",b4);
							fprintf (hfout,"%s",b5);
							fprintf (hfout,"%s","\n");
						}

						if (fflog)
						{
							fprintf (hflog,"%s",b4);
							fprintf (hflog,"%s",b5);
							fprintf (hflog,"%s","\n");
						}

					}
				}

			}	


			/* grabo la linea */
			if ( 1 )
			{
#if 1
				/* cuando terminos pruebas, va este */
				strcpy ((*fnp[ j ]).l, b1 );
#endif

#if 0
				strcpy ((*fnp[ j ]).l, b0 );
#endif
			}
		}
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	es_cadena_int_src3  
 *
 *	es cadena interesante para sub p_src3  - busca var = / xxxx /
 *
 * -----------------------------------------------------------------------------------
 */


int	es_cadena_int_src3(s,largo)
char	*s;
int 	*largo;
{
	char	c,d;
	int	i,j,k;
	int	l1;
	int	p1,p2,p3;
	int	f1,f2,f3,f5;
	int	m1;


	/* ej. log  variable=/.false./    */
	/* ej. char variable=/'X'/        */


	m1 = 0;
	f5 = 0;
	l1 = 0;
	p1 = 0;
	
	for (i=0, f1 = 1, f2 = 0;  f1 && s[i]; i++)
	{
		c = s[i];
		d = s[i+1];

		if ( !f2 && i > 10 && c == '!')
			f1 = 0;

		if ( !f2 && c == '/' && d == '/')
			f1 = 0;

		if ( !f2 && c == '/' )
			f2 = 1, p1 = i;

		if ( f2 && l1 && c != '/' )
		{
			f3 = 0;

			/* caracteres que aceptamos como parte de var=/xxxxx/ */
			if ( c >= 'A' && c <= 'Z' ) 
				f3 = 1;
			if ( c >= 'a' && c <= 'z' ) 
				f3 = 1;
			if ( c >= '0' && c <= '9' ) 
				f3 = 1;
			if ( c == '_' || c == '.' )
				f3 = 1;
			if ( c == '\'' )
				f3 = 1;
			if ( c == '-' )
				f3 = 1;
			if ( c == ' ' )
				f3 = 1;

			if (!f3)
				f1 = 0;
		}

		if ( f2 && l1 && c == '/' )
		{
			l1++;
			f1 = 0;
			f2 = 0;
			f5 = p1;
		}

		if ( f2 )
			l1++;
	}	
			
	printf ("TTT5:es_cadena_int_src3:          l1:%2d f5:%2d |%s| \n",l1,f5,s);

	*largo = l1;
	return (f5);
}



#if 0

/*
 *	es_cadena_interesante
 *
 *	busca en un string (linea de source)
 *	inicializacion de variables formato antiguo  
 *	var = / xxxx /
 *	
 */

int	es_cadena_int_src3(s)
char	*s;
{
	char	c;
	int	i,j,k;
	int	l1;
	int	p1,p2,p3;
	int	f1,f2,f3;
	int	f9;
	int	target;
	int	s_count;
	char	b1[MAXB];
	char	b2[MAXB];



	target = 10;	/* miremos cadenasde mas de 10 caracteres por ahora */
	f9 = 0;		/* cadena interesante 0 no 1 si */

	strcpy(b1,s);


	s_count = 0;

	p1 = 0;
	f1 = 1;
	f2 = 1;
	f3 = 1;
	l1 = 0;
	i  = 0;

printf ("es_cadena: |%s| \n",s);

	while ( f2 )
	{
		c = b1[p1 + i];
		k = 0;
			
		if ( c >= 'a' && c <= 'z' )
			k = 1;

		if ( c >= 'A' && c <= 'Z' )
			k = 1;

		if ( c >= '0' && c <= '9' )
			k = 2;

		if ( c == ' ' )
			k = 3;

		if ( c == '_' )
			k = 4;

		if ( c == '.' )
			k = 5;

		if ( c == '/' )
			k = 6;

printf ("while: i: %d f9: %d k/c: %c%c |%s| \n",i,f9,c,b1[1+i+1],s);

		switch (k)
		{
			case 	6:
				if ( !f9 && i == 0 )
					f9 = 1, i++;

				if ( !f9 && i )
					i++;

				if ( f9 && i > 0 )
					f2 = 0;

				break;

			default:
				if ( c == 0 )
					f2 = 0;
				i++;
				break;
					

#if 0
				/* no es caracter valido en la cadena, terminamos */
			case	0:
				f2 = 0;
				if (f1 == 0)
					f9 = i;
				else
					f9 = 0;
				break;

				/* es letra */
			case	1:
				if (f1)
					f1=0;
				i++;
				break;

				/* es numero , solo si hubo una letra antes */
			case	2:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;


				/* aceptamos casos en los que hay blancos como parte del nombre !! */
			case 	3:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;

				/* aceptamos casos en los que hay _ como parte del nombre !! */
			case 	4:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;
#endif
		}			
	}



	return (f9);

}



#endif







/*
 * -----------------------------------------------------------------------------------
 *
 *	trim_blanks         
 *
 *	elimina blancos al final de una linea
 *
 * -----------------------------------------------------------------------------------
 */


char	*trim_blanks(s)
char	*s;
{
	static	char	b[MAXB];
	int	i;
	int	f4;
	
	strcpy(b,s);

	/* blancos al final */
	for (i=strlen(b)-1, f4=1 ; i && f4 ; i-- )
		if (b[i] == ' '  )
			b[i]=0;
		else
			f4=0;

	return (b);
}


 
/*
 * -----------------------------------------------------------------------------------
 *
 *	pasa_filtro         
 *
 *	verifica si es palabra reservada del lenguaje
 *
 * -----------------------------------------------------------------------------------
 */



int	pasa_filtro(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2,f3;
	char	b1[MAXB];
	char	b2[MAXB];

	int	target;


	target = 20;	/* por ahora dejasmos este largo ... */


	strcpy(b1,s);
	f1 = 0;
	f2 = 1;


	if (f2 && !strncmp("real",pasar_a_minusc(b1),4))
		f2 = 1, f1 = 31;
	
	if (f2 && !strncmp("integer",pasar_a_minusc(b1),7))
		f2 = 1, f1 = 32;
	
	if (f2 && !strncmp("logical",pasar_a_minusc(b1),7))
		f2 = 1, f1 = 33;
	
	if (f2 && !strncmp("character",pasar_a_minusc(b1),9))
		f2 = 1, f1 = 34;
	
	if (f2 && !strncmp("if",pasar_a_minusc(b1),2))
		f2 = 1, f1 = 35;
	
	if (f2 && !strncmp("use",pasar_a_minusc(b1),3))
		f2 = 1, f1 = 21;
	
	if (f2 && !strncmp("call",pasar_a_minusc(b1),4))
		f2 = 1, f1 = 22;
	
	if (f2 && !strncmp("subroutine",pasar_a_minusc(b1),10))
		f2 = 1, f1 = 23;

	if (f2 && !strncmp("include",pasar_a_minusc(b1),7))
		f2 = 1, f1 = 24;
	



	return (f1);

}




/*
 *	es_cadena_interesante
 *
 *	busca en un string (linea de source)
 *	nombres 'largos' (pongamos, mayor a 30 chars )
 *	
 */

int	es_cadena_interesante(s)
char	*s;
{
	int	i,j,k;
	int	l1;
	int	p1,p2,p3;
	int	f1,f2,f3;
	int	f9;
	int	target;
	char	c;
	char	b1[MAXB];
	char	b2[MAXB];



	target = 10;	/* miremos cadenasde mas de 10 caracteres por ahora */
	f9 = 1;		/* cadena interesante 0 no 1 si */

	strcpy(b1,s);

#if 1


	p1 = 0;
	f1 = 1;
	f2 = 1;
	f3 = 1;
	l1 = 0;
	i  = 0;


	while ( f2 )
	{
		c = b1[p1 + i];
		k = 0;
			
		if ( c >= 'a' && c <= 'z' )
			k = 1;

		if ( c >= 'A' && c <= 'Z' )
			k = 1;

		if ( c >= '0' && c <= '9' )
			k = 2;

		if ( c == ' ' )
			k = 3;

		if ( c == '_' )
			k = 4;

		switch (k)
		{
				/* no es caracter valido en la cadena, terminamos */
			case	0:
				f2 = 0;
				if (f1 == 0)
					f9 = i;
				else
					f9 = 0;
				break;

				/* es letra */
			case	1:
				if (f1)
					f1=0;
				i++;
				break;

				/* es numero , solo si hubo una letra antes */
			case	2:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;


				/* aceptamos casos en los que hay blancos como parte del nombre !! */
			case 	3:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;

				/* aceptamos casos en los que hay _ como parte del nombre !! */
			case 	4:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;
		}			
	}


#endif



	return (f9);

}




/*
 *	una forma de encontrar long names ...
 *
 */


int	p_src1()
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
		gp_uso(107);
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
 *	pro_prue 4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	prue4
 *
 *	hago un malloc de un espacio y lo convierto en vector de punteros 
 *	a estructuras.
 */


#if 0
typedef	struct	tfn	*fnptr;
typedef	struct	tfn
{	char	l[HUG2];		/* por ahora, despues malloc */
	int	f1;			/* usos varios */
	int	f2;
	int	f3;
}	node;
#endif


/* bloque */
#if 1

int	pro_prue4()
{
	int	i,j,k;
	int	f1,f2,f3;
	int	x1,x2;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;

	fnptr	*pp;

	char	z[MAXV];
	sprintf (z,"prue4");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!1 || !2 )
		gp_uso(108);


	x1 = 200;

	pp = (void *) malloc( x1 * sizeof (fnptr) );
	if ( pp == NULL )
		error(333);
	printf ("malloc ok !! \n");



	for (i=0; i<x1; i++)
	{
		pp[i] = (fnptr ) malloc ( sizeof (node) );
		if ( pp[i] == NULL )
			error(334);
	}

	for (i=0; i<x1; i++)
	{
		(*pp[i]).f1 = 1000 + i;
		(*pp[i]).f2 = 2000 + i;
	}
 
	for (i=0; i<x1; i++)
	{
		printf ("%3d %5d %5d \n",i,(*pp[i]).f1 , (*pp[i]).f2 );
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
 *	pro_prue 5
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	prue5
 *
 *	carga un fuente fortran entero sobre estuctura de cadena de estrcts 
 *	si queremos cargar toda la base en memoria, y queremos extender 
 *	un programa, tendriamos que estar moviendo 500,000 lineas a cada rato !
 */

/*
 *	2018-04-21 
 *
 *	bd_load
 *	carga la base de datos del diccionario
 *	diccio base: diccio01.txt
 *
 */



/* bloque */
#if 1

int	pro_prue5()
{
	int	flag;
	int	ql;

	int	i,j,k;
	int	f1,f2,f3;
	int	x1,x2;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;


	char	z[MAXV];
	sprintf (z,"prue5");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!1 || !2 )
		gp_uso(109);

#if 1



	/* cantidad de palabras en el diccionario */
	q_wrd = 0;

	/* ptr al contenedor de direccion del ultimo nodo de la cadena */
	npa = (pnodeptr *) &np1;

	ql=0;
	while (fgets(b1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(b1) )
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

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de palabras en diccionario  : %6d\n",q_wrd);
	}


	if (gp_fverbose("d3"))
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

		printf ("\n");
	}

#endif

		
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
 *	pro_prue 6
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	exṕerimentos para ver si puedo determinar en lineas comentadas
 *	que es un comentario humano, o que es una linea de codigo comentada
 *
 */

int	q_wrd;
char	cwd[10000][32];

int	q_cid;
char	cid[20000][MAXR];

#define	MAX_L	76

/* bloque */
#if 1

int	pro_prue6()
{
	int	flag;
	int	ql;

	int	i,j,k;
	int	c1,c2,c3;
	int	f1,f2,f3,f4;
	int	f_sig;
	int	x1,x2;
	char	c;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;


	char	z[MAXV];
	sprintf (z,"prue6");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffin2 || !ffou2 || !fflog  )
		gp_uso(110);



	/*
	 * cargo todos los comentarios que extraje
	 *
	 */

	ql=0;
	while (fgets(b1,MAXB,hfinp) != NULL)
	{
		if (strlen(b1) > MAXB)
			error(2001);

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

		strcpy(cpt[ql],b1);
		ql++;
	}

	/*
	 * cargo todos los token que extraje de comentarios 
	 *
	 */

	q_cid = 0;
	while (fgets(b1,MAXB,hfin2) != NULL)
	{
		if (strlen(b1) > MAX_L)
		{	printf ("%3d |%s|\n",strlen(b1),b1);
			error(2002);
		}

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

		strcpy(cid[q_cid],b1);
		q_cid++;
	}


	/*
	 * cargo todas las palabras que suenan en comentarios de personas
	 *
	 */

	q_wrd = 0;
#if 1

	if ( 1 && ((hwi = fopen ("diccio_dep.txt","r")) == NULL) )
	{
		error(3001);
	}

	q_wrd = 0;
	while (fgets(b1,MAXB,hwi) != NULL)
	{
		if (strlen(b1) > 32)
		{	printf ("%3d |%s|\n",strlen(b1),b1);
			error(3002);
		}

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

		strcpy(cwd[q_wrd],b1);
		q_wrd++;
	}

	fclose(hwi);

#endif

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de palabras en diccionario  : %6d\n",ql);
		printf ("Cantidad de terminos en index        : %6d\n",q_cid);
		printf ("Cantidad de palabras en dic dep      : %6d\n",q_wrd);
	}


	/*
	 * ciclos
	 * - marcar en index los terminos que parece que determinan una linea de codigo
	 * - utilizar esos terminos en la busqueda de comentarios
	 * - grabar en log para ver resultados
	 */

	/* cosas que parezcan variables, probablemente sean lineas comentadas */

	/*
	 * x1 
	 *
	 */

	for (i=0; i< q_cid; i++)
	{
		strcpy(b1,cid[i]+2);

		f2 = 1;

		/* el primer caracter tiene que ser una letra */
		if (f2)
		{
			c = b1[0];
			if ( !( (c >= 'A' && c <= 'Z' ) || (c >= 'a' && c<= 'z') )  )
				f2 = 0;
		}

		/* tiene que haber solo combinacion de letras / numeros / _ */
		if (f2)
		{
			for (j=0, f1=1, f2=1; f1 && j< strlen(b1); j++)
			{
				c = b1[j];
	
				if ( !((c >= 'A' && c <= 'Z' ) || (c >= 'a' && c<= 'z') || ( c == '_') || ( c >= '0' && c <= '9'))  )
					f1 = 0, f2 = 0;
			}
		}

		/* el caracter _ tiene que estar al menos 1 vez */
		if (f2)
		{
			for (j=0, c1 = 0, f1=1, f2=1; f1 && j< strlen(b1); j++)
			{
				c = b1[j];
	
				if ( c == '_') 
					c1++;
			}

			if ( c1 == 0 )
				f2 = 0;

		}

		/* las cadenas tipo cccccccccc no van */
		if (f2)
		{
			for (j=0, c1 = 0, c2 = 0, f1=1, f2=1; f1 && j< strlen(b1); j++)
			{
				c = b1[j];
	
				if ( c == 'c') 
					c1++;
				if ( c == 'C') 
					c2++;
			}

			if ( c1 > 8 )
				f2 = 0;
			if ( c2 > 8 )
				f2 = 0;
		}



		/* si paso todas las condiciones, entonces puede ser un termino de interes */
		if (f2)
		{
			cid[i][0] = '1';
		}
	}


	/*
	 * testeo que comentarios surgen al chequear terminos de interes
	 *
	 */

	for (i=0; i < q_cid; i++)
	{	
		f3 = 0;
		if (cid[i][0] == '1')
			f3 = 1;

		if ( f3 )
		{
			strcpy(b1,cid[i]+2);

			if (gp_fverbose("d4"))
			{
				printf ("Probando termino : |%s| \n",b1);
			}

			for (j=0; j < ql; j++)
			{
				strcpy(b2,cpt[j]+2);


				for (k=0, f_sig = 1, f4 = 0; f_sig && k < strlen(b2); k++)
				{
					if (!strcmp(b1,b2+k))
						f_sig = 0, f4 = 1;
				}


				/* 
				 * En este punto ...
				 * Tengo:
				 * - indentificado un termino (con todas las condiciones para que sea un termino considerado valido)
				 * - encuentro 1 comentario que clasifica como "linea de codigo comentada" x el termino
				 * 
				 * Pero ... ahora debo aplicar todas las condiciones que anulan la clasificacion valida,
				 *          pero ya sin aplicar mas conficiones al termino ....
				 */

				if (f4)
				{
					/* marco este comentario como 'clasificado linea de codigo' */
					cpt[j][0] = '1';
					fprintf (hflog,"%-48.48s | %s\n",b1,b2);

#if 0
					fprintf (hflog,"|%s|\n",b1);
					fprintf (hflog,"|%s|\n",b2);
					fprintf (hflog,"|%s\n"," ");
#endif
				}
			}
		}
	}



	/*
	 * vuelvo a regrabar diccio de comentarios e index de terminos utiles
	 *
	 */

	for (i=0; i< ql; i++)
	{
		fprintf (hfout,"%s\n",cpt[i]);
	}
		
	for (i=0; i< q_cid; i++)
	{
		fprintf (hfou2,"%s\n",cid[i]);
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
 *	pro_prue 7
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *
 *	prue7
 *
 *	probamos algunas cosas para las redes neuronales
 *
 */


/* bloque */
#if 1


int	fim1(m1)
int	m1[100][100];
{
	int	i,j,k;
	
	for (i=0; i<100; i++)
		for (j=0; j<100; j++)
			m1[i][j] = i * 10000 + j;
}

int	fim2(m0)
void	*m0;
{
	int	i,j,k;
	int	*m1;

	m1 = (int *)m0;
	
	
	for (i=0; i<100; i++)
		for (j=0; j<100; j++)
			m1[i*100+j] = i * 10000 + j;
}


int	m2[100][100];


int	pro_prue7()
{
	int	i,j,k;
	int	f1,f2,f3;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;



	char	z[MAXV];
	sprintf (z,"prue7");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

#if 0
	if (!1 || !2 )
		gp_uso(11);
#endif


	/* bloque */

	fim1(m2);

	for (i=0; i<100; i++)
		for (j=0; j<100; j++)
			printf ("%2d %2d %5d %5d\n", i,j,i * 10000 + j, m2[i][j]);
		
	/* proceso */
	fim2(m2);

	for (i=0; i<100; i++)
		for (j=0; j<100; j++)
			printf ("%2d %2d %5d %5d\n", i,j,i * 10000 + j, m2[i][j]);
		
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
 *	pro_prue 8 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	prue 8 
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido de fuentes en memoria para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 *
 *	para buscar variables fortran que tienen blancos intermedios
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_prue8()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"prue8");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffdat )
		gp_uso(104);



	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
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
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

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
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
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

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

#if 0
	pr8_p1();
#endif



	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(609);

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







/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prue 9 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	prue 9 
 *
 *	abre achivo de pruebas (salida de pro_exec6 ... )
 *	con lineas de codigo
 *	prueba la rutina de chatgpt para identificar variables
 */


int	pro_prue9()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	int	n1;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];
	char	*v_var[100];
	int	l_var[100];

	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"prue9");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout  )
		gp_uso(104);


	/* cantidad de archivos y lineas totales cargadas  */
	ql = 0;

	while (fgets(b1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j >= 0; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;
					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			/* verifico cuantas variables tengo */
    			n1 = 0;
    			rutina_w(b1, &n1, l_var,v_var);

			printf ("L: (%3d) #: %d |%s| \n",ql,n1,b1);


#if 1
    			for (int i = 0; i < n1; i++) 
			{

				strncpy(d1,v_var[i],l_var[i]);
				d1[l_var[i]] = 0;

				printf("VVV: (%2d) %3d |%s|\n", i, l_var[i], d1 );
#if 0
				printf("VVV: (%2d) |%s|\n", i, d1);
#endif
    			}

#endif

			ql++;
		}
	}



	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



int	rutina_w(s,count,v_len,v_pos)
char	*s;
int	*count;
int	*v_len;
char	**v_pos;
{

	*count = 2;

	v_len[0]= 4;
	v_pos[0]= s+10;
	v_len[1]= 4;
	v_pos[1]= s+20;
}















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
	sprintf (z,"proc1");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!!ffsrc )
		gp_uso(111);

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
		gp_uso(112);


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
 *
 *	pro_proc 4 
 *
 *	carga un conjunto de fuentes sobre nodos estruct encadenados
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	proc 4
 *
 *	carga todos los fuentes en archivo lista
 *	
 *
 */ 


int	pro_proc4()
{
	int	i,j,k;
	int	flag;
	int	ql,qlf;
	int	q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	FILE	*hwi,*hwo;


	char	z[MAXV];
	sprintf (z,"proc4");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

/* proc4 */
#if 1

	if (!ffinp || !ffout || !ffdat )
		gp_uso(113);


	/* cantidad de lineas en el archivo  */
	qf_ff = 0;
	q_ptr = 0;
	ql=0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
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

			qfc_load(hwi,ql,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n",qlf,d1);

			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(905);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).ql = qlf;
			(*tb[qf_ff]).pf = 0;
			(*tb[qf_ff]).uf = qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d %5d |%s|\n\n",
					(*tb[qf_ff]).ql,(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
			ql++;
		}
	}


	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de files/lineas cargados:  %5d %5d \n",ql,q_ptr);
		printf ("\n");
	}

	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< qf_ff; i++)
		{
			printf ("i: %5d  %5d |%s| \n",
				i,(*tb[i]).ql,(*tb[i]).n );
		}
	}

	printf ("\n");


	/* proceso todos los files */
#if 1
	ps_src1();
#endif


	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));


		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(604);

#if 1
		qfc_write(hwo,i);
#endif
	}

	fclose(hwo);
		


#endif
/* proc4 */


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}






/*
 * -----------------------------------------------------------------------------------
 *
 *	carga un archivo a vector de estructuras
 *
 * -----------------------------------------------------------------------------------
 */



/* qfv_load */
#if 1

int	qfv_load(pfr,q1,l1)
FILE	*pfr;
fnptr	*q1;
int	*l1;
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	char	z[MAXV];
	sprintf (z,"qfv_load");

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

			if (b1[strlen(b1)-1] == '\r' )
				b1[strlen(b1)-1] = 0;

			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j >= 0; j--)
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
		printf ("\n\n");
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}

#endif
/* qfv_load */






/*
 * -----------------------------------------------------------------------------------
 *
 *	carga un archivo a cadena de estructuras
 *
 * -----------------------------------------------------------------------------------
 */



/* qfc_load */
#if 1

int	qfc_load(pfr,q1,l1)
FILE	*pfr;
int	q1;
int	*l1;
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	char	z[MAXV];
	sprintf (z,"qfc_load");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* cantidad de lineas en el archivo  */
	ql=0;





	ldfq1 = (ldf_ptr *) &vldf[q1];



	while (fgets(b1,MAXB,pfr) != NULL)
	{

#if 0
		if (!linea_vacia(b1)  && b1[0] != '#' )
#endif
		if ( 1 )
		{

			if (b1[strlen(b1)-1] == '\r' )
				b1[strlen(b1)-1] = 0;

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

			*ldfq1 = (ldf_ptr  ) malloc (sizeof (ldf_node));
			if ( *ldfq1 == NULL)
				error(909);

			sprintf ( (**ldfq1).l,"%s",b1);
			(**ldfq1).f1 = 0;
			(**ldfq1).f2 = 0;
			(**ldfq1).f3 = ql;
			(**ldfq1).nx = (ldf_ptr) NULL;
			ldfq1 = (ldf_ptr *) & (*ldfq1)->nx;

			ql++;
		}
	}

	*l1 = ql;

	if (gp_fverbose("d2"))
	{
		printf ("Cantidad de lineas en source ql       : %6d\n",ql);
	}

	if (gp_fverbose("d4"))
	{

		i=0;
		ldfq1 = (ldf_ptr *) &vldf[q1];

		while ( (*ldfq1) != (ldf_ptr) NULL )
		{
			printf ("i: %3d  (**ldfq1).f3: %3d  (**ldfq1).l: |%s|\n",i,(**ldfq1).f3,(**ldfq1).l);
			ldfq1 = (ldf_ptr *) & (*ldfq1)->nx;
			i++;
		}


	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}

#endif
/* qfc_load */





/*
 * -----------------------------------------------------------------------------------
 *
 *	graba un fuente en cadena de structs a file 
 *
 * -----------------------------------------------------------------------------------
 */



/* qfc_write */
#if 1

int	qfc_write(pfr,q1)
FILE	*pfr;
int	q1;
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	char	z[MAXV];
	sprintf (z,"qfc_write");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	i=0;
	ldfq1 = (ldf_ptr *) &vldf[q1];

	while ( (*ldfq1) != (ldf_ptr) NULL )
	{
		fprintf (pfr,"%s\n", (**ldfq1).l );

		if (gp_fverbose("d4"))
		{
			if (i == 0)
				printf ("Grabo fuente nro: %5d\n",q1);


			printf ("GRA: i: %3d  (**ldfq1).f3: %3d  (**ldfq1).l: |%s|\n",i,(**ldfq1).f3,(**ldfq1).l);
		}

		ldfq1 = (ldf_ptr *) & (*ldfq1)->nx;
		i++;
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}

#endif
/* qfc_write */






/*
 * -----------------------------------------------------------------------------------
 *
 *	agrega una estructura al vector de estructuras (qfv_load)
 *
 * -----------------------------------------------------------------------------------
 */



int	qf_addline(q1,nl)
fnptr	*q1;
int	nl;
{
	q1[nl] = (fnptr  ) malloc (sizeof (node));
	if ( q1[nl] == NULL)
		error(911);

	(*q1[nl]).l[0] = 0;
	(*q1[nl]).f1 = 0;
	(*q1[nl]).f2 = 0;
	(*q1[nl]).f3 = 0;
}












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
		gp_uso(114);


	/* cargo lista l1 */
	fnq1 = &fnp[0];
	qfv_load(hfinp,fnq1,&qf_lin);

	if (gp_fverbose("d3") )
	{
		for (i=0; i< qf_lin; i++)
			printf ("%3d |%s|\n", i, fnp[i]->l );
	}

	/* cargo lista l2 */
	fnq2 = &fnf[0];
	qfv_load(hfin2,fnq2,&qf_fen);

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

			if (gp_fverbose("d4"))
			{
				printf ("cmp: |%s| |%s| \n",b1,b2);
			}


			if (!strcmp ( b1 , b2 ) )
			{
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
		gp_uso(115);

	/* cargo mk a memo */
	load_makefile(hfinp);

	/* proceso mk */
	proc_makefile();

	/* elimino dups */
	filter_makefile();

	/* ordeno */
	ordenar_makefile();

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
 *	make_cmp
 *	compara dos elementos del vector de estrcturas para el qsort 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * lo que esta en el sprintf anda ??????
 */


int	make_cmp(fnp1,fnp2)
fnptr	*fnp1;
fnptr	*fnp2;
{
	static	char	b1[MAXB];
	static	char	b2[MAXB];
	int	k;

#if 0
	sprintf (b1,"%2d%-40-40s", (*fnp1)->f1, (*fnp1)->l );
	sprintf (b2,"%2d%-40-40s", (*fnp2)->f1, (*fnp2)->l );
#endif
	sprintf (b1,"%2d%-40.40s", (*fnp1)->f1, (*fnp1)->l );
	sprintf (b2,"%2d%-40.40s", (*fnp2)->f1, (*fnp2)->l );



	k = strcmp(b1,b2);

	if (gp_fverbose("d4"))
	{
		printf ("SSS make_cmp %s %s %d !! \n",b1,b2,k);
	}

	return k;
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	ordenar_makefile
 *	ordena alfabeticamente los nombres que encontro en el makefile
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	le estamos pasando a qsort
 *	vector de punteros a estructuras
 *	cantidad de elementos en el vector (que hay que ordenar)
 *	tamano de cada elemento (es un puntero a estructura)
 *	la funcion de comparacion
 *
 */


int	ordenar_makefile()
{
	qsort ( &fnf[0],qf_fen,sizeof( fnptr) ,make_cmp );
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
	char	b1[HUG2];
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

	while (fgets(b1,HUG2,fpr) != NULL)
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
				error(908);

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
	char	b1[HUG2];
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
								error(906);

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

	if (gp_fverbose("d4"))
	{
		for (i=0; i<qf_fen; i++)
		{
			printf ("RRR: %4d %2d |%-40.40s| \n",i, (*fnf[i]).f1, (*fnf[i]).l );
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
	qfv_load(hfinp,fnq1,&qf_lin);


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
 *	- cambia el formato de declaracion de variables (migro a tool6)
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

	/* que la vida sea mas leve */
	if (!ffaux)
		agregar_ffaux("parser.err");

	/* chequeamos reqs */
	if (!ffinp || !ffout || !ffaux )
		gp_uso(116);

	/* cargamos file en memo */
	fnq1 = &fnp[0];
	qfv_load(hfinp,fnq1,&ql_ini);


	/* mientras que no cambie la cant de lineas !!! */
	ql_fin=ql_ini;

	
#if 0
	/* 1 - cambio los formatos de declaracion de variables */
	cfor_vars(&ql_ini,&ql_fin);
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
 *	check_file
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * Suponiendo que estamos en f77 ... 
 * check file verifica si hay alguna linea "valida" (obviando comentarios)
 * que sea mas larga que 72 , porque esa va a dar error al compilar !
 *
 */



int	check_file(ql_f)
int	*ql_f;
{

	int	i,j,k;
	int	l1,l2;
	int	p1,p2;
	int	f1,f2,f3,f4;
	int	qf;

	char	b1[MSTR];
	char	b2[MSTR];


	qf = *ql_f;

	/* f1 = 1 ... check_file ok !
	 *      0 ... fallo !
	 */

	f1 = 1;

printf ("Entro a CHECK qf: %5d  \n",qf);

	for (i=0; i< qf; i++)
	{
printf ("CHECK proceso linea : %5d \n",lne(i));
		memset(b1,0,MSTR);
		strcpy(b1, (*fnp[i]).l );

		f2 = 1;
		
		/*  lineas comentadas no cuentan */
		if (f2)
		{
			if (b1[0]=='C' || b1[0]=='c' || b1[0]=='!')
				f2 = 0;
		}


		/* otros tipos de lineas comentadas no cuentan */
		if (f2)
		{
			j=0;
			f3 = 1;
	
			while ( f3 && j<strlen(b1) )
			{
				if (b1[j]=='!')
					f3 = 0, f2 = 0;
				if (b1[j]!=' ' && b1[j]!='!')
					f3 = 0;
				if (b1[j]==' ')
					j++;
			}
		}


			
		/* largo de la linea ... */
		if (f2)
		{
			if (strlen(b1) < 73)
				f2 = 0;
		}

		if (f2)
		{
			j = 7;

			l1 = strlen(b1);
			f3 = 1;

			while ( f3 && j < strlen(b1) )
			{
				if (b1[j]=='!')
					f3 = 0, l1 = j-1;
				if (b1[j]!='!')
					j++;
			}

			/* reviso si quedan blancos al final */
			for (j=strlen(b1) - 1, f3=1; f3 && j > 0; j--)
				if (b1[j] == ' ')
					b1[j]= 0, l1--;
				else
					f3=0;



			if (l1 < 73)
				f2 = 0;
		}



		/* si la linea no cumplio ninguno de los criterios de linea valida,
		 * entonces check_file falla !
		 */
		if (f2)
		{
			printf ("CHECK_FILE Linea: %6d (%3d) |%s|\n",lne(i),l1,b1);
			f1 = 0;
		}
			

#if 0
		l_pars(i,&q_tk);


		/* armo la linea de nuevo con todos los tokens
		 * solucion por ahora ... dejo los comentarios al final, afuera
		 */
		memset(b2,0,MSTR);
		for (j=0, f5=1; j< q_tk; j++)
			strcat (b2,tk[j]);
#endif
	}

	return (f1);
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_alo
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	cambia sentencias con allocate                            
 *      agrega check_alloc
 *	Si hay que desdoblar lineas, ql_f vuelve con nueva ultima linea
 *
 */



int	cfor_alo(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k,k1,l;
	int	f1,f2,f3,f4,f5;
	int	pf,uf,qf;
	int	m1,m2;
	int	l1,l2;
	int	p1;
	int	t1;

	int	fl_n;
	int	f_proceso;

	char	exte_name[MAXV];
	int	tipo_ext;

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	
	char	d1[MAXR];
	char	d2[MAXR];
	char	d3[MAXR];




	memset (b3,'X',MAXB);

	/* compatibilidad con cfor_vars / p_src3 etc
	 *
	 * primera y ultima fila del source 
	 * poner nombre del file en table de files
	 */

	pf = 0;
	uf = *ql_i - 1;

	fl_n = 0;

#if 1
	/* registro datos del archivo */
	tb[fl_n] = (ffptr ) malloc (sizeof (ff));
	if ( tb[fl_n] == NULL )
		error(907);

	strcpy ( (*tb[fl_n]).n, extract_fname(fout));
	(*tb[fl_n]).pf = pf; 
	(*tb[fl_n]).uf = uf; 
	(*tb[fl_n]).ql = uf - pf + 1;

#endif

	strcpy(d3,e_name(fl_n) );
	tipo_ext = 0;
	if (!strncmp(d3,"for",3))
		tipo_ext = 1;
	if (!strncmp(d3,"f90",3))
		tipo_ext = 2;
	
	printf ("WWW1: |%s| \n",(*tb[fl_n]).n );
	printf ("WWW2: |%s| \n",d3);


/* EEE */

	/* reviso cada linea */
	for (j = pf; j <= uf; j++)
	{
		/* la linea j */
		strcpy (b0, (*fnp[ j ]).l );
		strcpy (b1, pasar_a_minusc(b0));
		strcpy (b2, pasar_a_minusc(b0));

		/* parseo fila a tokens ... si tiene sentido  */
		l_pars(j,&q_tk);


		l2 = strlen(b1);

		/*
		 * detectar si la linea es comentarios o si esta vacia ...
		 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
		 */

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;


		/* tipo de caso a manejar */
		t1 = 0;

		/* solo proceso lineas que no son comentario */
		if (f_proceso)
		{	
			if (tiene_allocate(b1))
			{
				/* determino tipo de caso ... */


				/*
				 * caso 1
				 * Es fuente .for 
				 * el allocate esta solo y entero en la linea 
				 *
				 * caso 2
				 * Es fuente .for 
				 * el allocate esta solo y continua en linea siguiente 
				 *
				 * caso 5
				 * Es fuente .f90 
				 * el allocate esta solo y entero en la linea 
				 *
				 */

				if (tipo_ext == 1)
				{
printf ("UUU1 b1       |%s| \n",b1);
printf ("UUU2 tipo_ext |%d| \n",tipo_ext);

					if (es_alloc_t1(b1))
					{
						t1 = 1;
printf ("UUU3 t1       |%d| \n",t1);
					}
					else
					{
						t1 = 2;
					}

printf ("UUU4 t1       |%d| \n",t1);
				}

				if (tipo_ext == 2)
				{
					if (es_alloc_t1(b1))
					{
						t1 = 5;
					}

				}

				printf ("XXX1: (%d,%d) |%s| \n",tipo_ext,t1,b1);


			}


#if 0
			f5 = 1;
			while ( f5 )
			{
				strcpy(b2,b1);

				f5 = 0;
				if (p1 = es_cadena_int_src3(b1,&m1))
				{	f5 = 1;

					memset(d1,0,MAXR);
					strncpy(d1,b1+p1,m1);

		printf ("XXX1: |%s| \n",d1);
						memset (d2,0,MAXR);
						strcpy(d2,d1);
						d2[0] = '=';
						d2[m1-1] = ' ';
						sq_vinit_simple++;
		printf ("XXX2: |%s| \n",d2);


					strncpy(b1+p1,d2,m1);


					sprintf (b4,"f:%-30.30s l:%06d c:%4d|%s| \n", (*tb[fl_n]).n,lne(j),sq_vinit_simple,b2 );
					sprintf (b5,"f:%-30.30s l:%06d c:%4d|%s| \n", (*tb[fl_n]).n,lne(j),sq_vinit_simple,b1 );
					
					if (gp_fverbose("d3"))
					{
						printf ("TTTX\n");
						printf ("TTTX%s",b4);
						printf ("TTTX%s",b5);
						printf ("TTTX\n");
					}

#if 1
					if (fflog)
					{
						fprintf (hflog,"%s",b4);
						fprintf (hflog,"%s",b5);
						fprintf (hflog,"%s","\n");
					}
#endif
				}
			}
#endif
		}	

		/* grabo la linea */
		if ( 1 )
		{
			strcpy ((*fnp[ j ]).l, b1 );
		}
	}



#if 0
	/* actualizo la cantidad de lineas del source para proximos cambios */
	*ql_f = qf;
#endif

}




int	es_alloc_t1(s)
char	*s;
{
	char	b0[MAXB];

	int	f_res;
	int	f_sigo;

	int	p1,p2;
	int	i,j,k;
	int	l1,l2;

	int	f1;

	int	st1,st2,st3,st4;

	/* copio string para trabajar local */
	strcpy(b0,s);
	l2 = strlen(b0);

	/* recorto si hay comentarios al final de la linea */
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '!' && b0[i-1] != '4')
			f_sigo = 0, b0[i] = 0;
	}

	/* largo definitivo de la sentencia */
	l2 = strlen(b0);
	

	f_res = 0;
	p1    = 0;
	p2    = l2 -1;

	st1 = 0;
	st2 = 0;
	st3 = 0;

		
	/* la primer palabra tiene que ser allocate */
	st1 = 0;
	for (i=0, f_sigo=1 ; f_sigo && i<=p2; i++)
	{	if (b0[i] == ' ' || b0[i] == '\t')
			p1++;
		else
		{	
			if (!strncmp(b0+i,"allocate",8))
			{
				f_sigo = 0;
				st1 = 1;
				p1 = i+8;
			}
		}
	}
	
printf ("RRR0 paso 1 - rimer palabra es allocate          \n");
printf ("RRR1 s           |%s| \n",s);
printf ("RRR2 b1          |%s| \n",b0);
printf ("RRR3 st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);


	/* puede haber blancos entre allocate y ( ... los quito */
	while (b0[p1] == ' ')
	{
		l2 = strlen(b0)-1;
		for (k=p1; k<l2; k++)
			b0[k] = b0[k+1];
		b0[l2]=0;
		p2--;
	}


printf ("RRR0 paso 2 - si hay blancos corro la linea          \n");
printf ("RRR1 s           |%s| \n",s);
printf ("RRR2 b1          |%s| \n",b0);
printf ("RRR3 st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);

	/* el ultimo caracter salvo blancos, es un parentesis que cierra */
	st2 = 0;
	l2 = strlen(b0);
	for (i=l2-1, f_sigo=1; f_sigo && i; i-- )
		if (b0[i] == ' ')
			p2--;
		else
		{
			if (b0[i] == ')')
			{
				f_sigo = 0;
				st2 = 1;
				p2  = i;
			}
			else
			{
				f_sigo = 0;
			}
		}

printf ("RRR0 paso 3 - el ultimo caracter salvo blanco es )           \n");
printf ("RRR1 s           |%s| \n",s);
printf ("RRR2 b1          |%s| \n",b0);
printf ("RRR3 st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
printf ("RRR4 p1 p2       |%2d,%2d|  \n",p1,p2);
printf ("RRR5 b0[p1]      |%c| \n",b0[p1]);
printf ("RRR6 b0[p2]      |%c| \n",b0[p2]);


	/* caracteres especificos de allocate entre ( y ) */
	st3 = 1;
	for (i=p1; i<=p2; i++)
	{
		if (!((b0[i] >= 'a' && b0[i] <= 'z') || 
                      (b0[i] >= '0' && b0[i] <= '9') || 
                      b0[i] == ' ' || b0[i] == ':' || b0[i] == ',' || 
                      b0[i] == '-' || b0[i] == '(' || b0[i] == ')' || 
	              b0[i] == '_' || b0[i] == '*' || b0[i] == '+'    )  )
		{
			st3 = 0;
		}
	}

	
printf ("RRR0 paso 4 - caracteres especificos entre ( y )          \n");
printf ("RRR1 s           |%s| \n",s);
printf ("RRR2 b1          |%s| \n",b0);
printf ("RRR3 st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
printf ("RRR4 p1 p2       |%2d,%2d|  \n",p1,p2);
printf ("RRR5 b0[p1]      |%c| \n",b0[p1]);
printf ("RRR6 b0[p2]      |%c| \n",b0[p2]);



#if 0
	/* tiene que abrir un parentesis */
	st3 = 0;
	if (b0[p1] == '(' )
		st2 = 1;
#endif


	if (st1 && st2 && st3)
		f_res = 1;
			
printf ("RRR0 paso 5           \n");
printf ("RRR7 f_res      |%d| \n",f_res);


	return (f_res);

}


/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_ini
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	cambia el tipo de inicializacion de var /xxx/ a var = xxx
 *	Si hay que desdoblar lineas, ql_f vuelve con nueva ultima linea
 *
 */



int	cfor_ini(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k,k1,l;
	int	f1,f2,f3,f4,f5;
	int	pf,uf,qf;
	int	m1,m2;
	int	l1;
	int	p1;

	int	fl_n;

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	
	char	d1[MAXR];
	char	d2[MAXR];




	memset (b3,'X',MAXB);

	/* compatibilidad con cfor_vars / p_src3 etc
	 *
	 * primera y ultima fila del source 
	 * poner nombre del file en table de files
	 */

	pf = 0;
	uf = *ql_i - 1;

	fl_n = 0;

#if 1
	/* registro datos del archivo */
	tb[fl_n] = (ffptr ) malloc (sizeof (ff));
	if ( tb[fl_n] == NULL )
		error(907);

	strcpy ( (*tb[fl_n]).n, extract_fname(fout));
	(*tb[fl_n]).pf = pf; 
	(*tb[fl_n]).uf = uf; 
	(*tb[fl_n]).ql = uf - pf + 1;

#endif



	/* reviso cada linea */
	for (j = pf; j <= uf; j++)
	{
		/* la linea j */
		strcpy (b1, (*fnp[ j ]).l );
		strcpy (b0, (*fnp[ j ]).l );

		/* parseo fila a tokens ... si tiene sentido  */
		l_pars(j,&q_tk);


		/*
		 * detectar si la linea es comentarios ...
		 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
		 */

		f4 = 1;
		if (es_linea_comentario(b1))
			f4 = 0;

		/* solo proceso lineas que no son comentario */
		if (f4)
		{	
			f5 = 1;
			while ( f5 )
			{
				strcpy(b2,b1);

				f5 = 0;
				if (p1 = es_cadena_int_src3(b1,&m1))
				{	f5 = 1;

					memset(d1,0,MAXR);
					strncpy(d1,b1+p1,m1);

		printf ("XXX1: |%s| \n",d1);
						memset (d2,0,MAXR);
						strcpy(d2,d1);
						d2[0] = '=';
						d2[m1-1] = ' ';
						sq_vinit_simple++;
		printf ("XXX2: |%s| \n",d2);


					strncpy(b1+p1,d2,m1);


					sprintf (b4,"f:%-30.30s l:%06d c:%4d|%s| \n", (*tb[fl_n]).n,lne(j),sq_vinit_simple,b2 );
					sprintf (b5,"f:%-30.30s l:%06d c:%4d|%s| \n", (*tb[fl_n]).n,lne(j),sq_vinit_simple,b1 );
					
					if (gp_fverbose("d3"))
					{
						printf ("TTTX\n");
						printf ("TTTX%s",b4);
						printf ("TTTX%s",b5);
						printf ("TTTX\n");
					}

#if 1
					if (fflog)
					{
						fprintf (hflog,"%s",b4);
						fprintf (hflog,"%s",b5);
						fprintf (hflog,"%s","\n");
					}
#endif
				}
			}
		}	

		/* grabo la linea */
		if ( 1 )
		{
			strcpy ((*fnp[ j ]).l, b1 );
		}
	}



#if 0
	/* actualizo la cantidad de lineas del source para proximos cambios */
	*ql_f = qf;
#endif

}





/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_vcb
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	lee una lista de variables con blancos intermedios         
 *	busca en el fuente si encuentra algna de esa variables
 *	si encuentra, las cmbia
 *	ql_f vuelve con nueva ultima linea
 *
 */



int	cfor_vcb(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k,k1,k2,l;
	int	f1,f2,f3,f4,f5;
	int	flag;
	int	pf,uf,qf;
	int	m1,m2;
	int	p1;
	int	l1,l2;
	int	c1;

	int	fl_n;
	int	f_sig;

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	
	char	d1[MAXR];
	char	d2[MAXR];




	memset (b3,'X',MAXB);

	/* compatibilidad con cfor_vars / p_src3 etc
	 *
	 * primera y ultima fila del source 
	 * poner nombre del file en table de files
	 */

	pf = 0;
	uf = *ql_i - 1;

	fl_n = 0;

#if 1
	/* registro datos del archivo */
	tb[fl_n] = (ffptr ) malloc (sizeof (ff));
	if ( tb[fl_n] == NULL )
		error(907);

	strcpy ( (*tb[fl_n]).n, extract_fname(fout));
	(*tb[fl_n]).pf = pf; 
	(*tb[fl_n]).uf = uf; 
	(*tb[fl_n]).ql = uf - pf + 1;

#endif


	/* cargo archivo con lista de variables vcb */
	if (!ffin2)
		gp_uso(1001);
 	
	q_vcb = 0;

	while (fgets(b1,MAXV,hfin2) != NULL)
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

			/* saco el ultimo | */
			if (b1[strlen(b1)-1] == '|')
				b1[strlen(b1)-1] = 0;
			else
				error(5003);

			/* copio desde el primer caracter despues del primer | */
			strcpy(vcb[q_vcb],b1+1);

printf ("XXX cargue vcb |%s| \n",vcb[q_vcb]);


			q_vcb++;
		}
	}

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de lineas cargadas : %6d\n",q_vcb);
	}




	/* reviso cada linea */
	for (j = pf; j <= uf; j++)
	{
		/* la linea j */
		strcpy (b0, (*fnp[ j ]).l );
		strcpy (b1, (*fnp[ j ]).l );
		strcpy (b2, pasar_a_minusc(b1));
		l2 = strlen(b2);

		/* parseo fila a tokens ... si tiene sentido  */
		l_pars(j,&q_tk);


		/*
		 * detectar si la linea es comentarios ...
		 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
		 */

		c1 = 0;

		f4 = 1;
		if (es_linea_comentario(b2) || linea_vacia_for(b2) )
			f4 = 0;

		/* solo proceso lineas que tienen codigo */
		if (f4)
		{	
			f5 = 1;
			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}
	
							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("ZZ1 b3 var (%5d) |%s|\n",k,b3);
							printf ("ZZ2 b2 lin (%5d) |%s|\n",lne(j),b2);
							printf ("ZZ3 cnt (%5d)\n",c1);
							printf ("ZZ4 b0             |%s|\n",b0);
							printf ("ZZ5 b1             |%s|\n",b1);
							printf ("ZZ6 b2             |%s|\n",b2);
							printf ("ZZ7 \n");

							if (fflog)
							{
								fprintf (hflog,"%s\n",b0);
								fprintf (hflog,"%s\n",b1);
								fprintf (hflog,"%s","\n");
							}
						}
					}
				}		
	
			} /* while f5 */
		}	

		/* grabo la linea */
		if ( 1 )
		{
			strcpy ((*fnp[ j ]).l, b1 );
		}

	}


#if 0
	/* actualizo la cantidad de lineas del source para proximos cambios */
	*ql_f = qf;
#endif

}










/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_mas
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	f1
 *	1 - integer
 *	2 - logical
 * 	3 - real
 *	4 - character
 *
 *	Si hay que desdoblar lineas, ql_f vuelve con nueva ultima linea
 *
 */


#if 1

int	cfor_mas(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,i1,j,k,k1;
	int	p1,p2;
	int	f1,f2,f3,f4,f5,f6,f7;
	int	qi,qf;
	int	d1;
	int	delta1,delta2;

	char	b1[MSTR];
	char	b2[MSTR];
	char	b3[MSTR];
	char	b4[MSTR];
	char	b5[MSTR];
	char	b6[MSTR];

	char	*tok;


	d1 = 0;		/* lineas agregadas ! */
	qi = *ql_i;
	qf = *ql_f;
	f4 = 0;		/* modifico linea con kind o len */





	for (i=0; i< qf; i++)
	{
		f4 = 0;

		memset(b1,0,MSTR);
		strcpy(b1, (*fnp[i]).l );

		l_pars(i,&q_tk);

		if (gp_fverbose("d4"))
		{
			printf ("cfor_mas:  Vengo de l_pars\n");
			printf ("cfor_mas:  Linea %4d #tk %4d |%s|\n",i,q_tk,b1);
			for (j=0; j<q_tk; j++)
			{
				printf ("cfor_mas: TK: %3d %3d f1: %d |%s|\n",j,strlen(tk[j]),f1,tk[j]);
			}
		}

		if ( f1=tiene_dec_var1() )
		{
			f4 = 1;

			/* caso int, log, real */
			if (f1 )
			{
				if (gp_fverbose("d3"))
				{	printf ("cfor_mas: fixed !  f1: %d \n",f1);
					printf ("cfor_mas: Linea %4d q_tk %4d lin |%s|\n",i,q_tk,b1);
				}
			}
				
		} /* tiene_dec_var */



		/* armo la linea de nuevo con todos los tokens
		 * solucion por ahora ... dejo los comentarios al final, afuera
		 */
		memset(b2,0,MSTR);
		for (j=0, f5=1; j< q_tk; j++)
			strcat (b2,tk[j]);

		/* la linea no tenia dec de variables */
		if ( f4 == 0)
		{
			strcpy ( (*fnp[i]).l, b2);
		}

		/* la linea tenia dec de variables */
		if (f4 == 1)
		{
			if ( def_var_continua(b2))
			{
				if (gp_fverbose("d3"))
				{
					printf ("DEF_var_cont desp del if ... b2: |%s| \n",b2);
				}

				memset (b3,0,MSTR);

				delta1 = 0;
				do
				{
					strcat (b3, limpiar_mas ( (*fnp[i+delta1]).l )  );
					delta1++;
		
					sq_lineas_con_mas_elim++;

					do
					{
						f7=0;
	
						/* 
						 * ojo !! para una misma linea no puede ser true mas de una condicion !! 
						 * si me encuentro una linea que es coment ... skip 
						 */
	
						if ( (*fnp[i+delta1]).l[0] == '!')
							f7=1;
	
						/* 
						 * si me encuentro una linea en blanco  ... skip 
						 */

						if ( (*fnp[i+delta1]).l[0] != '!' && strlen( (*fnp[i+delta1]).l ) < 4 )
							f7=1;
	
						if (f7)
							delta1++;

					} while (f7);

				}
				while ( def_var_continua (  (*fnp[i+delta1]).l) ) ;

				if (gp_fverbose("d3"))
				{
					printf ("limpiar_mas (desp del while) : |%s| \n", limpiar_mas ( (*fnp[i+delta1]).l) );
				}

				strcat (b3, limpiar_mas ( (*fnp[i+delta1]).l) );


				if (gp_fverbose("d3"))
				{
					printf ("\n\n");
					printf ("Concateno1: delta1: %2d len: %2d b3: |%s| \n\n\n",delta1,strlen(b3),b3); 
				}

				/* 
				 * mala eleccion strtok ... hay que canbiar , x & en los lugares que no hay que cortar !!
				 * si hay coma entre esta estructura, reemplazar .. (:,:)
				 */
				for (i1=0; i1<strlen(b3); i1++)
					if (!strncmp(b3+i1,":,:",3) )
						b3[i1+1] = '&';

				for (i1=0,f6=0; i1<strlen(b3); i1++)
				{
					if (!f6 && b3[i1] == '(')
						f6 = 1;
					if ( f6 && b3[i1] == ',')
						b3[i1] = '&';
					if ( f6 && b3[i1] == ')')
						f6 = 0;
				}



				if (gp_fverbose("d3"))
				{
					printf ("\n\n");
					printf ("Concateno2: delta1: %2d len: %2d b3: |%s| \n\n\n",delta1,strlen(b3),b3); 
				}
				
				memset (b4,0,MSTR);
				memset (b5,0,MSTR);

				preparame_dos_lineas(b3,b4,b5);
				strcpy(b6,b5);

				if (gp_fverbose("d3"))
				{	printf ("Vengo de preparame_dos_lineas \n");
					printf ("l1 |%s| \n",b4);
					printf ("l2 |%s| \n",b5);
					printf ("l3 |%s| \n",b6);
				}

				delta2 = 0;
				tok = strtok(b5,",");


				if (gp_fverbose("d3"))
				{
					printf ("CFMAS 5 : 1er strtok  |%s|  \n",tok);
				}

				while ( tok != NULL )
				{
					sprintf (b3,"      %s  %s",b4,tok); 

					tok = strtok(NULL,",");

					if (gp_fverbose("d3"))
					{
						printf ("strtok: |%s| \n",b3);
					}
					delta2++;

				}
				

				if (gp_fverbose("d3"))
				{
					printf ("ZZZ delta1                   :   %4d        \n",delta1);
					printf ("ZZZ delta2                   :   %4d        \n",delta2);
					printf ("ZZZ qf qf-1  s(qf-1)         :   %4d %4d |%s|  \n", qf, qf-1, (*fnp[qf-1]).l );
					printf ("ZZZ i   s(i)                 :   %4d  |%s|  \n", i , (*fnp[i]).l );
					printf ("ZZZ Debo correr lineas desde :   %4d  |%s|  \n",
								 (qf-1)+(delta2-delta1-1) , (*fnp[(qf-1)+(delta2-delta1-1)]).l );
					printf ("ZZZ Hasta linea              :   %4d  |%s|  \n", i+delta2 , (*fnp[i+delta2]).l );
					printf ("\n\n");
				}

				/* correr todas las lineas ... */
				if (gp_fverbose("d3"))
				{
				printf ("MUEVO: ant de for,  delta1   : %2d \n", delta1);
				printf ("MUEVO: ant de for,  delta2   : %2d \n", delta2);
				printf ("MUEVO: ant de for,  i        : %2d \n", i);
				printf ("MUEVO: ant de for,  desde    : %2d \n", (qf-1)+(delta2-delta1-1) );
				printf ("MUEVO: ant de for,  hasta    : %2d \n", i+delta2);
				}


				for ( k = (qf-1)+(delta2-delta1-1); k >= i + delta2  ; k-- )
				{
 
					if ( k >= qf )
					{
						/* agrego lineas al final */ 
						fnp[k] = (fnptr  ) malloc (sizeof (node));
						if ( fnp[k] == NULL)
							error(910);
						(*fnp[k]).l[0] = 0;
						(*fnp[k]).f1 = 0;
						(*fnp[k]).f2 = 0;
						(*fnp[k]).f3 = 0;
					}

					if (gp_fverbose("d3"))
					{
						printf ("KKK2:   a k: %4d   desde: %4d  f(k): |%s|    \n\n",
							k, k-(delta2-delta1-1), (*fnp[k-(delta2-delta1-1)]).l );
					}

					memcpy ( fnp[k],fnp[k-(delta2-delta1-1)], sizeof (node) );
				}


				/* grabar ambas lineas */
				delta2 = 0;
				tok = strtok(b6,",");
				while ( tok != NULL )
				{	
					k1 = i+delta2;

					/* hay que agregar malloc ... deberia conf rango de lineas ! */
					if (fnp[k1] == NULL)
					{				
						fnp[k1] = (fnptr  ) malloc (sizeof (node));
						if ( fnp[k1] == NULL)
							error(912);
						(*fnp[k1]).l[0] = 0;
						(*fnp[k1]).f1 = 0;
						(*fnp[k1]).f2 = 0;
						(*fnp[k1]).f3 = 0;
					}

					sprintf (b3,"      %s  %s",b4,tok);

					if (gp_fverbose("d3"))
					{
						printf ("KK3: GRABO: qf     : %4d  \n", qf);
						printf ("KK3: GRABO: f(x)   : %4d  \n", i+delta2);
						printf ("KK3: GRABO: str de : |%s| \n", b3 );
						printf ("KK3: GRABO: str a  : |%s| \n", (*fnp[i+delta2]).l );
					}


					/* recuperando la chanchada */
					for (i1=0; i1<strlen(b3); i1++)
						if (b3[i1] == '&' )
							b3[i1] = ',';
						
#if 0
						if (!strncmp(b3+i1,":&:",3) )
							b3[i1+1] = ',';
#endif


					strcpy ( (*fnp[i+delta2]).l, b3);

					if (gp_fverbose("d3"))
					{
						printf ("KK4: GRABO: f(x) : %4d  \n", i+delta2);
						printf ("KK4: GRABO: str  :|%s|  \n",  (*fnp[i+delta2]).l );
					}

					tok = strtok(NULL,",");
					delta2++;
				}

			
				/* agregar una linea mas al count */
				qf = qf + delta2 - delta1 - 1;

			}
			else
			{	/* no es linea con continuacion */
				strcpy ( (*fnp[i]).l, b2);
			}
		}
	}

	/* actualizo la cantidad de lineas del source para proximos cambios */
	*ql_f = qf;
}


#endif


/*
 * recibe linea con continuacion (+) y declaracion de variables 
 * si hay comentarios al final, elimina
 *
 */

char	*limpiar_mas(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	p1;

	static char	b1[MAXB];

	for (i=0, f1=1, p1=0; f1 && i<strlen(s); i++)
		if (s[i] != ' ' && s[i] != '+')
			p1 = i, f1 = 0;

#if 0
	/* por ahora .. si hay comentarios al final, fuera */
	for (i=strlen(s)-1, f1=1; f1 && i; i--)
		if (s[i] == ',')
			s[i+1] = 0, f1 = 0;
#endif

	/* por ahora .. si hay comentarios al final, fuera */
	for (i=strlen(s)-1, f1=1; f1 && i; i--)
		if (s[i] == '!')
			s[i] = 0, f1 = 0;

	strcpy(b1,s+p1);

	return b1;

}



int	def_var_continua(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	p1,p2;

	
	f1 = 1;		/* seguimos verificando */
	f2 = 0;		/* default, no tiene char de continua */
	f3 = 0;		/* la linea no tiene comentario al final */

	if (gp_fverbose("d3"))
	{
		printf ("def_var_continua - - - - 1 |%s| \n",s);
	}

	
	p1 = strlen(s);

	/* busco el ultimoo caracter valido en la linea, descartando que haya comentario */
	for (i=0 , f1=1; i < p1; i++)
		if (s[i] == '!')
		{	f3 = 1;
			/* codigo,codigo[,][   ]! texto */
			for (j=i-1; f1 && j; j--)
				if (s[j] != ' ')
					f1=0, p1 = j;
		}

	if (!f3)
		p1--;
			
	if (gp_fverbose("d3"))
	{
		printf ("def_var_continua - - - - 2 strlen(s) : %d p1: %d f2: %d s(p1): %c  s(p1-1): %c |%s| \n",
			strlen(s),p1,f2,s[p1],s[p1-1],s);
	}

	if (s[p1] == ',')
		f1 = 0, f2 = 1;

#if 0
	for ( i=0, f3 = 0; f1 && !f3 && i< strlen(s); i++)
		if (s[i] == '!' )
			p1 = i, f1 = 0, f3 = 1;

	for ( i=p1; f1 && i; i--)
		if (s[i] == ',')
			f2 = 1, f1 = 0;
#endif


	if (gp_fverbose("d3"))
	{
		printf ("def_var_continua - - - - 3 |%s| f2 (ret): (%d) \n\n",s,f2);
	}

	return f2;
}

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
 *	4 - character
 *
 *	Si hay que desdoblar lineas, ql_f vuelve con nueva ultima linea
 *
 */


int	cfor_vars(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k,l1;
	int	p1,p2;
	int	f1,f2,f3,f4,f5;
	int	qi,qf;
	int	d1;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];


	d1 = 0;		/* lineas agregadas ! */
	qi = *ql_i;
	qf = *ql_f;
	f4 = 0;		/* modifico linea con kind o len */



	for (i=0; i< qf; i++)
	{
		f4 = 0;

		strcpy(b1, (*fnp[i]).l );

		l_pars(i,&q_tk);

		if ( f1=tiene_dec_var1() )
		{
			f4 = 1;

			/* caso int, log, real */
			if (f1 == 1 || f1 == 2 || f1 == 3)
			{
				/* si no lo pude arreglar, encontre un caso no contemplado !!*/
				if (! fix_dec_var1 () )
				{	
printf ("DDD despues de fix_dec_var1 q_tk: %d\n",q_tk);

					printf ("CASO NO CONTEMPLADO: (%5d) len: %2d |%s| \n",i,strlen(b1),b1);
				}
#if 0
					error(701);
#endif
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
				
		} /* tiene_dec_var */



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


		/* la linea no tenia dec de variables */
		if ( f4 == 0)
		{
			strcpy ( (*fnp[i]).l, b2);
		}


		/* la linea tenia dec de variables */
		if (f4 == 1)
		{
			/* en el caso de fix format ... si tiene mas de 72 chars ... problemas */
			if ( (l1=largo_linea(b2)) > LF77 )
			{
				if (gp_fverbose("d3"))
				{	
					printf ("ACA algo no anda carajo !!!! \n");
					printf ("Atencion! -- linea larga %5d %3d |%s|\n",i,l1,b2);
				}
printf (" ZZZ--- 1 \n");

				memset (b3,0,MAXB);
				memset (b4,0,MAXB);

printf (" ZZZ--- 2 \n");

				armame_dos_lineas(b2,b3,b4);
				if (gp_fverbose("d3"))
				{	printf ("dos lineas: 1  |%s| \n",b3);
					printf ("dos lineas: 2  |%s| \n",b4);
				}

printf (" ZZZ--- 3 \n");
				/* correr todas las lineas ... */
				correme_una_linea(i+1,qf);

printf (" ZZZ--- 4 \n");

#if 1
				/* grabar ambas lineas */
				strcpy ( (*fnp[i]).l, b3);
				strcpy ( (*fnp[i+1]).l, b4);
			
				/* agregar una linea mas al count */
				qf++;
#endif

#if 0
				strcpy ( (*fnp[i]).l, b2);
#endif
			}
			else
			{	/* no es linea larga, guardar una sola linea*/
				strcpy ( (*fnp[i]).l, b2);
			}
		}
	}

	/* actualizo la cantidad de lineas del source para proximos cambios */
	*ql_f = qf;
}






/*
 *	correr todas las lineas en el vector
 *	desde la l1 al final
 *	hay que agregar una al final para hacer lugar 
 */

int	correme_una_linea(l1,ql)
int	l1;
int	ql;
{

	int	i,j,k;


	/* agrego una linea al final */
	fnp[ql] = (fnptr  ) malloc (sizeof (node));
	if ( fnp[ql] == NULL)
		error(913);

	(*fnp[ql]).l[0] = 0;
	(*fnp[ql]).f1 = 0;
	(*fnp[ql]).f2 = 0;
	(*fnp[ql]).f3 = 0;


	for (j=ql; j>l1; j--)
	{
		memcpy ( fnp[j],fnp[j-1], sizeof (node) );
	}
}





#if 0

/*	NO ANDA - dejo aca para revisar en otro momento ....
 *	correr todas las lineas en el vector
 *	desde la l1 al final
 *	hay que agregar una al final para hacer lugar 
 */

int	correme_una_linea(l1,ql)
int	l1;
int	ql;
{

	int	i,j,k;



	for (j=ql; j>l1; j--)
	{

		/* agrego una linea al final */
		fnp[j] = (fnptr  ) malloc (sizeof (node));
		if ( fnp[j] == NULL)
			error(914);

		(*fnp[j]).l[0] = 0;
		(*fnp[j]).f1 = 0;
		(*fnp[j]).f2 = 0;
		(*fnp[j]).f3 = 0;

printf (" - - - - 3 l1: %4d ql: %4d j-1: %4d j: %4d  s[j-1]: |%s|  s[j]: |%s| \n", l1,ql,j-1,j, (*fnp[j-1]).l,(*fnp[j]).l );

		memcpy ( fnp[j],fnp[j-1], sizeof (node) );

printf (" - - - - 4 l1: %4d ql: %4d j-1: %4d j: %4d  s[j-1]: |%s|  s[j]: |%s| \n\n\n", l1,ql,j-1,j, (*fnp[j-1]).l,(*fnp[j]).l );

	}
}

#endif

/*
 * -----------------------------------------------------------------------------------
 *
 *	armame dos lineas
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	s  ... linea de entrada a separar 
 *	
 *	separa en dos lineas l1 y l2 
 *	algunos considerandos !
 *
 *	si es del tipo x ej
 *	integer (kind=4) :: variable(subindice1,subindice2) ....
 *	entonces, la ',' esa no sirve!! tengo que estar seguro
 *	que cerro el parentesis !!
 *
 *	idem si es x ej
 *	character (len=40000) :: variable_super_largar / cosa ,
 *	esa ',' tampoco me sirve, tiene que cerrar la '/'
 *
 *	si es una fuction ... tampoco sirve buscar el ::
 */

int	armame_dos_lineas(s,l1,l2)
char	*s;
char	*l1;
char	*l2;
{
	int	i,j,k;
	int	c1;
	int	f1,f2,f3,f4,f5;
	int	p1,p2,p3,p4,p5;
	int	f_fix;
	char	b1[MAXB];

	memset(b1,' ',MAXB);

	f_fix = 0;		/* f_fix 0, aun no se soluciono */

	/* es una FUNCTION ? */
	f5=0;
	p4=0;
	for (i=0; !f5 && i<strlen(s); i++)
		if (!strncmp(s+i,"FUNCTION",8) || !strncmp(s+i,"function",8) )
			p4=i, f5 = 1;


	/* busco el :: - como estoy convirtiendo ... la linea ya lo tiene que tener ! */
	for (i=0, f1=1; f1 && i<strlen(s); i++)
		if (s[i] == ':' && s[i+1] == ':' )
			f1 = 0, p1 = i+2, p2 = i+2;

		
	/* busco donde empieza el resto, despues de :: y blancos */
	for (i=0, c1=0, f1=1; f1 && i<strlen(s); i++)
		if (s[i+p1] == ' ')
			c1++;
		else
			f1=0;

	/* busco una coma que separe campos, que no este dentro de parentesis ! */
	for (i=0, f1=1, f2=1, f3=1, f4=0; f1 && i<strlen(s); i++)
	{	if (f2 && s[i+p1] == '(')
			f2 = 0;
		if (!f2 && s[i+p1] == ')' )
			f2 = 1;

		if (f3 && s[i+p1] == '/')
			f3 = 0, f1=1;
		if (!f3 && s[i+p1] == '/' )
			f3 = 1;

		if (f2 && f3 && s[i+p1] == ',')
			f1 = 0, p2 = p1+i;
	}


	/* si es funcion, separo en declaracion y funcion */
	if (f5)
	{
		p1 = p4 - 1;
		p2 = LF77+1;
	}


	/* 
	 * si encontro la ',' puede separar campos
	 * si no la encontro, es el caso de una sola variable con
	 * sub indices largos , y llego al fin de linea !
	 * caso en que  hay:
	 *     ... (   ,   ) , 
	 *     ... /       /,
	 */
	

	if (p1 != p2)
	{	
printf ("p1 != p2: f4: %d l1: |%s| \n",f4,l1);
printf ("p1 != p2: f4: %d l2: |%s| \n",f4,l2);

		if (p2 > LF77 )
		{

			strncpy(l1,s,p1);
			strcpy(l2,"     + ");
			strcat(l2,s+p1);
			f_fix = 1;
printf ("p2 > LF77: f4: %d l1: |%s| \n",f4,l1);
printf ("p2 > LF77: f4: %d l2: |%s| \n",f4,l2);
		}
		else
		{
			strncpy(l1,s,p2);
			l1[p2]=0;

			strncpy(l2,s,p1);
			strncat(l2,b1,c1);
			strcat(l2,s+p2+1);

			f_fix = 1;
printf ("p2 > LF77,else: f4: %d l1: |%s| \n",f4,l1);
printf ("p2 > LF77,else: f4: %d l2: |%s| \n",f4,l2);
		}

	}
	else
	{
		strncpy(l1,s,p1);
		strcpy(l2,"      + ");
		strcat(l2,s+p1);

		f_fix = 1;
printf ("p1 != p2,else: f4: %d l1: |%s| \n",f4,l1);
printf ("p1 != p2,else: f4: %d l2: |%s| \n",f4,l2);
	}


printf ("salida: f4: %d l1: |%s| \n",f4,l1);
printf ("salida: f4: %d l2: |%s| \n",f4,l2);

#if 0
	if ( p1 == p2)
	{
		printf ("SALGO de armame_dos_lineas \n");
		printf ("l1: |%s| \n",l1);
		printf ("l2: |%s| \n",l2);
	}
#endif


	sq_lineas_desdobladas++;	

}





int	preparame_dos_lineas(s,l1,l2)
char	*s;
char	*l1;
char	*l2;
{
	int	i,j,k;
	int	c1;
	int	f1,f2,f3;
	int	p1,p2,p3;


	if (gp_fverbose("d3"))
	{
		printf ("- preparame s  |%s| \n",s);
	}

	for (i=0, f1=1, p1=0; f1 && i<strlen(s); i++)
		if (s[i] == ':' && s[i+1] == ':' )
			f1 = 0, p1 = i+2;

	for (i=p1, f1=1, p2=0; f1 && i<strlen(s); i++)
		if (s[i] != ' ')
			f1 = 0, p2 = i;

	strncpy(l1,s,p1);
	l1[p1]=0;

	strcpy(l2,s+p2);

	if (gp_fverbose("d3"))
	{
		printf ("- preparame l1 |%s| \n",l1);
		printf ("- preparame l2 |%s| \n",l2);
	}

}


/*
 * -----------------------------------------------------------------------------------
 *
 *	fix_fec_var1
 *
 * -----------------------------------------------------------------------------------
 */



int	fix_dec_var1()
{
	int	i,j,k,i1,l,m,n;
	int	f1,f2,f3,f4;
	int	minus, aster, kind, opt, inten;
	int	dbl, alloca, save, func, cont;
	int	n_minus, n_aster, n_kind, n_opt, n_inten;
	int	n_dbl, n_alloca, n_save, n_func, n_cont;
	int	coma,n_coma,f_coma;
	int	ult, n_type;
	int	f_aster;
	char	nr[16];
	char	b1[MAXB];

	int	f_int,f_rea,f_log,f_cha;

	char	ca;
	char	s_varb[MAXB];
	char	s_inte[MAXB];

	char	b4[MAXB];


	memset(b1,0,MAXB);
	memset(s_varb,0,MAXB);
	memset(s_inte,0,MAXB);


	f3     = 1;	/* f3 true es GO ... si por alguna condicion paramos, f3 false */

	func     = 0;
	inten    = 0;
	save     = 0;
	kind     = 0;
	ult      = 0;
	func     = 0;
	opt      = 0;
	cont     = 0;
	alloca   = 0;
	dbl      = 0;
	aster    = 0;
	coma     = 0;

	n_alloca = 0;
	n_dbl    = 0;
	n_type   = 0;
	n_aster  = 0;
	n_coma   = 0;
	
	f_aster  = 1;
	f_coma   = 1;

	f_int    = 0;
	f_rea    = 0;
	f_log    = 0;
	f_cha    = 0;


	/*
	 * el concepto de ult
	 * es que es la ultima posicion valida del sector
	 * de declaracion de una variable 
	 *
	 */

	for (i=0; i< q_tk; i++)
	{	
		if (!f_int && !strcmp("integer",tk[i]) )
			f_int = 1, minus = 1, n_type = i;

		if (!f_int && !strcmp("INTEGER",tk[i]) )
			f_int = 1, minus = 0, n_type = i;

		if (!f_log && !strcmp("logical",tk[i]) )
			f_log = 1, minus = 1, n_type = i;

		if (!f_log && !strcmp("LOGICAL",tk[i]) )
			f_log = 1, minus = 0, n_type = i;

		if (!f_rea && !strcmp("real",tk[i]) )
			f_rea = 1, minus = 1, n_type = i;

		if (!f_rea && !strcmp("REAL",tk[i]) )
			f_rea = 1, minus = 0, n_type = i;

		if (!dbl && !strcmp(":",tk[i]) && !strcmp(":",tk[i+1]) )
			dbl=1,n_dbl = i;

		if (!f_coma && tk[i][0] == ',')
			f_coma = 1, coma = 1, n_coma = i;

		if (!kind && ( !strcmp("kind",tk[i]) || !strcmp("KIND",tk[i+1]) ))
		{	kind=1,n_kind = i;
			for (k=i+1; k< i+8; k++)
				if (tk[k][0] == ')')
					ult=k;

		}

		if (!strcmp("*",tk[i]) && ( i - n_type < 4 ) )
			aster = 1, n_aster=i;

		if (!strcmp("optional",tk[i]) || !strcmp("OPTIONAL",tk[i]) )
			opt = 1, n_opt = i, ult=i;

		if (!inten && (!strcmp("intent",tk[i]) || !strcmp("INTENT",tk[i]) ))
		{	inten = 1, n_inten = i, ult=i;
			for (k=i+1; k< i+8; k++)
				if (tk[k][0] == ')')
					ult=k;
		}

		if (!strcmp("allocatable",pasar_a_minusc(tk[i])) )
			alloca = 1, n_alloca = i, ult=i;

		if (!strcmp("save",pasar_a_minusc(tk[i])) )
			save = 1, n_save = i, ult=i;

		if (!strcmp("function",pasar_a_minusc(tk[i])) )
		{	func = 1, n_func = i;
		}

		if (!strcmp("&",tk[i]) )
			cont = 1, n_cont = i;
	}



	strcpy(s_varb,tk[n_type]);
	if (minus)
		strcpy(s_varb,pasar_a_minusc(tk[n_type]));


	if (inten)
	{
		/* condiciones para que este todo bien */
		f4=1;
		if (f4 && strcmp(tk[n_inten+1],"("))
			f4 = 0;
		if (f4 && strcmp(tk[n_inten+3],")"))
			f4 = 0;
		if (f4 && 
			( strcmp("in",pasar_a_minusc(tk[n_inten+2]))  &&
			  strcmp("out",pasar_a_minusc(tk[n_inten+2])) &&
			  strcmp("inout",pasar_a_minusc(tk[n_inten+2]))  )
		   )
			f4 = 0;

		if (f4)
		{
			sprintf (s_inte,"%s%s%s%s",tk[n_inten],tk[n_inten+1],tk[n_inten+2],tk[n_inten+3]);
			printf ("fix_dec_var1: INTE: |%s|\n",s_inte);
		}
		else
			error(1001);
	}
			

		
	/* algunos pifies -  casos excepcionales ... kind=4)variable .. */
	if (kind)
	{	

		for (f2=1, k=n_kind+1; k< n_kind+8; k++)
			if (tk[k][0] == ')' && tk[k+1][0] != ' ')
			{
				q_tk++;
				for (l=q_tk-1; l>= k+1 && l; l--)
				{	strcpy(tk[l],tk[l-1]);
				}
				strcpy (tk[k+1]," ");	
				ult = k;
				f2 = 0;
			}
	}


#if 0
no anda ... hay que mejorar esto del ultimo ...

	/* si hay coma despues de lo ultimo valido, es el caso  ... (kind=2), var ... */
	if (coma)
	{	if (n_coma > ult)
			tk[n_coma][0] = ' ';
	}
#endif 

#if 1
	/* algunos casos excepcionales ... logical* 4 var */
	if (aster)
	{	
		strcpy(nr,tk[n_aster+1]);
		if (!es_num_tk(nr) && tk[n_aster+1][0]==' ' && es_num_tk(tk[n_aster+2]) )
		{	strcpy(nr,tk[n_aster+2]);
			strcpy(tk[n_aster+1],nr);
			strcpy(tk[n_aster+2]," ");
		}
	}
#endif



	if (gp_fverbose("d4"))
	{
		printf ("RR1 n_type:   %d \n",n_type);
		printf ("RR1 n_inten:  %d \n",n_inten);
		printf ("RR1   inten:  %d \n",  inten);
		printf ("RR1   aster:  %d \n",  aster);
		printf ("RR1    kind:  %d \n",   kind);

		for ( i1 = 0 ; i1< 20 && i1 < q_tk; i1++)
			printf ("Token-1-  %d |%s| \n",i1,tk[i1]);
	}


	i = n_type;
	f1=0;


	/* caso 1
	 * el formato es correcto, no tocamos nada
	 *
	 * type (kind= nn ), intent(rr) :: pepe
	 * type (kind= nn )  function      pepe
	 * type                         :: pepe
	 * type              function      pepe 
	 *	
	 * tiene kind y dbl  o  tiene kind inten y dbl
	 *
	 */

	if ( !f1 && ((kind && dbl) || (kind && dbl && inten))  )
	{
		f1 = 1;
	}

	if ( !f1 && kind && func )
	{
		f1 = 1;
	}

	if ( !f1 && !aster && !kind && dbl && !func )
	{
		f1 = 1;
	}
	if ( !f1 && !aster && !kind && !dbl && func )
	{
		f1 = 1;
	}

	

	/* caso 2
	 * formato simple
	 *
	 * type*n pepe
	 *
	 * tiene aster 
	 * no tiene kind
	 * no tiene dbl
	 * no es function
	 *
	 * type pepe 	
	 *
	 * no tiene aster
	 * no tiene kind
	 * no tiene dbl
	 *
	 * type*n :: pepe
	 *
	 * tiene aster
	 * no tiene kind
	 * tiene dbl
	 * no es funcion
	 */

	if ( !f1 && aster && !kind && !dbl && !func )
	{
		ca = tk[n_aster+1][0];
		if (ca != '1' && ca != '2' && ca != '4' && ca != '8' )
			error(1002);

		sprintf (tk[i] , "%s (kind=%c) :: ",s_varb,ca);
		tk[i+1][0]=0;
		tk[i+2][0]=0;
		
		f1 = 1;
	}


	if ( !f1 && !kind && !dbl && !func )
	{

		sprintf (tk[i] , "%s :: ",s_varb);
		
		f1 = 1;
	}


#if 1
	if ( !f1 && aster && !kind && dbl && !func )
	{
		ca = tk[n_aster+1][0];
		if (ca != '1' && ca != '2' && ca != '4' && ca != '8' )
			error(1003);

		sprintf (tk[i] , "%s (kind=%c) ",s_varb,ca);
		tk[i+1][0]=0;
		tk[i+2][0]=0;
		
		f1 = 1;
	}

#endif


	/* caso 3
	 * formato simple
	 *
	 * type*n :: pepe
	 *
	 * tiene aster 
	 * no tiene kind
	 * tiene dbl
	 * no es function
	 *
 	 * type (kind=n) pepe
	 *
	 * no tiene aster
	 * tiene kind
	 * no tiene dbl
	 * no es funcion
	 *
	 */

	if ( !f1 && !aster && kind && !dbl && !func )
	{
		ca = tk[n_kind+4][0];
		if (ca != ' ' )
			error(1004);

		sprintf (tk[n_kind+4],"%s"," :: ");
		
		f1 = 1;

	}





	/* caso 4
	 * formato simple funcion
	 *
	 * type*n function pepe
	 *
	 * tiene aster
	 * no tiene kind
	 * no tiene dbl
	 * es function
	 */

	if ( !f1 && aster && !kind && !dbl && func )
	{
		ca = tk[n_aster+1][0];
		if (ca != '1' && ca != '2' && ca != '4' && ca != '8' )
			error(1005);

		sprintf (tk[i] , "%s (kind=%c)  ",s_varb,ca);
		tk[i+1][0]=0;
		tk[i+2][0]=0;
		
		f1 = 1;
	}


	/* caso 4
	 * formato con intent 
	 *
	 * type*n, intent pepe
	 *
	 * tiene aster
	 * no tiene kind
	 * no tiene dbl
 	 * tiene intent
	 * no es function
	 */

	if ( !f1 && aster && !kind && !dbl && inten && !func )
	{
		ca = tk[n_aster+1][0];
		if (ca != '1' && ca != '2' && ca != '4' && ca != '8' )
			error(1006);

		sprintf (tk[i] , "%s (kind=%c), %s :: ",s_varb,s_inte,ca);
		tk[i+1][0]=0;
		tk[i+2][0]=0;
		
		tk[n_inten+0][0] = 0;
		tk[n_inten+1][0] = 0;
		tk[n_inten+2][0] = 0;
		tk[n_inten+3][0] = 0;
		f1 = 1;
	}


	/* caso especial solo para  TRANSOBJ2.FOR 
	 *
	 * real (kind=4),
	 * se ve que sacaron el allocatable,
	 * quedo la coma, y lahey compila igual 
	 */

#if 1
	if ( !f1 && !aster && kind && !dbl && !inten && !alloca && !func )
	{
printf ("fix_dec_var1: FFF entro caso transobj2 \n");
printf ("EE1|%s| %c \n",tk[n_type],tk[n_kind+5][0]);
		/* caso especifico */
		if (!strcmp(tk[n_type],"REAL") && tk[n_kind+5][0]==','  )
		{
printf ("EE2|%s| %c \n",tk[n_type],tk[n_kind+5][0]);
			sprintf (tk[n_kind+5] , "%s"," :: ");
			f1 = 1;
		}

	}

#endif

	if (gp_fverbose("d4"))
	{
		for ( i1 = 0 ; i1< 15 && i1 < q_tk; i1++)
			printf ("fix_dec_var1: Token:  %d |%s| \n",i1,tk[i1]);
		printf ("\n");
		memset(b1,0,MAXB);
		for (m=0; m<q_tk; m++)
			strcat(b1,tk[m]);
		printf ("fix_dev_var1: Linea: |%s| \n\n",b1);
	}


	/* f1 1, resolvio f1 0, no pudo cambiar formato !! */
	if (f1)
	{	if (f_int)
			sq_integer++;
		if (f_rea)
			sq_real++;
		if (f_log)
			sq_logical++;
	}
	else
	{
		sq_variables_no_convertidas++;

	}


	return(f1);
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	fix_dec_var2
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * arregla declaracion de variables cuando son character
 */

int	fix_dec_var2()
{
	int	f2,f3;
	int	i,j,k,l,i1;
	int	minus, aster, len, opt, inten, alloca, save, func, cont, dbl;
	int	n_minus, n_aster, n_kind, n_opt, n_len;
	int	n_inten, n_alloca, n_save, n_func, n_cont, n_dbl;
	int	ult, n_type;
	int	f_aster,f_len;
	int	f_cha;

	char	ca;
	char	nr[16];
	char	s_varb[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];

	int	f1;	/* resolvio si no */

	memset(s_varb,0,MAXB);
	memset(b1,0,MAXB);


	alloca  = 0;
	func    = 0;
	inten   = 0;
	aster   = 0;
	save    = 0;
	len     = 0;
	dbl     = 0;
	ult     = 0;
	func    = 0;
	opt     = 0;
	cont    = 0;
	n_len   = 0;
	n_type  = 0;
	n_aster = 0;
	n_dbl   = 0;


	f_aster = 0;
	f_len   = 0;

	memset(b2,0,MAXB);
	for (j=0; j<q_tk; j++)
		strcat(b2,tk[j]);


	for (i=0; i< q_tk; i++)
	{	
		if (!strcmp("character",tk[i]) )
			f_cha=1,minus = 1, n_type = i;

		if (!strcmp("CHARACTER",tk[i]) )
			f_cha=1,minus = 0, n_type = i;

		if (!dbl && !strcmp(":",tk[i]) && !strcmp(":",tk[i+1]) )
			dbl=1,n_dbl = i;

		if (!f_aster && !strcmp("*",tk[i]) && ( i - n_type < 4 ) )
			f_aster= 1, aster = 1, n_aster=i;

		if (!f_len && !strcmp("len",tk[i]) || !strcmp("LEN",tk[i]) )
		{	f_len=1, len = 1, n_len = i, ult=i;
			for (k=i+1; k<i+8; k++)
				if (tk[k][0] == ')')
					ult=k;
		}

		if (!strcmp("optional",tk[i]) || !strcmp("OPTIONAL",tk[i]) )
			opt = 1, n_opt = i, ult=i;

		if (!strcmp("intent",tk[i]) || !strcmp("INTENT",tk[i]) )
		{	inten = 1, n_inten = i, ult=i;
			for (k=i+1; k< i+8; k++)
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


	strcpy(s_varb,tk[n_type]);
	if (minus)
		strcpy(s_varb,pasar_a_minusc(tk[n_type]));

		
	/* algunos casos excepcionales ...  len=4)varible ... */
	if (len)
	{	
printf ("ZZZfix_dec_var2: arregle el len=4)var ... \n");

		for (f2=1, k=n_len+1; k< n_len+8; k++)
			if (tk[k][0] == ')' && tk[k+1][0] != ' ')
			{
				q_tk++;
				for (l=q_tk-1; l>= k+1 && l; l--)
				{	strcpy(tk[l],tk[l-1]);
				}
				strcpy (tk[k+1]," ");	
				ult = k;
				f2 = 0;
			}
	}


#if 0
	/* algunos casos excepcionales ... character* 6 var */
	if (aster)
	{	
printf ("ZZZfix_dec_var2:1 estoy en if blanco despues de aster ... |%s| \n",b2);
		if (tk[n_aster+1][0] == ' ')
		{
printf ("ZZZfix_dec_var2:2 estoy en if blanco despues de aster ... |%s| \n",b2);

			strcpy(nr,tk[n_aster+2]);
			if (es_num_tk(nr))
			{
				strcpy(tk[n_aster+1],tk[n_aster+2]);
				strcpy(tk[n_aster+2]," ");
				n_aster--;
			}
			else
				error(2001);
		}
	}

#endif

#if 0
	if (aster)
	{	
		strcpy(nr,tk[n_aster+1]);
		if (!es_num_tk(nr))
		{
			if (tk[n_aster+1][0] != '(' ) 
				error(2001);
		}
	}
#endif

#if 1
	/* algunos casos excepcionales ... character* 6 var */
	if (aster)
	{	
		strcpy(nr,tk[n_aster+1]);
		if (!es_num_tk(nr) && tk[n_aster+1][0]==' ' && es_num_tk(tk[n_aster+2]) )
		{	strcpy(nr,tk[n_aster+2]);
			strcpy(tk[n_aster+1],nr);
			strcpy(tk[n_aster+2]," ");
		}
	}
#endif


	if (gp_fverbose("d3"))
	{
		for ( i1 = 0 ; i1< 15 && i1<q_tk ; i1++)
			printf ("Token-1-  %d |%s| \n",i1,tk[i1]);
	}

	i = n_type;
	f1=0;



 
	/* caso 0 - esta bien formado, no hay que hacer nada !
	 * tiene len
	 * tiene dbl 
	 *
	 * character (len=10) :: pepe
	 * character (len=*)  :: pepe
	 */

	if ( !f1 && (len && dbl) || (len && !dbl &&func)  )
	{
printf ("ZZZfix_dec_var2: estoy en caso 0 ... |%s| \n",b2);

		f1 = 1;
	}

	/* caso 1
	 * no tiene asterisco
	 * no tiene numero
	 * no tiene dbl
	 * no tiene len
	 * no es funcion 
	 * character esta solo en la linea (variables siguen en otra linea)
 	 *
	 * character 
	 */

	if ( !f1 && !aster && !dbl && !len && !func  )
	{
printf ("ZZZfix_dec_var2: estoy en caso 1 ... |%s| \n",b2);

		if (ult == 0)
			ult = i+1;

		if (ult == q_tk)
			q_tk++;

		sprintf (tk[ult],"%s"," :: ");
		f1 = 1;
	}


	/* caso 2 
	 * tiene asteristco
	 * tiene numero
	 * no tiene dbl
	 * no es funcion
	 *
	 * character*32 pepe
	 */
	
	if ( !f1 && aster && es_num_tk(nr) && !dbl && !func  )
	{
printf ("ZZZfix_dec_var2: estoy en caso 2 ... |%s| \n",b2);
		strcpy(nr,tk[n_aster+1]);
		tk[n_aster+1][0] = 0;

		sprintf (tk[i],"%s (len=%s) ",s_varb,nr);
	
		if (ult == 0)
			ult = i+1;

		sprintf (tk[ult],"%s",":: ");

#if 0
		/* casos excepcionales ... la PM con los blanquitos */
		if (tk[ult+1][0]=='*')
			tk[ult+1][0]=' ';
#endif
		f1 = 1;
	}

	/* caso 3 
	 * no tiene asteristco
	 * tiene len
	 * no tiene dbl
	 * no es funcion
	 * character esta solo en la linea (variables siguen en otra linea)
	 */

	if ( !f1 && !aster && !dbl && len && !func  )
	{
printf ("ZZZfix_dec_var2: estoy en caso 3 ... |%s| \n",b2);

		if (ult == 0)
			ult = i+1;

		if (ult != 0)
			ult++;

		if (ult == q_tk)
			q_tk++;

		sprintf (tk[ult],"%s"," :: ");
		f1 = 1;
	}


	/* caso 4 
	 * tiene asteristco
	 * tiene numero
	 * tiene dbl
	 * no es funcion
	 *
	 * character*32 :: pepe
	 */

	if (!f1 && aster && es_num_tk(tk[n_aster+1]) && dbl && !func )
	{
		strcpy(nr,tk[n_aster+1]);
		tk[n_aster+0][0] = 0;
		tk[n_aster+1][0] = 0;

		sprintf (tk[i],"%s (len=%s)",s_varb,nr);
	
		if (ult == 0)
			ult = i+1;

		f1 = 1;

	}

	
	/* caso 5 
	 * tiene asteristco
	 * tiene (*)
	 * no tiene dbl
	 * no es funcion
	 */
	
	if (!f1 && aster && !es_num_tk(tk[n_aster+1]) && !dbl && !func )
	{
		if (ult == 0)
		{	k=0;
			while (tk[n_aster+k][0] != ')')
				k++;
			ult = n_aster+k+1;

			if (ult == q_tk)
				q_tk++; 
		}

		sprintf (tk[n_aster+0],"%s"," ");
		sprintf (tk[n_aster+2],"%s","len=*");
		sprintf (tk[n_aster+4],"%s"," :: ");
		f1 = 1;
	}

	/* caso 6 
	 * tiene asteristco
	 * tiene (*)
	 * tiene dbl
	 * no es funcion
	 */
	
	if (!f1 && aster && !es_num_tk(tk[n_aster+1]) && dbl && !func )
	{
		if (ult == 0)
		{	k=0;
			while (tk[n_aster+k][0] != ')')
				k++;
			ult = n_aster+k+1;

			if (ult == q_tk)
				q_tk++; 
		}

		sprintf (tk[n_aster+0],"%s"," ");
		sprintf (tk[n_aster+2],"%s","len=*");
		sprintf (tk[n_aster+4],"%s"," ");
		f1 = 1;
	}







	/* no pudo hacer fix ! */
	if (!f1)
	{
		memset(b2,0,MAXB);
		for (j=0; j<q_tk; j++)
			strcat(b2,tk[j]);

		printf ("No hubo caso para .... |%s| \n",b2);

		if (gp_fverbose("d3"))
		{
			printf ("FIX_dev_var2: caso: X f1: %d |%s| \n",f1,b2);
		}
	}




	/* f1 1, resolvio f1 0, no pudo cambiar formato !! */
	if (f1)
	{	if (f_cha)
			sq_character++;
	}
	else
	{
		sq_variables_no_convertidas++;

	}


	return(f1);
}

















int	tiene_dec_var1()
{
	int	i,j;
	int	f1,f2,f3;
	char	b1[MAXV];

	int	f_comm, n_comm;

	f1=0;	/* f1 0 indica que no encontro declaracion de variable */
	
#if 0
	if (tk[0][0] == '!')
		f1 = 1;
#endif

	f_comm	= 0;
	n_comm  = 0;
	f2      = 1;
	memset (b1,0,MAXV);


	for (i=0; !f1 && f2 && i<q_tk; i++)
	{
		if ( !strcmp("integer", pasar_a_minusc( tk[i] )) )
			f1 = 1;
		if ( !strcmp("logical", pasar_a_minusc( tk[i] )) )
			f1 = 2;
		if ( !strcmp("real", pasar_a_minusc( tk[i] )) )
			f1 = 3;
		if ( !strcmp("character", pasar_a_minusc( tk[i] )) )
			f1 = 4;

		/* vuelvo a armar la linea a partir de los tokens */
		strcat(b1,tk[i]);

		/* condiciones para terminar de buscar */
		if (!strcmp("&",tk[i]))
			f2 = 0;
		if (!strcmp("!",tk[i]))
			f2 = 0;
		if (strlen(b1) > 12)
			f2 = 0;
			
	}

	return f1;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	largo_linea
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * devuelve el largo de linea real de una sentencia fortran
 * sin tener en cuenta si puso un chorro de comentarios al final
 *
 */


int	largo_linea(s)
char	*s;
{
	int	i,j,k;
	int	f1;


	k=0;
	for (i=0,f1=1; f1; i++)
		if (s[i] == 0 || ( i> 10 && s[i] == '!') )
			f1=0, k = i;

printf ("LLL: calculado  %3d |%s| \n",k,s);
printf ("LLL: strlen     %3d |%s| \n",strlen(s),s);

	return k;
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	linea_vacia_for 
 *
 *	edicion especial para sources fortran
 *
 *	Determina si una linea esta vacia (generalmente, para lineas leidas de files)
 *	La linea NO esta vacia si:
 *
 *	1) contiene al menos 1 caracter distinto de
 *	' ' 	blanco
 *	'\t' 	tab
 *	'\n'	new line
 *
 *	2) si el primer caracter es distinto de '!'
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

	/* si toda la linea es comentario, consideramos vacia para fortran */
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
		gp_uso(117);

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
			{	error(503);
			} 

			j=tipo_char(b1[p1]);

			switch (j)
			{
				/* otro caracter !!! */
				case TC_RST:
					flag_caracteres=1;
					if (gp_fverbose("d1"))
					{
						printf ("Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
						if (ffaux)
							fprintf (hfaux,"Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
							
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
 * 
 *	ffinp
 *	ffout   Archivo de salida con cambios realizados
 *	ffaux   Generalmente, errores del parser
 *	ffsta   Estadisticas de los procesos
 *
 *	ffckf	Ir guardando info general de todo el proceso 
 *
 *	Para entradas/salidas extras de los procesos
 *
 *	ffin2
 *	ffou2
 *	fflog
 *
 */


#if 1

int	pro_tool6()
{
	int	i,j,k;
	int	ql_ini,ql_fin;
	int	l1,n_l1;
	int	lml,n_lml;
	int	f1;
	int	pf,uf;

	char	b1[MAXB];

	char	z[MAXV];
	sprintf (z,"tool6");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/*
	 * en aux graba caracteres no contemplados por el parser
	 */

	/* que la vida sea mas leve */
	if (!ffaux)
		agregar_ffaux("parser.err");
	if (!ffsta)
		agregar_ffsta("tool.sta");
	if (!ffckf)
		agregar_ffckf("check.log");
	if (!fflog)
		agregar_fflog("p.log");


	if (!ffinp || !ffout || !ffaux)
		gp_uso(118);



	/* cargamos file en memo */
	fnq1 = &fnp[0];
	qfv_load(hfinp,fnq1,&ql_ini);

	/* compatibiliad con la corrida tipo bunch files */
	/* registro datos del archivo */
	qf_ff=0;
	tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
	if ( tb[qf_ff] == NULL )
		error(915);

	strcpy ( (*tb[qf_ff]).n, extract_fname(finp));
	(*tb[qf_ff]).pf = 0;
	(*tb[qf_ff]).uf = ql_ini-1;
	qf_ff++;

	/* proceso file */
	pf = (*tb[0]).pf ;
	uf = (*tb[0]).uf ;

	if (gp_fverbose("d3"))
	{
		for (i=0; i<qf_ff; i++)
		{
			printf ("PRO_TOOL6: file a procesar: |%s| \n", (*tb[i]).n );
		}
	}

	/* registro nombre de archivo en check log */
	if (ffckf)
	{
		fprintf (hfckf,"File: %s\n", (*tb[0]).n);
	}


#if 0
	lml=0;
	for (i=0; i<ql_ini; i++)
	{
		strcpy(b1, (*fnp[i]).l );
		if ( b1[0] != 'C' && b1[0] != 'c' && b1[0] != '!')
		{	
			l1 = strlen(b1);

			for (k=0,f1=1; f1 && k < strlen(b1); k++)
			{
				if (b1[k]=='!')
					f1=0,l1=k-1;
			}
				
			for (k=strlen(b1)-1, f1=1; f1 && k; k--)
				if (b1[k] == ' ')
					b1[k]=0, l1--;
				else
					f1 = 0;

			if ( l1 > lml )
			{	lml = l1, n_l1 = lne(i);
				printf ("LLL |%s|\n",b1);
			}
		}
	}
#endif


	lml_src_sc(&lml,&n_lml,pf,uf);



	if (gp_fverbose("d2"))
	{
		printf ("Linea mas larga: %4d\n\n",lml);
		printf ("|%s|\n",(*fnp[n_lml]).l );
	}


	if (ffckf)
	{
		fprintf (hfckf,"Linea mas larga encontrada antes de procesar file (sin coments)  %3d \n",lml);
		fprintf (hfckf,"Nro de linea: %4d \n",lne(n_lml));
		fprintf (hfckf,"|%s|\n",(*fnp[n_lml]).l );
	}


	/* mientras que no cambie la cant de lineas !!! */
	ql_fin=ql_ini;

	
	/* 0 - pidio cambiar comentarios                              */
	if ( ffchg_com )
		cfor_comm(&ql_ini,&ql_fin);

	/* 1 - pidio borrar comentarios                               */
	if ( ffchg_dec )
		cfor_dec(&ql_ini,&ql_fin);
	
	/* 2 - pidio cambiar type selectors                           */
	if ( ffchg_typ )
		cfor_vars(&ql_ini,&ql_fin);

	/* 3 - pidio sacar las lineas de cont. y replicar declaracion */
	if ( ffchg_mas )
		cfor_mas(&ql_ini,&ql_fin);

	/* 4 - pidio convertir formato var/xx/ por var = xx           */
	if ( ffchg_ini )
		cfor_ini(&ql_ini,&ql_fin);
	
	/* 5 - pidio cambiar lineas de continuacion                   */
	if ( ffchg_lco )
		cfor_lcon(&ql_ini,&ql_fin);

	/* 6 - pidio cambiar variables con blancos intermedios        */
	if ( ffchg_vcb )
		cfor_vcb(&ql_ini,&ql_fin);

	/* 7 - pidio agregar check a lineas con allocate              */
	if ( ffchg_alo )
		cfor_alo(&ql_ini,&ql_fin);
	

	/* hago un checking final de las lineas ! */
	if ( 0 )
	{
		if ( !check_file(&ql_fin) )
		{
			printf ("CHECK LINEAS  largas !! \n");
	
			if (ffckf)
			{
				fprintf (hfckf,"Atencion ! file quedo con filas largas (si es f90 no hacer caso) !! \n");
			}
		}
	}

	/* grabo file */
	for (i=0; i< ql_fin; i++)
	{
		strcpy(b1, (*fnp[i]).l );

		if (gp_minusculas)
			strcpy( b1 , pasar_a_minusc( (*fnp[i]).l )  );

		fprintf (hfout,"%s\n",b1);
	}


	mostrar_reportes("tool6");


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}

#endif




/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_dec
 *
 *	borra todos los comentarios
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * borra comentarios:
 * a) si son lineas de codigo 
 * b) que esten despues de lineas de codigo "activas "
 * 
 * deja:
 * aquellos comentarios que describen algoritmos y demas 
 *
 */


#if 1

int	cfor_dec(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k;
	int	p1,p2;
	int	f1,f2,f3,f4;
	int	qi,qf;
	int	fl_n;
	int	pf,uf,ln;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];


	qi = *ql_i;
	qf = *ql_f;
	memset(b4,' ',MAXB);


	/* compatibilidad con cfor_vars / p_src3 etc
	 *
	 * primera y ultima fila del source 
	 * poner nombre del file en table de files
	 */

	pf = 0;
	uf = *ql_i - 1;

	fl_n = 0;

#if 1
	/* registro datos del archivo */
	tb[fl_n] = (ffptr ) malloc (sizeof (ff));
	if ( tb[fl_n] == NULL )
		error(916);

	strcpy ( (*tb[fl_n]).n, extract_fname(fout));
	(*tb[fl_n]).pf = pf; 
	(*tb[fl_n]).uf = uf; 
	(*tb[fl_n]).ql = uf - pf + 1;

#endif



	/* recorro todas las lineas */
	for (i=pf; i<= uf ; i++)
	{
		/* copio linea y linea siguiente */
		strcpy(b1, (*fnp[i]).l );
		strcpy(b2, (*fnp[i]).l );
		strcpy(b3, (*fnp[i]).l );

#if 0
		/* parseo en tokens */
		l_pars(i,&q_tk);

		if(gp_fverbose("d4"))
		{
			printf ("cfor_dec: b1: |%s| \n",b1);
			for (j=0; j<q_tk; j++)
			{
				printf ("cfor_dec: TK: %3d (%3d) |%s|\n",j,strlen(tk[j]),tk[j]);
			}	
			printf ("\n");
		}
#endif

#if 0
		if ( es_linea_comentario(b1))
		{
			if (gp_fverbose("d3"))
			{
				printf ("cfor_dec: com detectado \n");
				printf ("cfor_dec: %5d |%s|\n",lne(i),b1);
			}
		
			strcpy(b2,"!");

			/* actualizo stats */
			sq_comentarios_elim++;
		}
#endif

	/* borro comentarios al final */

		if (gp_cborrar == 1)
		{
			if ( !es_linea_comentario(b3) && tiene_coment_final (b3,&p1))
			{
				b3[p1] ='\0';

				if (fflog)
				{
					fprintf (hflog,"%5d %s\n",lne(i),b1);
					fprintf (hflog,"%5d %s\n",lne(i),b3);
					fprintf (hflog,"%s","\n");
				}

				sq_coment_al_final_elim++;
			}
		}


		/* armo la linea de nuevo con todos los tokens */
#if 0
		memset (b3,0,MAXB);
		for (j=0; j< q_tk; j++)
			strcat (b3,tk[j]);
#endif

		strcpy ( (*fnp[i]).l, b3);


	}
}




#endif


/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_comm
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	devuelve true si la linea es de este tipo 
 *      
 *      !     aca hay cualquier cosa      ! comentario
 *            aca hay cualquier cosa      ! comentario
 *            aca hay cualquier cosa      ! comentario !!! expresivo
 *
 *	no modifica las lineas tipo
 *
 *      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 *            aca hay cualquier cosa ' no tocar ! '  
 *
 *	ojo con las lineas que tienen pepe's margin .... ( que HDP !!! )
 *
 *	INTEGER (KIND=2) :: LOCAL_MAX_MONTHS=12,LOCAL_MAX_YEARS=12 ! UP'ED FOR DOUG 051310 ! UP'ED FOR SCOTT 02/26/08 ! UP'ED FOR SCOTT 01/08/04
 *
 */

int	tiene_coment_final(s,p)
char	*s;
int	*p;
{
	int	i,j,k;
	int	l1;
	int	c1,c2,c3,c4;
	int	p1,p2;
	int	f1,f2,f3,f4;
	int	f_ret,f_sig;
	char	b1[MAXB];



	/* lo basico .. si no nay ' y encuentro un ! ... es coment */


	f_ret = 0;
	f_sig = 1;

	f1 = 0;
	f4 = 1;
	l1 = strlen(s);
	i = 7;             /* la eleccion de i es crucial ... */

	while (f4 && i < l1 )
	{
		if ( f1 && s[i] == '\'' )
			f1 = 0;

		if (!f1 && s[i] == '\'' )
			f1 = 1;

		if (!f1 && s[i] == '!' )
		{
			f_ret = 1;
			f_sig = 0;
			*p = i;
			f4 = 0;
		}

		i++;
	}
		



	if (f_sig)
	{

	f_ret = 0;
	f_sig = 1;
	i = strlen(s);
	*p = 0;

		
	/* si despues de ! hay un solo ' , todo lo demas no funca */
	c4 = 0;
	f4 = 1;
	j  = 8;
       	p2 = 0;

	while ( f4 && j < i)
	{	if (s[j] == '!')
			p2 = j;

		if (p2 && s[j] == '\'')
		{
			c4++;
		}

		j++;
	}

	}


	/* ver si no se mando !!!!!!!!!!!!!!! !! !!! !!!! - hay cada uno */
	if (f_sig)
	{
		c1 = 0;
	 	c2 = 0;
		c3 = 0;

		for (i=0; i < strlen(s); i++)
		{	if (s[i] == '!')
				c1++;
			if (s[i] == ' ')
				c2++;
			if (s[i] != '!' && s[i] != ' ')
				c3++;
		}

		if (c1 > 15 && c3 < 10)
		{
			f_sig = 0;
			f_ret = 0;
		}
	}

			

	i = strlen(s);
	f1 = 0;
	f2 = 0;

	while (f_sig)
	{
		/* solo si hay mas de un solo '  */
		if (c4 > 1)
		{

		if (s[i] == '\'')
		{	if (f1 == 0)
				f1 = 1;
			else
				f1 = 0;
		}
		}

		
		if (s[i] == '"')
		{	if (f2 == 0)
				f2 = 1;
			else
				f2 = 0;
		}


		if (f1 == 0 && f2 == 0)
		{
			if (s[i] == '!')
			{	f_ret = 1;
				p1 = i;
				*p = i;
			}
		}

		i--;
		if (i <= 6)
			f_sig = 0;
	}

	return (f_ret);
}

















/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_comm
 *
 * -----------------------------------------------------------------------------------
 */


#if 1

int	cfor_comm(ql_i,ql_f)
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

	/* recorro todas las lineas */
	for (i=0; i< qi ; i++)
	{
		/* copio linea y linea siguiente */
		strcpy(b1, (*fnp[i]).l );

		/* truchada ... */
		if (b1[4]=='!')
		{
			b1[4]=' ';
			b1[0]='!';
		}

		l_pars(i,&q_tk);

		if (tk[4][0]=='!')
		{
			tk[4][0]=' ';
			tk[0][0]='!';
		}

		if(gp_fverbose("d4"))
		{
			for (j=0; j<q_tk; j++)
			{
				printf ("TK: %3d %3d |%s|\n",j,strlen(tk[j]),tk[j]);
				if (tk[j][0] == 9)
					printf ("TAB: !!!! en linea %5d |%s| \n",i,b1);
			}
			printf ("voy b1: |%s| \n",b1);
		}

		if ( es_linea_comentario(b1))
		{
			if (gp_fverbose("d3"))
			{
				printf ("cfor: com detectado \n");
				printf ("cfor: %4d #tk %4d |%s|\n",i,q_tk,b1);
			}

			/* pongo comentario
			 * hay casos en que el comentario es 
			 * 'C1000 ..... '
			 * 
			 */
#if 0
			strcpy (tk[0],"!");
#endif
			tk[0][0]='!';

			/* actualizo stats */
			sq_lineas_com++;
		}

		/* armo la linea de nuevo con todos los tokens */
		memset (b3,0,MAXB);
		for (j=0; j< q_tk; j++)
			strcat (b3,tk[j]);

		strcpy ( (*fnp[i]).l, b3);

		if (gp_fverbose("d3"))
			printf ("fix: |%s|\n",b3);

	}
}




#endif



/*
 *	recibe una linea completa
 *	revisa condiciones para determinar si
 *	la linea entera es un comentario
 */

int	es_linea_comentario(s)
char	*s;
{

	int	i,j,k;
	int	f1,f2,f3;

	char	b1[MAXB];

	strcpy(b1,s);

	f1 = 1;		/* sigo verificando */
	f2 = 1;		/* es comentario */
	i  = 0;
	
	
	if ( b1[0] == 'c' || b1[0] == 'C' || b1[0] == '!' )
		f1 = 0;
	else	
		f2 = 0;

	/* caso especial encontrado */
	if ( !strncmp(b1,"***",3))
		f2 = 1, f1 = 0;

	/* hay casos en los que la linea es '    !' */
	while (f1 && i< strlen(b1) )
	{
		if (b1[i] == '!')
			f1 = 0, f2 = 1;

		if ( b1[i] != ' ' && b1[i] != '!' )
			f1 = 0, f2 = 0;

		i++;
	}

	return f2;
}



int	es_linea_comentario2(s)
char	*s;
{

	int	i,j,k;
	int	f1,f2,f3;

	char	b1[MAXB];

	strcpy(b1,s);

	f1 = 1;		/* sigo verificando */
	f2 = 1;		/* es comentario */
	i  = 0;
	

	if ( b1[5] == '+' || b1[6] == '+')
	{
		f1=0;
	}
	else
	{

	if ( b1[0] == 'c' || b1[0] == 'C' || b1[0] == '!' )
		f1 = 0;
	else	
		f2 = 0;


	while (f1 && i< strlen(b1) )
	{
		if (b1[i] == '!')
			f1 = 0, f2 = 1;

		if ( b1[i] != ' ' && b1[i] != '!' )
			f1 = 0, f2 = 0;

		i++;
	}
	}

	return f2;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_lcon
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	en esta linea falla ! lo pone antes del !
 *
 *	error....   CALL MG_LOCATE_WRITE(15,35,'error reading definition file!',
 *
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
			memset (b3,0,MAXB);
			for (j=0; j< q_tk; j++)
				strcat (b3,tk[j]);

			if (tiene_coment_intermedio (b3) )
			{
				for (j=2, f4=1; f4 && j<q_tk; j++)
				{
					if (!strcmp(tk[j],"!"))
					{
						sprintf (tk[j],"& ! ");
						f4=0;
					}
				}
			}
			else
			{
				/* ponemos el & al final ... */
				memset(tk[q_tk],0,MAXB);

				if (gp_rellena)
				{	
					strncpy(tk[q_tk++],b4,90-strlen(b3));
				}

				sprintf (tk[q_tk++],"   &");
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


			sq_lcont++;
		}


		/* armo la linea de nuevo con todos los tokens */
		memset (b3,0,MAXB);
		for (j=0; j< q_tk; j++)
			strcat (b3,tk[j]);
		strcpy ( (*fnp[i]).l, b3);
		if (gp_fverbose("d3"))
			printf ("fix: |%s|\n",b3);

		/* copio la segunda linea, sin el mas */
		strcpy ( (*fnp[i+1]).l, b2);
	}
}



/*
 *	en esta linea falla ! lo pone antes del !
 *
 *	error....   CALL MG_LOCATE_WRITE(15,35,'error reading definition file!',
 *
 */

int	tiene_coment_intermedio (s)
char	*s;
{
	int 	i,j,k;
	int	f1,f2,f3;
	int	c1;

	for (i=0, c1=0, f2=0; i<strlen(s); i++)
		if (s[i] == '\'')
		{
			c1++;
			if ( c1 == 2)
				f2 = 1;
		}


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



int	tiene_amper(s)
char	*s;
{
	int	i,j,k;	
	int	f1,f2,f3;
	char	b1[MAXB];
	int	l2;

	f1=0;
	f2=1;

	strcpy(b1,s);
	l2 = strlen(b1);

	for (i=0; !f1 && f2 && i<l2; i++)
	{
		if (b1[i] == '&')
		{	
			f1 = 1;
			f2 = 0;
		}
	}

	return f1;
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_tool 7 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	tool7
 *
 *	Devuelve linea mas larga en file
 *	Esto es fundamental para luego saber como procesar
 *	archivos con la opcion --chglcon
 *	(lineas de continuacion )
 *
 */


/* bloque */
#if 1


int	pro_tool7()
{
	int	i,j,k;
	int	f1,f2,f3,f4;
	int	p1;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;

	int	lml;
	int	n_lml;
	
	int	lml2;
	int	n_lml2;
	
	int	ql_ini;



	char	z[MAXV];
	sprintf (z,"tool7");

	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp )
		gp_uso(119);

	/* bloque */
	/* cargamos file en memo */
	fnq1 = &fnp[0];
	qfv_load(hfinp,fnq1,&ql_ini);

	lml    = 0;
	n_lml  = 0;
	lml2   = 0;
	n_lml2 = 0;

	for (i=0, f1 = 0; i<ql_ini; i++)
	{
		strcpy(b1,(*fnp[i]).l);


		if ( f4 = tiene_coment_final(b1,&p1))
			b1[p1] = 0;


		if (!es_linea_comentario2 (b1))
		{
			if ( (k = strlen( b1) ) > lml)
				lml = k, n_lml = i;

		}

		if (es_linea_comentario2 (b1) )
		{	

			
			f1++;
			if ( (k = strlen( b1 )) > lml2)
				lml2 = k, n_lml2 = i;
		}

	}

	if (gp_fverbose("d1"))
	{
		
		printf ("\n\n");
		printf ("Cantidad de lineas %7s : %d %s\n"," ",ql_ini," s/com");
		printf ("Linea mas larga   (%5d): %d \n",n_lml+1,lml);
		printf ("|%s|\n\n",(*fnp[n_lml]).l );
		if (f1)
		{
		printf ("Cantidad de lineas %7s : %d %s\n"," ",f1," c/com");
		printf ("Linea mas larga   (%5d): %d \n",n_lml2+1,lml2);
		printf ("|%s|\n\n",(*fnp[n_lml2]).l );
		printf ("\n\n");
		}
			
	
	}


	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */



/* bloque 1 */
#if 1


/*
 * -----------------------------------------------------------------------------------
 *
 *	lml_src_sc 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	Devuelve linea mas larga en file
 *	considera lineas sin comentarios
 *
 */


int	lml_src_sc(ar_lml,ar_n_lml,pf,uf)
int	*ar_lml;
int	*ar_n_lml;
int	pf,uf;
{
	int	i,j,k;
	int	f1,f2,f3,f4;
	int	p1;
	char	b1[MAXB];

	int	lml1;
	int	n_lml1;
	
	int	lml2;
	int	n_lml2;
	

	lml1   = 0;
	n_lml1 = 0;
	lml2   = 0;
	n_lml2 = 0;

	for (i=pf; i<= uf; i++)
	{
		strcpy(b1,(*fnp[i]).l);

		if ( tiene_coment_final(b1,&p1) )
			b1[p1] = 0;


		if (!es_linea_comentario2 (b1))
		{
			if ( (k = strlen( b1) ) > lml1)
				lml1 = k, n_lml1 = i;
		}

		if (es_linea_comentario2 (b1) )
		{	
			if ( (k = strlen( b1 )) > lml2)
				lml2 = k, n_lml2 = i;
		}
	}

	*ar_lml   = lml1;
	*ar_n_lml = n_lml1;

}


#endif
/* bloque 1 */



/*
 * -----------------------------------------------------------------------------------
 *
 *	len_linea_src           
 *
 * -----------------------------------------------------------------------------------
 */


int	len_linea_src(s,tipo,l1,l2)
char	*s;
int	*tipo;
int	*l1,*l2;
{






}





/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_tool 8 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	tool8
 *
 *
 *	revisar mthnmcom ...
 *	muestra que variables han quedado con un mix de blanco y _
 */


/* bloque */
#if 1


int	pro_tool8()
{
	int	i,j,k;
	int	f1,f2,f3;
	int	f_sigo,flag;
	int	p1,p2;
	char	b1[MAXB];
	char	b2[MAXB];
	int	c1,c2;
	int	ql;
	FILE	*hwi;

	char	z[MAXV];
	sprintf (z,"tool8");

	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!fflog)
		agregar_fflog("vars.log");
	if (!ffinp || !ffout)
		gp_uso(120);



	ql=0;
	while (fgets(b1,MAXB,hfinp) != NULL)
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
	
			for (i=0, f_sigo=1; f_sigo && i<strlen(b1); i++)
			{
				if (!strncmp(b1+i,"::",2))
				{
					f_sigo=0;
					strcpy(b2,b1+i+3);

					for (j=strlen(b2), f_sigo=1; f_sigo && j >= 7; j--)
					{	
						if (b2[j]=='!')
						{
							b2[j]=0;
							j--;
							while (b2[j]==' ')
								b2[j]=0,j--;

							f_sigo=0;
						}
					}


					/* grabo todas las variables encontradas */
					printf ("var:%3d |%s|\n",ql,b2);
					fprintf (hfout,"|%s|\n",b2);


					/* si la var tiene mix de blancos y _ ... mando a log */
					for (j=0, c1=0, c2=0; j<strlen(b2); j++)
					{
						if (b2[j]==' ')
							c1++;
						if (b2[j]=='_')
							c2++;

					}

					if (c1 && c2)
						fprintf (hflog,"|%s|\n",b2);


				}
			}

			ql++;
		}
	}
	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_tool 9 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	tool9
 *
 */


/* bloque */
#if 1


int	pro_tool9()
{
	int	i,j,k;
	int	f1,f2,f3;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;

	char	z[MAXV];
	sprintf (z,"tool9");

	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout)
		gp_uso(121);



	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */





/*
 * -----------------------------------------------------------------------------------
 *
 *	lne (line)
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * para que las lineas que imprimo de los vectores (que empiezan en cero)
 * coincidan con las lineas en los archivos al verlos con vi (que empieza en  1)
 */


int	lne(l)
int	l;
{
	return (l+1);
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

	if ( ffin3 && ((hfin3 = fopen (fin3,"r")) == NULL) )
	{
		error(125);
	}

	if ( ffout && ((hfout = fopen (fout,"w")) == NULL) )
	{
		error(103);
	}

	if ( ffou2 && ((hfou2 = fopen (fou2,"w")) == NULL) )
	{
		error(104);
	}

	if ( ffou3 && ((hfou3 = fopen (fou3,"w")) == NULL) )
	{
		error(134);
	}

	if ( ffou4 && ((hfou4 = fopen (fou4,"w")) == NULL) )
	{
		error(135);
	}

	if ( ffou5 && ((hfou5 = fopen (fou5,"w")) == NULL) )
	{
		error(136);
	}

	if ( ffou6 && ((hfou6 = fopen (fou6,"w")) == NULL) )
	{
		error(137);
	}

	if ( ffaux && ((hfaux = fopen (faux,"w")) == NULL) )
	{
		error(105);
	}
	
	if ( fflog && ((hflog = fopen (flog,"w")) == NULL) )
	{
		error(106);
	}

	if ( ffsta && ((hfsta = fopen (fsta,"w")) == NULL) )
	{
		error(107);
	}

	if ( ffckf && ((hfckf = fopen (fckf,"w")) == NULL) )
	{
		error(108);
	}

	if ( ffsrc && ((hfsrc = fopen (fsrc,"r")) == NULL) )
	{
		error(109);
	}

	if ( fflst && ((hflst = fopen (flst,"r")) == NULL) )
	{
		error(110);
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

	if ( ffin3)
		fclose(hfin3);

	if ( ffout)
		fclose(hfout);

	if ( ffou2)
		fclose(hfou2);

	if ( ffou3)
		fclose(hfou3);

	if ( ffou4)
		fclose(hfou4);

	if ( ffou5)
		fclose(hfou5);

	if ( ffou6)
		fclose(hfou6);

	if ( ffaux)
		fclose(hfaux);

	if ( fflog)
		fclose(hflog);

	if ( ffsta)
		fclose(hfsta);

	if ( ffckf)
		fclose(hfckf);

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

	if (gp_verbose("d1"))
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
 *	atenti:
 * 	agregamos para l_pars (que lo usamos mucho para el proyecto fortran)
 *	que cambie tab TC_BLA ... (cuando es 9), por 4 blancos 
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
	if (gp_fverbose("d3"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}



	/* si encuentro caracteres no considerados para el parser, avisar al final de todo el proceso */
	flag_caracteres = 0;

	q_lin=0;
	memset(b1,32,MAXB);
	memset(b2,0,MAXB);

	strcpy (b1, (*fnp[ line_number ]).l );

	/* si no hay tokens ... devuelvo cant de tokens en 0 */
	q_tk=0;
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
			printf ("l_pars: Linea  : %d \n",line_number);
			printf ("l_pars: Buffer :|%s|\n",  b1);
		}


		/* comienzo parser de tokens */
		p1=0;
		q_tk=0;

		while ( f1 )
		{
			/* controlamos cantidad de tokens ... */
			if (q_tk > MAXT-10)
			{	error(504);
			} 

			j=tipo_char(b1[p1]);

			switch (j)
			{
				/* otro caracter !!! */
				case TC_RST:
					flag_caracteres=1;
					if (gp_fverbose("d1"))
					{
						printf ("Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
						if (ffaux)
							fprintf (hfaux,"Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
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
			printf ("l_pars: Termine de parsear linea (%d tokens) \n",q_tk);


		/* verifico si hay que sacar output  en minusculas */
		if (gp_minusculas)
		{
			for (j=0; j< q_tk; j++)
				strcpy(tk[j],pasar_a_minusc(tk[j]));
		}




	/* solo para l_pars version proyecto fortran !! - tabs x 4 blancos 
	 * en caso de usar tipo_char para parser
	 * TC_BLA viene definido true si es blanco o tab
	 * y esta seteado en guardar 1 solo x token
	 */

		if (gp_tabs)
		{
			for (j=0; j< q_tk; j++)
			{
				if (tk[j][0] == 9)
					sprintf (tk[j],"%s","    ");
			}
		}

#if 0
		/* verificamos si quedo bien la linea !! */
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
	if (gp_fverbose("d3"))
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
 *	agregar archivos utiles si no los pidieron
 *
 * -----------------------------------------------------------------------------------
 */


int	agregar_fflog(s)
char	*s;
{
	fflog = 1;
	strcpy(flog,s);

	if ( fflog && ((hflog = fopen (flog,"w")) == NULL) )
	{
		error(124);
	}
}

int	agregar_ffaux(s)
char	*s;
{
	ffaux = 1;
	strcpy(faux,s);

	if ( ffaux && ((hfaux = fopen (faux,"w")) == NULL) )
	{
		error(121);
	}
}

int	agregar_ffsta(s)
char	*s;
{
	ffsta = 1;
	strcpy(fsta,s);

	if ( ffsta && ((hfsta = fopen (fsta,"w")) == NULL) )
	{
		error(122);
	}
}

int	agregar_ffckf(s)
char	*s;
{
	ffckf = 1;
	strcpy(fckf,s);

	if ( ffckf && ((hfckf = fopen (fckf,"w")) == NULL) )
	{
		error(123);
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	devuelve nombre de file base + fecha, hora 
 *
 * -----------------------------------------------------------------------------------
 */


char	*fname(bname) 
char	*bname;
{
    static char 	nombre_completo[MAXP];
    char 		fecha_hora[MAXF];

    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    /* Formatear la fecha y hora */
    snprintf(fecha_hora, sizeof(fecha_hora), "_%02d%02d%02d_%02d%02d",
		tm.tm_year % 100, 
		tm.tm_mon + 1, 
		tm.tm_mday,
		tm.tm_hour,
		tm.tm_min);

    /* Crear el nombre completo */
    snprintf(nombre_completo, sizeof(nombre_completo), "%s%s", bname, fecha_hora);

    return nombre_completo;
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	devuelve variables fortran en un string
 * -----------------------------------------------------------------------------------
 */



#if 1


int rutina_v(string,nro,vector_ptr) 
char	*string;
int	*nro;
char	**vector_ptr;
{
    int count = 0;
    char *token;
    char *separators = " _"; // Separadores permitidos (blancos y '_')
    char *temp_string = strdup(string); // Hacemos una copia temporal del string
    char *str_ptr = temp_string;

    // Tokenizamos el string de acuerdo con los separadores
    while ((token = strtok(str_ptr, separators)) != NULL) 
    {
        str_ptr = NULL; // Para continuar tokenizando el string original
        
        // Validamos la sintaxis de str1 (debe comenzar con una letra y puede seguir con letras o números)
        if (isalpha(token[0])) 
        {
            int valid = 1;
            for (int i = 1; i < strlen(token); i++) 
            {
                if (!isalnum(token[i])) 
                {
                    valid = 0; // No válido si tiene algo que no sea letra o número
                    break;
                }
            }

            if (valid) 
            {
                vector_ptr[count] = token; // Guardamos el puntero al token válido
                count++;
            }

        }
    }

    // Liberamos la memoria temporal
    free(temp_string);
    
    *nro = count; // Guardamos el número de variables encontradas

    return count;     // Retornamos cant de variables encontradas
}



#if 0
int main() {
    char string[1024] = "str1 str2 str2_str2";
    int nro = 0;
    char *vector[100]; // Vector para almacenar punteros a las variables encontradas

    // Llamada a la rutina
    rutina_v(string, &nro, vector);

    // Imprimimos los resultados
    printf("Variables encontradas: %d\n", nro);
    for (int i = 0; i < nro; i++) {
        printf("Variable %d: %s\n", i + 1, vector[i]);
    }

    return 0;
}

#endif


#endif












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
	printf ("Uso cfg arc       : %2d\n",ffcfg);
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



	char	s_opcion[MAXR];	/* opcion menu ... en str */
	int	f1,f2,fg;
	char	b1[MAXB];
	int	k_cnt;
 
	int 	i,j,fl;
	char	prm[MAXV];
	char	exe[MAXV];

	char	var1[MAXB];	/* provisorio !! */
	FILE	*hwi;


	strcpy(s_opcion,"0");


	/* excepcion con verbose, por si lo pusieron al final de la linea, por defecto d5 !!! */

	for (i=0; i < gp_fq(GP_GET,0); i++  )
	{	
		if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'v' && !tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{	gp_verbose = 1;
			strcpy(gp_opciones,"d5");

			if (gp_fverbose("d3"))
				printf ("Parametro encontrado: -v \n");
		}

		if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"opciones",6) )
		{	strcpy(gp_opciones,desde_igual( gp_fp(GP_GET,i,(char **)0)));

			if (gp_fverbose("d3"))
				printf ("Parametro encontrado: -opciones \n");
		}

		if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'x' && !tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{	
			strcpy (exe, ( gp_fp(GP_GET,i,(char **)0) + 2 ));
			strcpy(s_opcion,exe);

			/* pongo defaults si no quiere usar -cfg=archio ... */
			ffcfg=1;
			strcpy(var1,"tfor.cfg");

			if (gp_fverbose("d3"))
				printf ("Parametro encontrado: -x |%s| \n",s_opcion);
		}

	}



#if 1

	/* Experimentos .... 
	 * Archivo de configuracion (tipo menu)
	 */

	/* si selecciono archivo de configuracion */
	for (i=0, fl=1; fl && i < gp_fq(GP_GET,0); i++)
	{
		if (gp_fverbose("d3"))
			printf ("Arg: %2d  s:|%s| \n", i, gp_fp(GP_GET,i,(char **) 0 ));

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
			qfv_load(hwi,fnq1,&qf_lin);
			
			if (gp_fverbose("d3"))
			{	printf ("Cant lineas arc de configuracion: %d\n",qf_lin);
				for (j=0; j< qf_lin; j++)
					printf ("Par %2d |%s| \n",j,(*fnp[j]).l);
			}

			fclose(hwi);

			k_cnt = 0;

			for (j=0, f2=1, fg=0; f2 && j<qf_lin; j++)
			{

				f1 = 1;
				if ( (*fnp[j]).l[0] == '#')
					f1 = 0;
				if ( (*fnp[j]).l[0] == ' ')
					f1 = 0;
				if ( (*fnp[j]).l[0] ==  0 )
					f1 = 0;


				/* revisar !!!! aca deberiamos elegir la opcion a ejecutar  */
				if (f1)
				{		

					if ( !strncmp( pasar_a_minusc( (*fnp[j]).l ),"opcion",6) ) 
					{
						strcpy(b1, desde_igual( (*fnp[j]).l ));


						if ( !strcmp(b1,s_opcion))
						{	fg = 1;
						}
					}

					if ( strncmp( (*fnp[j]).l,"opcion",6) && strncmp( (*fnp[j]).l,"end",3 ) )
					{
						if (fg)
						{	
							mis_argv = &mis_args[0];
							mis_args[ mis_argc ] = malloc ( strlen( (*fnp[j]).l ) + 2 );
							strcpy ( mis_args[mis_argc] , ( *fnp[j]).l );

							mis_argv++;
							mis_argc++;
						}

					}


					if ( !strncmp( (*fnp[j]).l,"end",3) ) 
					{
						if ( !strcmp(b1,s_opcion))
						{
							fl = 0;
							f2 = 0;
						}
					}
				} /* if (f1) */
			} /* for (j=0 ... */
		} /* if (ffcfg ... */
	} /* for (i=0 ... analisis si selecciono archivo de conf */


	mis_argv = &mis_args[0];
	if (gp_fverbose("d3"))
	{
		printf ("Arch de config solicit : %2d (ffcfg)  \n",ffcfg);
		printf ("Cantidad de argumentos : %2d          \n",mis_argc);
		printf ("\n");

		for (j=0; j<mis_argc; j++)
			printf ("Arg nro: %2d  str: |%-20.20s|  ptr: |%-20-20s|  \n",j,mis_args[j], *mis_argv++);
	}


	mis_argv = &mis_args[0];
	/* si pidio usar archivo de configuracion, reseteo pointers a args */
	if (ffcfg)
	{
		mis_argv = &mis_args[0];
		gp_init(mis_argc,mis_argv);
	}




	if (gp_fverbose("d3"))
	{
		printf ("\n");
		printf ("Argumentos despues de correspondientes gp_init ... \n");
		printf ("\n");
		printf ("cantidad de args : %d  \n",   gp_fq(GP_GET,0) );
		for (i=0; i < gp_fq(GP_GET,0); i++  )
		{
			printf ("arg(%2d) |%s| \n",i,  gp_fp(GP_GET,i,(char **)0)   );
		}

	}

#endif

	/* Seguimos con el proceso normal de analisis de parametros ingresados 
	 * ffcfg 0 - parametros tomados de linea de comando 
	 * ffcfg 1 - pidio usar archivo de configuracion - parametros tomados del archivo
	 *
	 */
	

	/* procesamos todos los parametros existentes */
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

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 't'  )
				gp_tabs = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'r'  )
				gp_rellena = 1;

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

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"in3",3) )
			{
				strcpy(fin3,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffin3=1;
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

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou3",3) )
			{
				strcpy(fou3,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou3=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou4",3) )
			{
				strcpy(fou4,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou4=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou5",3) )
			{
				strcpy(fou5,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou5=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou6",3) )
			{
				strcpy(fou6,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou6=1;
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

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"sta",3) )
			{	
				strcpy(fsta,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffsta=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ckf",3) )
			{	
				strcpy(fckf,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffckf=1;
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
			{	
				gp_niveldes = *desde_igual( gp_fp(GP_GET,i,(char **)0)) - '0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"opt",3) )
			{	
				gp_suboption = *desde_igual( gp_fp(GP_GET,i,(char **)0)) - '0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"cab",3) )
			{	
				gp_cborrar = *desde_igual( gp_fp(GP_GET,i,(char **)0)) - '0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"opciones",6) )
			{	
				strcpy(gp_opciones,desde_igual( gp_fp(GP_GET,i,(char **)0)));
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"debug",5) )
			{	
				gp_debug = *desde_igual( gp_fp(GP_GET,i,(char **)0)) - '0';
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

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"version",7) )
			{	
				gp_vers = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgcom",6) )
			{	
				ffchg_com = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgdec",6) )
			{	
				ffchg_dec = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgtyp",6) )
			{	
				ffchg_typ = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chglco",6) )
			{	
				ffchg_lco = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgmas",6) )
			{	
				ffchg_mas = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgini",6) )
			{	
				ffchg_ini = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgvcb",6) )
			{	
				ffchg_vcb = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgalo",6) )
			{	
				ffchg_alo = 1;
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

	printf ("Uso cfg arc       : %2d\n",ffcfg);

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

	/* encontrado en UN fuente fortran ....  */
	if (c == '|'  )
		x = TC_CVR;

	/* encontrado en UN fuente fortran ....  */
	if (c == '~'  )
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
	printf ("%s --version                           numero de version  / compilacion                           \n",z);
	printf ("%s -h                                  help                                                       \n",z);
	printf ("%s -v                                  verbose ... muestra cierta informacion de proceso          \n",z);
	printf ("%s -v -opciones=AxByCz...              info: A,B,C = (D)ebug, (I)nformative, (E) extra x=(0-5)    \n",z);
	printf ("%s                                     no imprime, 1 basico, 2 y 3 debug, 4 full debug        \n",w);
	printf ("%s -cfg=archivo.txt -xN                utiliza un archivo de config para pars ... opcion N        \n",z);
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
	printf ("tool6:         carga un fuente - opciones para arreglos varios - genera nuevo fuente              \n");
	printf ("%s -v -opciones=d5 -tool=6 -inp=f_org -out=f_new -aux=parser.err                                  \n",z);
	printf ("      --chgcom  convierte lineas comentadas                                                       \n");
	printf ("      --chgtyp  arregla especificacion de variables (kind,len) pone los :: en int log real char   \n");
	printf ("      --chglcp  arregla lineas de continuacion ... reemplaza + por &                              \n");
	printf ("                                                                                                  \n");
	printf ("prue2:         carga todos los archivos en list_src a  memo y los genera en new_repo              \n");
 	printf ("%s -v -opciones=d5 -prue=2 -inp=list_src -out=l_names -aux=p.err -dato=new_repo                   \n",z);
	printf ("                                                                                                  \n");
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
	gp_vers=0;
	gp_verbose=0;

	ffcfg=0;
	ffinp=0;
	ffin2=0;
	ffout=0;
	ffou2=0;
	ffou3=0;
	ffou4=0;
	ffou5=0;
	ffou6=0;
	ffaux=0;
	fflog=0;
	ffsta=0;
	ffckf=0;

	ffsrc=0;
	fflst=0;

	ffdat=0;

	sprintf (gp_opciones, "%s","______");
	sprintf (gp_dato    , "%s","______");
	sprintf (gp_pruebas , "%s","______");
	sprintf (gp_exec    , "%s","______");
	sprintf (gp_proc    , "%s","______");
	sprintf (gp_tool    , "%s","______");
	sprintf (gp_reportes, "%s","saludo");


	memset(gp_tpar,0,sizeof(gp_tpar));

}



/*
 * -----------------------------------------------------------------------------------
 *
 * 	version
 *
 *	version del codigo
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_version(x)
int	x;
{
	static	char	ver[MAXB];
	char	d[MAXV];
	char	w[MAXV];
	char	z[MAXB];

	strcpy (ver,"0049");
	strcpy (d," Wed Sep 18 05:53:47 -03 2024");

	sprintf (z,"%s -- (%s)  %s", gp_fp(GP_GET,0,(char **)0), ver, d  );
	memset (w,0,MAXV);
	strncpy (w,"                                        ",strlen(z));

	if (x)
	{
		printf ("%s\n",z);
		printf ("\n\n");
	}

	if (x)
		exit(x);
}



/*
 * -----------------------------------------------------------------------------------
 *
 * 	minusculas (version chatgpt )
 *
 * -----------------------------------------------------------------------------------
 */

// Función auxiliar para convertir una cadena a minúsculas

char *minusculas(char *s) 
{
    char *ptr = s;
    while (*ptr) 
    {
        *ptr = tolower(*ptr);
        ptr++;
    }
    return s;
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
#if 0
	printf ("gp_fverbose:  f_i : %d\n",f_i);
	printf ("gp_fverbose:  f_e : %d\n",f_e);
	printf ("gp_fverbose:  f_d : %d\n",f_d);
#endif
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





#if 0

#include<stdio.h>
#include <string.h>

int main() {
   char string[50] = "Hello! We are learning about strtok";
   // Extract the first token
   char * token = strtok(string, " ");
   // loop through the string to extract all other tokens
   while( token != NULL ) {
      printf( " %s\n", token ); //printing each token
      token = strtok(NULL, " ");
   }
   return 0;
}


#endif



#if 0



#include <stdio.h>
#include <string.h>
#include <time.h>

char* archivo_log(char *nombre_log) 
{
    static char nombre_completo[256];
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    // Formatear la fecha y hora
    char fecha_hora[20];
    snprintf(fecha_hora, sizeof(fecha_hora), "_%02d%02d%02d_%02d%02d",
             tm.tm_year % 100, tm.tm_mon + 1, tm.tm_mday,
             tm.tm_hour, tm.tm_min);

    // Crear el nombre completo
    snprintf(nombre_completo, sizeof(nombre_completo), "%s%s", nombre_log, fecha_hora);

    return nombre_completo;
}

int main()
{
    char nombre_log[256];
    strcpy(nombre_log, "prueba");

    strcpy(nombre_log, archivo_log(nombre_log));
    printf("Nombre del archivo: %s\n", nombre_log);

    return 0;
}




#endif




#if 0



#include <stdio.h>
#include <string.h>
#include <stdbool.h>

bool tiene_include(char *s, char *t) 
{
    // Define la cadena a buscar
    const char *pattern = "   include  '";
    int pattern_len = strlen(pattern);

    // Busca el patrón en el string s
    char *start = strstr(s, pattern);

    if (start != NULL) 
    {
        // Encuentra el inicio del nombre de archivo
        start += pattern_len;
        char *end = strchr(start, '\'');
        
        if (end != NULL) 
        {
            // Copia el nombre de archivo sin las comillas en t
            strncpy(t, start, end - start);
            t[end - start] = '\0';
            return true;
        }
    }

    // No se encontró el patrón
    return false;
}

int main() 
{
    char s[] = "Este es un ejemplo de cadena con   include  'nombre_de_file_valido' en el medio.";
    char t[100];

    if (tiene_include(s, t)) 
    {
        printf("Se encontró la sentencia. El nombre de archivo es: %s\n", t);
    }
    else 
    {
        printf("No se encontró la sentencia.\n");
    }

    return 0;
}




#endif



#if 0



#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

bool tiene_include(char *s, char *t) {
    // Define la cadena a buscar
    const char *keyword = "include";
    char *start = strstr(s, keyword);

    if (start != NULL) {
        // Avanza el puntero más allá de "include"
        start += strlen(keyword);

        // Salta los espacios en blanco
        while (isspace(*start)) {
            start++;
        }

        // Verifica si el siguiente carácter es un apóstrofe
        if (*start == '\'') {
            start++; // Salta el apóstrofe inicial

            // Encuentra el apóstrofe final
            char *end = strchr(start, '\'');
            if (end != NULL) {
                // Copia el nombre de archivo sin las comillas en t
                strncpy(t, start, end - start);
                t[end - start] = '\0';
                return true;
            }
        }
    }

    // No se encontró el patrón
    return false;
}

int main() {
    char s[] = "Este es un ejemplo de cadena con include    'nombre_de_file_valido' en el medio.";
    char t[100];

    if (tiene_include(s, t)) {
        printf("Se encontró la sentencia. El nombre de archivo es: %s\n", t);
    } else {
        printf("No se encontró la sentencia.\n");
    }

    return 0;
}



















#endif



/*
 * -----------------------------------------------------------------------------------
 *	end of source  ( I hope)
 *	end of source
 *	end of source
 * -----------------------------------------------------------------------------------
 */


#if 0

#include <stdlib.h>
#include <string.h>

// Ejemplo de estructura para info
typedef struct {
    char data[MAXB];
    // Otros campos...
} info_t;

// Función de comparación corregida
int info_cmp1(const void *a, const void *b) {
    // Convertir punteros void a punteros info_t
    const info_t *ia = (const info_t *)a;
    const info_t *ib = (const info_t *)b;

    // Comparar los campos de la estructura (ajusta según lo necesario)
    return strcmp(ia->data, ib->data);
}

// Otra función de comparación de ejemplo
int info_cmp2(const void *a, const void *b) {
    const info_t *ia = (const info_t *)a;
    const info_t *ib = (const info_t *)b;

    // Comparar los campos de la estructura (ajusta según lo necesario)
    return strcmp(ia->data, ib->data);
}

// Función que usa qsort
void ex3_p2() {
    info_t info[MAXN]; // Ajusta según tu implementación
    int n_info = /* número de elementos en info */;
    
    qsort(&info[1], n_info, sizeof(info_t), info_cmp1);
}

void ex3_p3() {
    info_t info[MAXN]; // Ajusta según tu implementación
    int n_info = /* número de elementos en info */;
    
    qsort(&info[0], n_info, sizeof(info_t), info_cmp2);
}





#endif





#if 0


#include <stdlib.h>
#include <string.h>

#define MAXB  1024

char info[1000][MAXB];
int n_info = 1000;

// Función de comparación corregida
int info_cmp1(const void *p1, const void *p2) 
{
    // Convertir punteros void a punteros char
    const char *a = (const char *)p1;
    const char *b = (const char *)p2;

    return strcmp(a, b);
}

// Ejemplo de uso de qsort
void ex3_p2() 
{
    qsort(&info[0], n_info, MAXB, info_cmp1);
}

int main() 
{
    // Llamada a la función para ordenar
    ex3_p2();

    return 0;
}


#endif


#if 0

//
//!     
//! INPUT DATA LIST
//!
//      INTEGER (KIND=4) :: VALUES_2_ZERO
//
//      INTEGER (KIND=2) :: SCENARIO_YEAR,TABLE,SCENARIO_INDEX
//      INTEGER (KIND=2) :: LOCAL_MAX_NUM_MARKET_AREAS=0
//      INTEGER (KIND=2) :: LOCAL_MAX_HOURS=24,LOCAL_MAX_DAYS=31
//
//!         1         2         3         4         5         6         7         6         7         8         9
//!12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
//      INTEGER (KIND=2) :: LOCAL_MAX_MONTHS=12,LOCAL_MAX_YEARS=12 ! UP'ED FOR DOUG 051310 ! UP'ED FOR SCOTT 02/26/08 ! UP'ED FOR SCOTT 01/08/04
//      INTEGER (KIND=2) :: MIN_YEAR=9999,MAX_YEAR=0
//      REAL (KIND=4) :: R_MONTHLY_SLOPE,R_MONTHLY_INTERCEPT
//      CHARACTER (LEN=20) :: HYDRO_VARIABLES
//      INTEGER (KIND=2) :: WD_INDEX(:,:,:,:) ! MARKET_ID, LOCAL_MONTH, LOCAL_DAY, LOCAL_YEAR
//      ALLOCATABLE :: WD_INDEX
//!      
//!      SAVE WD_LOAD,WD_INDEX
//!
//! END DATA DECLARATIONS      
//!
//

#endif


#if 0

            CALL MG_LOCATE_WRITE(14,35,
     +                             'Base Case Definition is missing or',
     +                                                   ALL_VERSIONS,1)
error....   CALL MG_LOCATE_WRITE(15,35,'error reading definition file!',
     +                                                   ALL_VERSIONS,1)
            er_message="bfil.0001 - Base case definition missing or "
     + //"error reading definition file."
            call end_program(er_message)
#endif




#if 0

		if (f4)
		{	
			f5 = 1;
			while ( f5 )
			{
				strcpy(b2,b1);

				f5 = 0;
				if (p1 = es_cadena_int_src3(b1,&m1))
				{	f5 = 1;

					memset(d1,0,MAXR);
					strncpy(d1,b1+p1,m1);

		printf ("XXX1: |%s| \n",d1);
						memset (d2,0,MAXR);
						strcpy(d2,d1);
						d2[0] = '=';
						d2[m1-1] = ' ';
						sq_vinit_simple++;
		printf ("XXX2: |%s| \n",d2);

					strncpy(b1+p1,d2,m1);

					sprintf (b4,"f:%-30.30s l:%06d c:%4d|%s| \n", (*tb[fl_n]).n,lne(j),sq_vinit_simple,b2 );
					sprintf (b5,"f:%-30.30s l:%06d c:%4d|%s| \n", (*tb[fl_n]).n,lne(j),sq_vinit_simple,b1 );
					
					if (gp_fverbose("d3"))
					{
						printf ("TTTX\n");
						printf ("TTTX%s",b4);
						printf ("TTTX%s",b5);
						printf ("TTTX\n");
					}

#if 1
					if (fflog)
					{
						fprintf (hflog,"%s",b4);
						fprintf (hflog,"%s",b5);
						fprintf (hflog,"%s","\n");
					}
#endif
				}
			}
		}	


#endif






#if 0

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

int rutina_v(char string[1024], int *nro, char **vector_ptr) {
    int count = 0;
    char *token;
    char *separators = " _"; // Separadores permitidos (blancos y '_')
    char *temp_string = strdup(string); // Hacemos una copia temporal del string
    char *str_ptr = temp_string;

    // Tokenizamos el string de acuerdo con los separadores
    while ((token = strtok(str_ptr, separators)) != NULL) {
        str_ptr = NULL; // Para continuar tokenizando el string original
        
        // Validamos la sintaxis de str1 (debe comenzar con una letra y puede seguir con letras o números)
        if (isalpha(token[0])) {
            int valid = 1;
            for (int i = 1; i < strlen(token); i++) {
                if (!isalnum(token[i])) {
                    valid = 0; // No válido si tiene algo que no sea letra o número
                    break;
                }
            }
            if (valid) {
                vector_ptr[count] = token; // Guardamos el puntero al token válido
                count++;
            }
        }
    }

    // Liberamos la memoria temporal
    free(temp_string);
    
    *nro = count; // Guardamos el número de variables encontradas
    return 0;     // Retornamos 0 en caso de éxito
}

int main() {
    char string[1024] = "str1 str2 str2_str2";
    int nro = 0;
    char *vector[100]; // Vector para almacenar punteros a las variables encontradas

    // Llamada a la rutina
    rutina_v(string, &nro, vector);

    // Imprimimos los resultados
    printf("Variables encontradas: %d\n", nro);
    for (int i = 0; i < nro; i++) {
        printf("Variable %d: %s\n", i + 1, vector[i]);
    }

    return 0;
}



#endif


#if 0

#include <stdio.h>
#include <stdlib.h>

// Comparison function for qsort
int compare(const void* a, const void* b) {
    return (*(int*)a - *(int*)b);
}

// Function to remove duplicates from a sorted array
int remove_duplicates(int* arr, int n) {
    if (n == 0) return 0;

    int j = 0;
    for (int i = 1; i < n; i++) {
        if (arr[j] != arr[i]) {
            j++;
            arr[j] = arr[i];
        }
    }
    return j + 1; // New length of array without duplicates
}

int main() {
    int arr[] = {4, 2, 9, 2, 4, 6, 9, 3};
    int n = sizeof(arr) / sizeof(arr[0]);

    // Sorting the array
    qsort(arr, n, sizeof(int), compare);

    // Removing duplicates
    int new_len = remove_duplicates(arr, n);

    // Printing the unique elements
    printf("Unique elements: ");
    for (int i = 0; i < new_len; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");

    return 0;
}


#endif



#if 0
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 100
#define MAX_LENGTH 1024

char vcb_o[MAX_LINES][MAX_LENGTH];
int qv1;

int compare(const void *a, const void *b) {
    return strcmp(*(const char **)a, *(const char **)b);
}

int main() {
    // Suponiendo que qv1 ya está definido y vcb_o está lleno con datos

    // Crear un array de punteros a las cadenas en vcb_o
    char *pointers[MAX_LINES];
    for (int i = 0; i < qv1; ++i) {
        pointers[i] = vcb_o[i];
    }

    // Ordenar el array de punteros
    qsort(pointers, qv1, sizeof(char *), compare);

    // Copiar las cadenas ordenadas de vuelta a vcb_o
    for (int i = 0; i < qv1; ++i) {
        strcpy(vcb_o[i], pointers[i]);
    }

    // Imprimir el resultado
    for (int i = 0; i < qv1; ++i) {
        printf("%s\n", vcb_o[i]);
    }

    return 0;
}


#endif




#if 0

int compare_vcb(const void *a, const void *b) 
{
    // Hacemos un cast a char * porque sabemos que estamos ordenando strings
    const char *ia = *(const char **)a;
    const char *ib = *(const char **)b;

    return strcmp(ia, ib);
}

#endif








#if 0
/*
 * esto vino de copilot !
 * lo hizo mejor que chatgpt
 *
 * encuentra variables en fuentes fortran exactas !! 
 * ademas de otras cosas, pero bueh va ...
 *
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_VARS 100
#define MAX_LINE_LENGTH 1024

typedef struct 
{
    char word[50];
    int value;
}   DictionaryEntry;


void rutina_x(char *, int *, int *, char **, int *, DictionaryEntry *, int );



#if 0
void rutina_x(char s[MAX_LINE_LENGHT], int *n, int vn[MAX_VARS], char *vp[MAX_VARS], int k[MAX_VARS], DictionaryEntry dict[], int dict_size) 
#endif

void rutina_x(s, n, vn, vp, k, dict, dict_size) 
char	*s;
int	*n;
int	*vn;
char	**vp;
int	*k;
DictionaryEntry *dict;
int	dict_size;
{

    int start;
    int len;
    int i;

    // Inicializar el contador de variables
    *n = 0; 
    len = strlen(s);
    i = 0;

    while (i < len) 
    {
        // Saltar espacios en blanco
        while (i < len && isspace(s[i])) 
        {
            i++;
        }

        // Si encontramos una letra, es el inicio de una variable
        if (i < len && isalpha(s[i])) 
        {
            // Guardar el puntero al inicio de la variable
            vp[*n] = &s[i]; 
            start = i;

            // Avanzar hasta el final de la variable
            while (i < len && (isalnum(s[i]) || s[i] == ' ' || s[i] == '_')) 
            {
                i++;
            }

            // Guardar la longitud de la variable
            vn[*n] = i - start; 

            // Verificar si la variable está en el diccionario
            k[*n] = 0; 
            for (int j = 0; j < dict_size; j++) 
            {
                if (strncmp(vp[*n], dict[j].word, vn[*n]) == 0 && strlen(dict[j].word) == vn[*n]) 
                {
                    k[*n] = dict[j].value;
                    break;
                }
            }

            (*n)++; 
        } 
        else 
        {
            i++;
        }
    }
}

int main(int argc, char *argv[]) 
{
    DictionaryEntry dict[MAX_VARS];

    if (argc != 4) 
    {
        fprintf(stderr, "Uso: %s <archivo_diccionario> <archivo_entrada> <archivo_salida>\n", argv[0]);
        return 1;
    }

    // Leer el archivo de diccionario
    FILE *file_dict = fopen(argv[1], "r");
    if (file_dict == NULL) 
    {
        perror("Error al abrir el archivo de diccionario");
        return 1;
    }

    int dict_size = 0;
    while (fscanf(file_dict, "%49[^,],%d\n", dict[dict_size].word, &dict[dict_size].value) == 2) {
        dict_size++;
    }
    fclose(file_dict);

    // Leer el archivo de entrada
    FILE *file_in = fopen(argv[2], "r");
    if (file_in == NULL) {
        perror("Error al abrir el archivo de entrada");
        return 1;
    }

    // Abrir el archivo de salida
    FILE *file_out = fopen(argv[3], "w");
    if (file_out == NULL) {
        perror("Error al abrir el archivo de salida");
        fclose(file_in);
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file_in)) {
        int n;
        int vn[MAX_VARS];
        char *vp[MAX_VARS];
        int k[MAX_VARS];

        // Escribir la línea original en el archivo de salida
        fprintf(file_out, "Línea: %s", line);

        // Procesar la línea para encontrar variables
        rutina_x(line, &n, vn, vp, k, dict, dict_size);

        // Escribir las variables encontradas en el archivo de salida
        for (int i = 0; i < n; i++) {
            fprintf(file_out, "%d (%2d) %d |%-*.*s|\n", i + 1, vn[i], k[i], vn[i], vn[i], vp[i]);
        }
        fprintf(file_out, "\n");
    }

    fclose(file_in);
    fclose(file_out);
    return 0;
}









#endif




#if 0
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define MAXB 1024

// Función auxiliar para convertir una cadena a minúsculas
char *pasar_a_minusc(char *s) {
    char *ptr = s;
    while (*ptr) {
        *ptr = tolower(*ptr);
        ptr++;
    }
    return s;
}

// Subrutina que verifica la combinación "= nombre_valido ( ... )"
int tiene_funcion_a_entry(char *linea, char *nombre_valido) {
    char b1[MAXB];
    strcpy(b1, pasar_a_minusc(linea));

    char *ptr = b1;
    char *igual_pos = strstr(ptr, "=");

    if (!igual_pos)
        return 0; // No hay "=" en la línea

    ptr = igual_pos + 1; // Avanzar después de "="
    while (*ptr && isspace(*ptr)) // Saltar espacios en blanco
        ptr++;

    // Verificar si el nombre válido coincide después de "="
    if (strncmp(ptr, nombre_valido, strlen(nombre_valido)) == 0) {
        ptr += strlen(nombre_valido);
        while (*ptr && isspace(*ptr)) // Saltar espacios en blanco después de nombre_valido
            ptr++;

        if (*ptr == '(') {
            ptr++; // Avanzar después de "("
            while (*ptr && *ptr != ')') // Ignorar contenido entre paréntesis
                ptr++;

            if (*ptr == ')') // Verificar si hay un paréntesis de cierre
                return 1;
        }
    }

    return 0; // No se cumple la condición
}

#endif


#if 0

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define MAXB 1024

// Función auxiliar para convertir una cadena a minúsculas
char *pasar_a_minusc(char *s) {
    char *ptr = s;
    while (*ptr) {
        *ptr = tolower(*ptr);
        ptr++;
    }
    return s;
}

// Subrutina que verifica si "nombre_valido" es invocada como función en la línea
int tiene_funcion_a_entry(char *linea, char *nombre_valido) {
    char b1[MAXB];
    strcpy(b1, pasar_a_minusc(linea));

    char *ptr = b1;
    char *func_pos = strstr(ptr, nombre_valido);

    while (func_pos) {
        ptr = func_pos + strlen(nombre_valido);

        // Saltar espacios en blanco después del nombre de la función
        while (*ptr && isspace(*ptr))
            ptr++;

        // Verificar si está seguido de un paréntesis de apertura
        if (*ptr == '(') {
            ptr++; // Avanzar después de "("
            int paren_count = 1;

            // Verificar el contenido dentro de los paréntesis
            while (*ptr && paren_count > 0) {
                if (*ptr == '(')
                    paren_count++;
                else if (*ptr == ')')
                    paren_count--;
                ptr++;
            }

            // Si se cerraron todos los paréntesis, es una invocación válida
            if (paren_count == 0)
                return 1;
        }

        // Buscar la próxima aparición de nombre_valido
        func_pos = strstr(func_pos + 1, nombre_valido);
    }

    return 0; // No se encontró una invocación válida
}




#endif

#if 0

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define TRUE 1
#define FALSE 0

int tiene_entry(const char *linea) {
    const char *ptr = linea;

    // Convertir a minúsculas para hacer la búsqueda insensible a mayúsculas
    char buffer[1024];
    int i = 0;
    while (*ptr && i < 1023) {
        buffer[i++] = tolower(*ptr++);
    }
    buffer[i] = '\0';

    // Buscar "entry" en el string
    char *entry_pos = strstr(buffer, "entry");
    while (entry_pos) {
        // Verificar que "entry" no esté precedido ni seguido por caracteres alfanuméricos o guiones bajos
        if ((entry_pos == buffer || !isalnum(*(entry_pos - 1)) && *(entry_pos - 1) != '_') &&
            isspace(*(entry_pos + 5))) {
            return TRUE;
        }
        // Buscar la próxima aparición de "entry"
        entry_pos = strstr(entry_pos + 1, "entry");
    }

    return FALSE;
}



#endif


/* end of file  */
/* end of file  */
/* end of file  */


#if 0
00001 COAL_VECTOR_OBJ.F90            (f90) 00155 |               ALLOCATE (COAL_VECTOR_MAP(-1:MaxVectorNum))|
00002 COAL_VECTOR_OBJ.F90            (f90) 00162 |               ALLOCATE (COAL_VECTOR_TYPE_IS_VALUE(-1:nArray), &|
00003 Coal_Model_Modules.f90         (f90) 00470 |                  allocate(AllArray(LowB1:UpB1))|
00004 Coal_Model_Modules.f90         (f90) 00473 |               allocate(AllArray(LowB1:UpB1))|
00005 Coal_Model_Modules.f90         (f90) 00484 |                  allocate(AllArray(UpB1))|
00006 Coal_Model_Modules.f90         (f90) 00487 |               allocate(AllArray(UpB1))|
00007 Coal_Model_Modules.f90         (f90) 00497 |                  allocate(AllArray(UpB1))|
00008 Coal_Model_Modules.f90         (f90) 00500 |               allocate(AllArray(UpB1))|
00009 Coal_Model_Modules.f90         (f90) 00510 |                  allocate(AllArray(LowB1:UpB1))|
00010 Coal_Model_Modules.f90         (f90) 00513 |               allocate(AllArray(LowB1:UpB1))|
00011 Coal_Model_Modules.f90         (f90) 00524 |                  allocate(AllArray(UpB1))|
00012 Coal_Model_Modules.f90         (f90) 00527 |               allocate(AllArray(UpB1))|
00013 Coal_Model_Modules.f90         (f90) 00539 |                  allocate(AllArray(UpB1,UpB2))|
00014 Coal_Model_Modules.f90         (f90) 00542 |               allocate(AllArray(UpB1,UpB2))|
00015 Coal_Model_Modules.f90         (f90) 00553 |                  allocate(AllArray(UpB1))|
00016 Coal_Model_Modules.f90         (f90) 00556 |               allocate(AllArray(UpB1))|
00017 Coal_Model_Modules.f90         (f90) 00566 |                  allocate(AllArray(UpB1))|
00018 Coal_Model_Modules.f90         (f90) 00569 |               allocate(AllArray(UpB1))|
00019 Coal_Model_Modules.f90         (f90) 00581 |                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2))|
00020 Coal_Model_Modules.f90         (f90) 00584 |               allocate(AllArray(LowB1:UpB1,LowB2:UpB2))|
00021 Coal_Model_Modules.f90         (f90) 00595 |                  allocate(AllArray(UpB1))|
00022 Coal_Model_Modules.f90         (f90) 00598 |               allocate(AllArray(UpB1))|
00023 Coal_Model_Modules.f90         (f90) 00608 |                  allocate(AllArray(UpB1))|
00024 Coal_Model_Modules.f90         (f90) 00611 |               allocate(AllArray(UpB1))|
00025 Coal_Model_Modules.f90         (f90) 00623 |                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2))|
00026 Coal_Model_Modules.f90         (f90) 00626 |               allocate(AllArray(LowB1:UpB1,LowB2:UpB2))|
00027 Coal_Model_Modules.f90         (f90) 00637 |                  allocate(AllArray(UpB1))|
00028 Coal_Model_Modules.f90         (f90) 00640 |               allocate(AllArray(UpB1))|
00029 Coal_Model_Modules.f90         (f90) 00651 |                  allocate(AllArray(UpB1))|
00030 Coal_Model_Modules.f90         (f90) 00654 |               allocate(AllArray(UpB1))|
00031 Coal_Model_Modules.f90         (f90) 00664 |                  allocate(AllArray(UpB1))|
00032 Coal_Model_Modules.f90         (f90) 00667 |               allocate(AllArray(UpB1))|
00033 Coal_Model_Modules.f90         (f90) 00679 |                  allocate(AllArray(UpB1,UpB2))|
00034 Coal_Model_Modules.f90         (f90) 00682 |               allocate(AllArray(UpB1,UpB2))|
00035 Coal_Model_Modules.f90         (f90) 00692 |                  allocate(AllArray(LowB1:UpB1))|
00036 Coal_Model_Modules.f90         (f90) 00695 |               allocate(AllArray(LowB1:UpB1))|
00037 Coal_Model_Modules.f90         (f90) 00707 |                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2))|
00038 Coal_Model_Modules.f90         (f90) 00710 |               allocate(AllArray(LowB1:UpB1,LowB2:UpB2))|
00039 Coal_Model_Modules.f90         (f90) 00725 |                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2,LowB3:UpB3))|
00040 Coal_Model_Modules.f90         (f90) 00728 |               allocate(AllArray(LowB1:UpB1,LowB2:UpB2,LowB3:UpB3))|
00041 Coal_Model_Modules.f90         (f90) 00739 |                  allocate(AllArray(UpB1))|
00042 Coal_Model_Modules.f90         (f90) 00742 |               allocate(AllArray(UpB1))|
00043 Coal_Model_Modules.f90         (f90) 00754 |                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2))|
00044 Coal_Model_Modules.f90         (f90) 00757 |               allocate(AllArray(LowB1:UpB1,LowB2:UpB2))|
00045 Coal_Model_Modules.f90         (f90) 00769 |                  allocate(AllArray(UpB1))|
00046 Coal_Model_Modules.f90         (f90) 00772 |               allocate(AllArray(UpB1))|
00047 Coal_Model_Modules.f90         (f90) 00783 |                  allocate(AllArray(UpB1))|
00048 Coal_Model_Modules.f90         (f90) 00786 |               allocate(AllArray(UpB1))|
00049 Coal_Model_Modules.f90         (f90) 00798 |                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2))|
00050 Coal_Model_Modules.f90         (f90) 00801 |               allocate(AllArray(LowB1:UpB1,LowB2:UpB2))|
00051 MIDASFileIndexingRoutines.f90  (f90) 00131 |         ALLOCATE(DIMENSION_NAME(DIMENSIONS),      &|
00052 MIDASFileIndexingRoutines.f90  (f90) 00169 |         ALLOCATE(DEM_1_PTR(MAX_UNIQUE_VALUES))|
00053 MIDASFileIndexingRoutines.f90  (f90) 00171 |         ALLOCATE(DEM_1_NAME_STORAGE(MAX_UNIQUE_VALUES))|
00054 MIDASFileIndexingRoutines.f90  (f90) 00173 |         ALLOCATE(DEM_2_PTR(MAX_UNIQUE_VALUES))|
00055 MIDASFileIndexingRoutines.f90  (f90) 00175 |         ALLOCATE(DEM_2_NAME_STORAGE(MAX_UNIQUE_VALUES))|
00056 MIDASFileIndexingRoutines.f90  (f90) 00178 |            ALLOCATE(DEM_3_PTR(MAX_UNIQUE_VALUES))|
00057 MIDASFileIndexingRoutines.f90  (f90) 00180 |            ALLOCATE(DEM_3_NAME_STORAGE(MAX_UNIQUE_VALUES))|
00058 MIDASFileIndexingRoutines.f90  (f90) 00183 |            ALLOCATE(DEM_4_PTR(MAX_UNIQUE_VALUES))|
00059 MIDASFileIndexingRoutines.f90  (f90) 00185 |            ALLOCATE(DEM_4_NAME_STORAGE(MAX_UNIQUE_VALUES))|
00060 MIDASFileIndexingRoutines.f90  (f90) 00188 |            ALLOCATE(DEM_5_PTR(MAX_UNIQUE_VALUES))|
00061 MIDASFileIndexingRoutines.f90  (f90) 00190 |            ALLOCATE(DEM_5_NAME_STORAGE(MAX_UNIQUE_VALUES))|
00062 MIDASFileIndexingRoutines.f90  (f90) 00193 |            ALLOCATE(DEM_6_PTR(MAX_UNIQUE_VALUES))|
00063 MIDASFileIndexingRoutines.f90  (f90) 00195 |            ALLOCATE(DEM_6_NAME_STORAGE(MAX_UNIQUE_VALUES))|
00064 MIDASFileIndexingRoutines.f90  (f90) 00198 |            ALLOCATE(DEM_7_PTR(MAX_UNIQUE_VALUES))|
00065 MIDASFileIndexingRoutines.f90  (f90) 00200 |            ALLOCATE(DEM_7_NAME_STORAGE(MAX_UNIQUE_VALUES))|
00066 MIDASFileIndexingRoutines.f90  (f90) 00352 |         ALLOCATE(DEM_1_PTR(DEM_1_UNIQUE_VALUES),           &|
00067 MIDASFileIndexingRoutines.f90  (f90) 00360 |         ALLOCATE(DEM_2_PTR(DEM_2_UNIQUE_VALUES),           & |
00068 MIDASFileIndexingRoutines.f90  (f90) 00370 |            ALLOCATE(DEM_3_PTR(DEM_3_UNIQUE_VALUES),        &|
00069 MIDASFileIndexingRoutines.f90  (f90) 00379 |            ALLOCATE(DEM_4_PTR(DEM_4_UNIQUE_VALUES),        &|
00070 MIDASFileIndexingRoutines.f90  (f90) 00389 |            ALLOCATE(DEM_5_PTR(DEM_5_UNIQUE_VALUES),        &|
00071 MIDASFileIndexingRoutines.f90  (f90) 00398 |            ALLOCATE(DEM_6_PTR(DEM_6_UNIQUE_VALUES),        &|
00072 MIDASFileIndexingRoutines.f90  (f90) 00407 |            ALLOCATE(DEM_7_PTR(DEM_7_UNIQUE_VALUES),        &|
00073 andecomp.f90                   (f90) 00211 |         ALLOCATE(PLAN_IDENTIFIER(0:MAX_NUMBER_OF_PLANS_TO_SAVE), STAT=I4ST)|
00074 andecomp.f90                   (f90) 00214 |         ALLOCATE(PLAN_OBJECT_FUNCTION_VALUE(0:MAX_NUMBER_OF_PLANS_TO_SAVE), STAT=I4ST)|
00075 andecomp.f90                   (f90) 00217 |         ALLOCATE(PLAN_HAS_MUT_EXC_UNITS(0:MAX_NUMBER_OF_PLANS_TO_SAVE), STAT=I4ST)|
00076 andecomp.f90                   (f90) 00220 |         ALLOCATE(PLAN_OPTION_SEQUENCE(50, MAX_NUMBER_OF_PLANS_TO_SAVE), STAT=I4ST)|
00077 andecomp.f90                   (f90) 00258 |         ALLOCATE(OPTION_POINTR(ACTIVE_OPTIONS), STAT=I4ST)|
00078 andecomp.f90                   (f90) 00261 |         ALLOCATE(OPTION_ANN_UNITS(ACTIVE_OPTIONS), STAT=I4ST)|
00079 andecomp.f90                   (f90) 00264 |         ALLOCATE(AVAIL_ANNUAL_UNITS(ACTIVE_OPTIONS), STAT=I4ST)|
00080 andecomp.f90                   (f90) 00267 |         ALLOCATE(OPTION_AVAIL_UNITS(ACTIVE_OPTIONS), STAT=I4ST)|
00081 andecomp.f90                   (f90) 00270 |         ALLOCATE(AVAIL_CUM_UNITS(ACTIVE_OPTIONS), STAT=I4ST)|
00082 andecomp.f90                   (f90) 00273 |         ALLOCATE(OPTION_CAP(ACTIVE_OPTIONS), STAT=I4ST)|
00083 andecomp.f90                   (f90) 00276 |         ALLOCATE(OPTION_LOADING_TYPE(ACTIVE_OPTIONS), STAT=I4ST)|
00084 andecomp.f90                   (f90) 00279 |         ALLOCATE(OPTION_IS_ACTIVE(ACTIVE_OPTIONS), STAT=I4ST)|
00085 andecomp.f90                   (f90) 00282 |         ALLOCATE(PLAN_BEING_TESTED(50), STAT=I4ST)|
00086 andecomp.f90                   (f90) 00402 |               ALLOCATE(BAD_ODD_PLANS(OPTION_DEPTH,MAX_BAD_PLANS))|
00087 andecomp.f90                   (f90) 00471 |            ALLOCATE(BAD_EVEN_PLANS(OPTION_DEPTH,MAX_BAD_PLANS))|
00088 andecomp.f90                   (f90) 00501 |            ALLOCATE(DEPTH_BEST_PLANS(OPTION_DEPTH, DEPTH_PLANS_2_SAVE), STAT=I4ST)|
00089 andecomp.f90                   (f90) 00504 |            ALLOCATE(DEPTH_PLAN_OBJECTIVE_VALUE(DEPTH_PLANS_2_SAVE), STAT=I4ST)|
00090 andecomp.f90                   (f90) 00507 |            ALLOCATE(PLANS_2_TEST_AT_NEXT_DEPTH(OPTION_DEPTH, MAX_PLANS_2_TEST), STAT=I4ST)|
00091 andecomp.f90                   (f90) 00534 |                     ALLOCATE(BAD_ODD_PLANS(OPTION_DEPTH,MAX_BAD_PLANS))|
00092 andecomp.f90                   (f90) 00548 |                     ALLOCATE(   &|
00093 andecomp.f90                   (f90) 00571 |                     ALLOCATE(LIST_OF_COMBINATIONS(OPTION_DEPTH-1,   &|
00094 andecomp.f90                   (f90) 00598 |                     ALLOCATE(LIST_OF_COMBINATIONS(OPTION_DEPTH-1,   &|
00095 andecomp.f90                   (f90) 00630 |                  ALLOCATE(DEPTH_BEST_PLANS(OPTION_DEPTH, DEPTH_PLANS_2_SAVE), STAT=I4ST)|
00096 andecomp.f90                   (f90) 00633 |                  ALLOCATE(DEPTH_PLAN_OBJECTIVE_VALUE(DEPTH_PLANS_2_SAVE), STAT=I4ST)|
00097 andecomp.f90                   (f90) 00636 |                  ALLOCATE(PLANS_2_TEST_AT_NEXT_DEPTH(OPTION_DEPTH, MAX_PLANS_2_TEST), STAT=I4ST)|
00098 andecomp.f90                   (f90) 00703 |                           ALLOCATE(TEMP_BAD_ARRAY(OPTION_DEPTH, MAX_BAD_PLANS), STAT=I4ST)|
00099 andecomp.f90                   (f90) 00716 |                              ALLOCATE(BAD_ODD_PLANS(OPTION_DEPTH,   &|
00100 andecomp.f90                   (f90) 00732 |                              ALLOCATE(BAD_EVEN_PLANS(OPTION_DEPTH,   &|
00101 andecomp.f90                   (f90) 00968 |            ALLOCATE(SORTED_POINTER(NUMBER_OF_SAVED_PLANS))|
00102 andecomp.f90                   (f90) 01015 |            ALLOCATE(POSITION_MATCHED(OPTION_DEPTH-1))|
00103 andecomp.f90                   (f90) 01056 |            ALLOCATE(POSITION_MATCHED(OPTION_DEPTH-1))|
00104 andecomp.f90                   (f90) 01336 |      ALLOCATE(ACTIVE_OPTION_LIST(TOTAL_OPTIONS))|
00105 andecomp.f90                   (f90) 01345 |         ALLOCATE(COMBINATION_ARRAYS(DEPTH))|
00106 cap_objt.f90                   (f90) 00233 |      ALLOCATE(DSM_CAP(3,STUDY_PERIOD))|
00107 cap_objt.f90                   (f90) 00287 |         ALLOCATE(PLAN_IDENTIFIER(0:MAX_NUMBER_OF_PLANS_TO_SAVE), &|
00108 cap_objt.f90                   (f90) 00355 |            ALLOCATE(ACTIVE_OPTIONS(OPTION_DEPTH))|
00109 cap_objt.f90                   (f90) 00514 |      ALLOCATE(DSM_CAP(3,STUDY_PERIOD))|
00110 cap_objt.f90                   (f90) 00936 |         ALLOCATE(COMMITTED_OPTION_POINTERS(MAX_COMMITTED_OPTIONS), &|
00111 cap_objt.f90                   (f90) 00985 |      ALLOCATE(DSM_CAP(3,STUDY_PERIOD))|
00112 cap_objt.f90                   (f90) 00998 |         ALLOCATE(DELAYED_ON_LINE_DATE(NEXT_COMMITTED_OPTION))|
00113 cap_objt.f90                   (f90) 01516 |         ALLOCATE(TEMP_I2_ARRAY(NEXT_COMMITTED_OPTION))|
00114 cap_objt.f90                   (f90) 01520 |         ALLOCATE(COMMITTED_OPTION_POINTERS(MAX_COMMITTED_OPTIONS))|
00115 cap_objt.f90                   (f90) 01527 |         ALLOCATE(COMMITTED_OPTION_TIMES_DELAYED(MAX_COMMITTED_OPTIONS))|
00116 cap_objt.f90                   (f90) 01535 |         ALLOCATE(COMMITTED_OPTION_START_YEAR(MAX_COMMITTED_OPTIONS))|
00117 cap_objt.f90                   (f90) 01543 |         ALLOCATE(COMMITTED_OPTION_ON_LINE_YR(MAX_COMMITTED_OPTIONS))|
00118 cap_objt.f90                   (f90) 01551 |         ALLOCATE( &|
00119 cap_objt.f90                   (f90) 01560 |         ALLOCATE(REC_LOCATOR(MAX_COMMITTED_OPTIONS))|
00120 cap_objt.f90                   (f90) 01567 |         ALLOCATE(TEMP_R4_ARRAY(NEXT_COMMITTED_OPTION))|
00121 cap_objt.f90                   (f90) 01571 |         ALLOCATE(OPTION_PERCENT_COMPLETED(MAX_COMMITTED_OPTIONS))|
00122 cap_objt.f90                   (f90) 01577 |         ALLOCATE(TEMP_L1_ARRAY(NEXT_COMMITTED_OPTION))|
00123 cap_objt.f90                   (f90) 01581 |         ALLOCATE(HAS_BEEN_ACCELERATED(MAX_COMMITTED_OPTIONS))|
00124 cap_objt.f90                   (f90) 01592 |         ALLOCATE(TEMP_I2_ARRAY(NEXT_PLANNED_OPTION))|
00125 cap_objt.f90                   (f90) 01597 |         ALLOCATE(PLANNED_OPTION_POINTERS(MAX_COMMITTED_OPTIONS))|
00126 cap_objt.f90                   (f90) 01604 |         ALLOCATE(PLANNED_OPTION_START_YEAR(MAX_COMMITTED_OPTIONS))|
00127 cap_objt.f90                   (f90) 01611 |         ALLOCATE(PLANNED_OPTION_ON_LINE_YR(MAX_COMMITTED_OPTIONS))|
00128 cap_objt.f90                   (f90) 01717 |            ALLOCATE(RECLN(2))|
00129 cap_objt.f90                   (f90) 01738 |            ALLOCATE(RECLN(200))|
00130 cap_objt.f90                   (f90) 01782 |         ALLOCATE(OVER_COST_LEVELS(MAX_OVER_TABLE_ROWS, &|
00131 cap_objt.f90                   (f90) 01798 |         ALLOCATE(UNDER_COST_LEVELS(MAX_UNDER_TABLE_ROWS, &|
00132 cap_objt.f90                   (f90) 02969 |        ALLOCATE(LAST_RESOURCE_ADDED(MAX(INT(1,2),UPPER_TRANS_GROUP)), &|
00133 cap_objt.f90                   (f90) 02995 |            ALLOCATE(OPTION_NAME(TOTAL_ALL_OPTIONS), &|
00134 cap_objt.f90                   (f90) 03000 |            ALLOCATE(ResourceIDtoOptionPosPrt(MaxProductionProinter))|
00135 cap_objt.f90                   (f90) 03009 |         ALLOCATE(SCREEN_SLOPE(TOTAL_ALL_OPTIONS), &|
00136 cap_objt.f90                   (f90) 03277 |         ALLOCATE(SCREEN_SLOPE(TOTAL_ALL_OPTIONS), &|
00137 cap_objt.f90                   (f90) 04004 |         ALLOCATE(MULTI_AREA_NAME(0:UPPER_TRANS_GROUP), &|
00138 cap_objt.f90                   (f90) 06356 |            ALLOCATE(MRX_RPS_MX_CURVE_MARGIN(HO,PA), &|
00139 cap_objt.f90                   (f90) 10079 |         ALLOCATE(LAST_RESOURCE_ADDED(MAX(INT(1,2),UPPER_TRANS_GROUP)))|
00140 cap_objt.f90                   (f90) 10087 |            ALLOCATE(OPTION_NAME(TOTAL_ALL_OPTIONS), &|
00141 cap_objt.f90                   (f90) 10212 |         ALLOCATE(SCREEN_SLOPE(TOTAL_ALL_OPTIONS), &|
00142 cap_objt.f90                   (f90) 10645 |         ALLOCATE(MULTI_AREA_NAME(0:UPPER_TRANS_GROUP), &|
00143 cap_objt.f90                   (f90) 11817 |         ALLOCATE(CO2_EMISSIONS_CAP(0:UPPER_TRANS_GROUP), &|
00144 capacity_arrays.f90            (f90) 00064 |    ALLOCATE(RPS_HYDRO_DB(2,400,NUMPMS,0:12))|
00145 capacity_arrays.f90            (f90) 00078 |    allocate(QUALIFYING_GEN_DB(ACTIVE_RPS,0:NUMPMS,2))|
00146 capacity_arrays.f90            (f90) 00079 |	allocate(GRX_QUAL_GEN_DB(ACTIVE_RPS,0:NUMPMS,2))|
00147 capacity_arrays.f90            (f90) 00080 |	allocate(SAVE_GRX_QUAL_GEN_DB(ACTIVE_RPS,0:NUMPMS,2))|
00148 capacity_arrays.f90            (f90) 00090 |     ALLOCATE(DEV_TG_CAP(0:16,0:MAX(1,nr_trans_classes), &|
00149 capacity_arrays.f90            (f90) 00096 |     ALLOCATE (DEV_NEW_TG_CAP(0:21,0:MAX(1,nr_trans_classes), &|
00150 capacity_arrays.f90            (f90) 00101 |     allocate(SAVED_DEV_NEW_TG_CAP(0:21,0:MAX(1, &|
00151 cat2objt.f90                   (f90) 00102 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
00152 cat2objt.f90                   (f90) 00173 |            ALLOCATE(BC_ASSET_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))|
00153 cat2objt.f90                   (f90) 00209 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
00154 cat2objt.f90                   (f90) 00293 |         ALLOCATE(OL_ASSET_CLASS_POINTER(MAX_OL_CLASS_ID_NUM))|
00155 cat2objt.f90                   (f90) 00475 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
00156 cat2objt.f90                   (f90) 00488 |         ALLOCATE(CATAWBA_REVENUES(0:NUM_OF_ASSET_CLASSES, &|
00157 cat2objt.f90                   (f90) 00513 |         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &|
00158 cat2objt.f90                   (f90) 00908 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
00159 cat2objt.f90                   (f90) 00913 |         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &|
00160 cat2objt.f90                   (f90) 02081 |      ALLOCATE(PA_HOURLY_LOADS(HOURS_INCREMENT), &|
00161 cat2objt.f90                   (f90) 02100 |         ALLOCATE(PA_UNIT_INDEX(NO_PA_RESOURCES), &|
00162 cat2objt.f90                   (f90) 02116 |            ALLOCATE(ANN_PA_ENERGY(0:NO_PA_RESOURCES+1), &|
00163 cat2objt.f90                   (f90) 04452 |         ALLOCATE(MONTHLY_PA_LOADS(LOCAL_HOURS))|
00164 cf_objt.f90                    (f90) 00737 |         ALLOCATE(SUM_CLASS_PEAKS(12,2,FORECAST_GROWTH_YEARS, &|
00165 cf_objt.f90                    (f90) 00742 |         ALLOCATE(CLASS_ANN_COIN_PEAK(MAX_LOAD_CLASSES, &|
00166 cf_objt.f90                    (f90) 00843 |            ALLOCATE(SUM_CLASS_PEAKS(12,2,FORECAST_GROWTH_YEARS,1))|
00167 cl_screen_data.f90             (f90) 00249 |         ALLOCATE(S_FUELMX(CL_OPTIONS))|
00168 cl_screen_data.f90             (f90) 00250 |         ALLOCATE(S_PBTUCT(CL_OPTIONS))|
00169 cl_screen_data.f90             (f90) 00251 |         ALLOCATE(S_PFESCR(CL_OPTIONS))|
00170 cl_screen_data.f90             (f90) 00252 |         ALLOCATE(S_SBTUCT(CL_OPTIONS))|
00171 cl_screen_data.f90             (f90) 00253 |         ALLOCATE(S_RPS_PROGRAM_NUMBER(CL_OPTIONS))|
00172 cl_screen_data.f90             (f90) 00254 |         ALLOCATE(S_RPS_PERCENT(CL_OPTIONS))|
00173 cl_screen_data.f90             (f90) 00255 |         ALLOCATE(S_SFESCR(CL_OPTIONS))|
00174 cl_screen_data.f90             (f90) 00256 |         ALLOCATE(S_FUELADJ_IN(CL_OPTIONS))|
00175 cl_screen_data.f90             (f90) 00257 |         ALLOCATE(S_MNRATE(CL_OPTIONS,12))|
00176 cl_screen_data.f90             (f90) 00258 |         ALLOCATE(S_INTER_BLOCKS(CL_OPTIONS,3))|
00177 cl_screen_data.f90             (f90) 00259 |         ALLOCATE(S_FUELADJ(CL_OPTIONS))|
00178 cl_screen_data.f90             (f90) 00260 |         ALLOCATE(S_EFOR(CL_OPTIONS))|
00179 cl_screen_data.f90             (f90) 00261 |         ALLOCATE(S_VCPMWH_IN(CL_OPTIONS))|
00180 cl_screen_data.f90             (f90) 00262 |         ALLOCATE(S_OMESCR(CL_OPTIONS))|
00181 cl_screen_data.f90             (f90) 00263 |         ALLOCATE(S_FIXED_COST_IN(CL_OPTIONS))|
00182 cl_screen_data.f90             (f90) 00264 |         ALLOCATE(S_FIXED_COST_ESCALATOR(CL_OPTIONS))|
00183 cl_screen_data.f90             (f90) 00265 |         ALLOCATE(S_MONTHLY_CAPACITY_POINTER(CL_OPTIONS))|
00184 cl_screen_data.f90             (f90) 00266 |         ALLOCATE(S_HR_FACTOR(CL_OPTIONS))|
00185 cl_screen_data.f90             (f90) 00267 |         ALLOCATE(S_INPUT_MW(2,CL_OPTIONS))|
00186 cl_screen_data.f90             (f90) 00268 |         ALLOCATE(S_CAP_PLANNING_FAC(CL_OPTIONS))|
00187 cl_screen_data.f90             (f90) 00269 |         ALLOCATE(S_COEFF(3,CL_OPTIONS),P_SO2(CL_OPTIONS),P_NOX(CL_OPTIONS),P_PARTICULATES(CL_OPTIONS),P_EMIS_OTH2(CL_OPTIONS), &|
00188 cl_screen_data.f90             (f90) 00271 |         ALLOCATE(AHR(CL_OPTIONS))|
00189 cl_screen_data.f90             (f90) 00272 |         ALLOCATE(PROD_PNTR_BY_CL_UNITS(PROD_PNTR))|
00190 cl_screen_data.f90             (f90) 00273 |         ALLOCATE(S_UNITNM(CL_OPTIONS))|
00191 cl_screen_data.f90             (f90) 00274 |         ALLOCATE(LDTYPE(CL_OPTIONS))|
00192 cl_screen_data.f90             (f90) 00275 |         ALLOCATE(S_ANNUAL_CL_FIXED_COST(CL_OPTIONS))|
00193 cl_screen_data.f90             (f90) 00276 |         ALLOCATE(S_TRANSACTION_GROUP_ID(CL_OPTIONS))|
00194 cl_screen_data.f90             (f90) 00277 |         ALLOCATE(S_EMISSION_MARKET_LINK(CL_OPTIONS))|
00195 cl_screen_data.f90             (f90) 00278 |         ALLOCATE(S_ANNUAL_CL_FIXED_COST_ESC(CL_OPTIONS))|
00196 cl_screen_data.f90             (f90) 00279 |         ALLOCATE(P_FUEL_DELIVERY(CL_OPTIONS))|
00197 cl_screen_data.f90             (f90) 00280 |         ALLOCATE(START_UP_COSTS(CL_OPTIONS))|
00198 cl_screen_data.f90             (f90) 00281 |         ALLOCATE(START_UP_COSTS_ESCALATION(CL_OPTIONS))|
00199 cl_screen_data.f90             (f90) 00282 |         ALLOCATE(MIN_DOWN_TIME(CL_OPTIONS))|
00200 cl_screen_data.f90             (f90) 00283 |         ALLOCATE(MIN_UP_TIME(CL_OPTIONS))|
00201 cl_screen_data.f90             (f90) 00284 |         ALLOCATE(P_FUEL_DELIVERY_2(CL_OPTIONS))|
00202 cl_screen_data.f90             (f90) 00285 |         ALLOCATE(P_FUEL_DELIVERY_3(CL_OPTIONS))|
00203 cl_screen_data.f90             (f90) 00286 |         ALLOCATE(S_FUEL_DELIVERY(CL_OPTIONS))|
00204 cl_screen_data.f90             (f90) 00287 |         ALLOCATE(S_FUEL_DELIVERY_2(CL_OPTIONS))|
00205 cl_screen_data.f90             (f90) 00288 |         ALLOCATE(s_fuel_delivery_3(CL_OPTIONS))|
00206 cl_screen_data.f90             (f90) 00289 |         ALLOCATE(APPLY_NOX_SEASON_DATE(CL_OPTIONS),S_PRIMARY_MOVER(CL_OPTIONS),S_UNIT_GAS_REGION_INDEX(CL_OPTIONS), &|
00207 cla_objt.for                   (for) 02996 |      ALLOCATE(TG_2_PLANNING_AREA(0:MAX(1,UPPER_TRANS_GROUP)))|
00208 cla_objt.for                   (for) 03000 |      ALLOCATE(DAY_TYPE_TRANS_GROUP_PAIR(MAX_DAY_TYPES,|
00209 cla_objt.for                   (for) 03002 |      ALLOCATE(DATA_BASE_POSITION(2*MAX_DAY_TYPES,MAX_CL_UNITS))|
00210 cla_objt.for                   (for) 03004 |      ALLOCATE(MAINT_DAYS(MAX_CL_UNITS))|
00211 cla_objt.for                   (for) 03011 |      ALLOCATE(CL_ANN_CAP(3,STUDY_PERIOD,2))|
00212 cla_objt.for                   (for) 03012 |      ALLOCATE(CL_TG_CAP(0:6,0:MAX(1,UPPER_TRANS_GROUP),STUDY_PERIOD,2))|
00213 cla_objt.for                   (for) 03013 |      ALLOCATE(NEWGEN_CAP_BY_INDEX(MAX_NEWGEN_INDEX,|
00214 cla_objt.for                   (for) 03016 |      ALLOCATE(CL_TG_AFTER_PEAK(0:UPPER_TRANS_GROUP,STUDY_PERIOD))|
00215 cla_objt.for                   (for) 03017 |      ALLOCATE(CL_TG_CAP_MARKET_MW(0:UPPER_TRANS_GROUP),|
00216 cla_objt.for                   (for) 03019 |      ALLOCATE(CL_TG_RETIRE(0:UPPER_TRANS_GROUP,STUDY_PERIOD))|
00217 cla_objt.for                   (for) 03022 |      ALLOCATE(CL_ANNUAL_LOAD_REDUCTION(STUDY_PERIOD))|
00218 cla_objt.for                   (for) 04528 |      ALLOCATE(RETIRE_RETRO_CO2_PRICE(RETIRE_RETRO_COUNTER),|
00219 cla_objt.for                   (for) 05427 |         ALLOCATE(MRX_RPS_CL_CURVE_MWH(NUNITS,PG),|
00220 cla_objt.for                   (for) 06195 |         ALLOCATE(TRANS_GROUP_FOR_DATA_BASE(MAX_TRANS_DATA_BASES))|
00221 cla_objt.for                   (for) 08049 |         ALLOCATE(ANNUAL_CL_CAPACITY(2,NUNITS,12))|
00222 cla_objt.for                   (for) 08050 |      ALLOCATE(ANNUAL_CL_MAINTENANCE(NUNITS,13))|
00223 cla_objt.for                   (for) 08051 |      ALLOCATE(ANNUAL_CL_MAINT_MW(NUNITS))|
00224 cla_objt.for                   (for) 08052 |      ALLOCATE(MAINT_DIVISIBLE(NUNITS))|
00225 cla_objt.for                   (for) 08058 |         ALLOCATE(MAINT_INDEX(NUNITS))|
00226 cla_objt.for                   (for) 08059 |         ALLOCATE(MAINT_INDEX_MW(NUNITS))|
00227 cla_objt.for                   (for) 08060 |         ALLOCATE(MAINT_INDEX_DAYS(NUNITS))|
00228 cla_objt.for                   (for) 08061 |         ALLOCATE(MAINT_INDEX_MONTHLY_MW(NUNITS,13))|
00229 cla_objt.for                   (for) 08065 |         ALLOCATE(MAINTENANCE_DAYS(NUNITS))|
00230 cla_objt.for                   (for) 08078 |         ALLOCATE(MARKET_RESOURCE_INDEX(NUNITS))|
00231 cla_objt.for                   (for) 08079 |         ALLOCATE(MARKET_RESOURCE_COUNTER(NUNITS))|
00232 cla_objt.for                   (for) 08937 |            ALLOCATE(MON_MDS_CL_UNIT_EMISSIONS(|
00233 cla_objt.for                   (for) 08939 |            ALLOCATE(MON_MDS_CL_UNIT_EMISSIONS_COST(|
00234 cla_objt.for                   (for) 08941 |            ALLOCATE(MON_MDS_CL_UNIT_CAPACITY(R_NUNITS,0:12))|
00235 cla_objt.for                   (for) 08942 |            ALLOCATE(MON_MDS_CL_UNIT_ENERGY(R_NUNITS,0:12))|
00236 cla_objt.for                   (for) 08943 |            ALLOCATE(MON_MDS_CL_UNIT_FUEL_COST(R_NUNITS,0:12))|
00237 cla_objt.for                   (for) 08944 |            ALLOCATE(MON_MDS_CL_UNIT_VAR_COST(R_NUNITS,0:12))|
00238 cla_objt.for                   (for) 08945 |            ALLOCATE(MON_MDS_CL_UNIT_FIXED_COST(R_NUNITS,0:12))|
00239 cla_objt.for                   (for) 08946 |            ALLOCATE(MON_MDS_NUC_FUEL_ADDER_COST(R_NUNITS,0:12))|
00240 cla_objt.for                   (for) 08947 |            ALLOCATE(MON_MDS_ECO_SALES_REV_FROM(R_NUNITS,0:12))|
00241 cla_objt.for                   (for) 08948 |            ALLOCATE(MON_MDS_ECO_SALES_ENRG_FROM(R_NUNITS,0:12))|
00242 cla_objt.for                   (for) 08949 |            ALLOCATE(MON_MDS_ECO_PUCH_COST_FROM(R_NUNITS,0:12))|
00243 cla_objt.for                   (for) 08950 |            ALLOCATE(MON_MDS_ECO_PUCH_ENRG_FROM(R_NUNITS,0:12))|
00244 cla_objt.for                   (for) 08951 |            ALLOCATE(MON_MDS_ICAP_REVENUES(R_NUNITS,0:12))|
00245 cla_objt.for                   (for) 08952 |            ALLOCATE(MON_MDS_CL_UNIT_MMBTUS(R_NUNITS,0:12))|
00246 cla_objt.for                   (for) 09124 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_CAP_LIMITED_CLASS_ID_NUM))|
00247 cla_objt.for                   (for) 09181 |         ALLOCATE(|
00248 cla_objt.for                   (for) 09183 |         ALLOCATE(CL_ANN_MULT_FUEL_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES,|
00249 cla_objt.for                   (for) 09185 |         ALLOCATE(FE_ANN_MULT_FUEL_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES,|
00250 cla_objt.for                   (for) 09187 |         ALLOCATE(NF_FUEL_LEASED_BY_CLASS(|
00251 cla_objt.for                   (for) 09189 |         ALLOCATE(NF_FUEL_OWNED_BY_CLASS(|
00252 cla_objt.for                   (for) 09191 |         ALLOCATE(CL_ANN_CLASS_VAR_COST(|
00253 cla_objt.for                   (for) 09193 |         ALLOCATE(CL_ANN_CLASS_FIXED_COST(|
00254 cla_objt.for                   (for) 09195 |         ALLOCATE(CL_ANN_CLASS_CAPACITY(|
00255 cla_objt.for                   (for) 09197 |         ALLOCATE(CL_ANN_CLASS_ENERGY(|
00256 cla_objt.for                   (for) 09199 |         ALLOCATE(CL_ANN_CLASS_REVENUE(|
00257 cla_objt.for                   (for) 09201 |         ALLOCATE(CL_ANN_CLASS_PURCHASES(|
00258 cla_objt.for                   (for) 09203 |         ALLOCATE(CL_ANN_CLASS_EMISSIONS(NUMBER_OF_EMISSION_TYPES,|
00259 cla_objt.for                   (for) 09205 |         ALLOCATE(CL_ANN_MULT_VAR_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES,|
00260 cla_objt.for                   (for) 09207 |         ALLOCATE(|
00261 cla_objt.for                   (for) 09210 |         ALLOCATE(CL_ANN_MULT_REVENUE(0:NUMBER_OF_CAP_LIMITED_CLASSES,|
00262 cla_objt.for                   (for) 09212 |         ALLOCATE(CL_ANN_MULT_CAPACITY(0:NUMBER_OF_CAP_LIMITED_CLASSES,|
00263 cla_objt.for                   (for) 09214 |         ALLOCATE(CL_ANN_MULT_ENERGY(0:NUMBER_OF_CAP_LIMITED_CLASSES,|
00264 cla_objt.for                   (for) 09216 |         ALLOCATE(CL_ANN_MULT_PURCHASES(0:|
00265 cla_objt.for                   (for) 09219 |         ALLOCATE(NUC_FUEL_LEASED_BURN_BY_CLASS(|
00266 cla_objt.for                   (for) 09221 |         ALLOCATE(NUC_FUEL_OWNED_BURN_BY_CLASS(|
00267 cla_objt.for                   (for) 09223 |         ALLOCATE(NUCLEAR_MWH_BY_CLASS(0:NUMBER_OF_CAP_LIMITED_CLASSES))|
00268 cla_objt.for                   (for) 09224 |         ALLOCATE(NUCLEAR_MMBTU_BY_CLASS(|
00269 cla_objt.for                   (for) 09226 |         ALLOCATE(INTRA_COMPANY_REVENUES(0:1024,0:3))|
00270 cla_objt.for                   (for) 09227 |         ALLOCATE(INTRA_COMPANY_NF_BURN(0:1024))|
00271 cla_objt.for                   (for) 09228 |         ALLOCATE(MON_MDS_CL_CLASS_FUEL_COST|
00272 cla_objt.for                   (for) 09230 |         ALLOCATE(MON_MDS_CL_MULT_FUEL_COST|
00273 cla_objt.for                   (for) 09233 |         ALLOCATE(MON_MDS_CL_MULT_VAR_COST|
00274 cla_objt.for                   (for) 09236 |         ALLOCATE(MON_MDS_CL_MULT_FIXED_COST|
00275 cla_objt.for                   (for) 09239 |         ALLOCATE(MON_MDS_CL_MULT_CAPACITY|
00276 cla_objt.for                   (for) 09242 |         ALLOCATE(MON_MDS_CL_MULT_ENERGY|
00277 cla_objt.for                   (for) 09245 |         ALLOCATE(MON_MDS_CL_MULT_PURCHASES|
00278 cla_objt.for                   (for) 09248 |         ALLOCATE(MON_MDS_CL_CLASS_VAR_COST|
00279 cla_objt.for                   (for) 09250 |         ALLOCATE(MON_MDS_CL_CLASS_FIXED_COST|
00280 cla_objt.for                   (for) 09252 |         ALLOCATE(MON_MDS_CL_CLASS_REVENUE|
00281 cla_objt.for                   (for) 09254 |         ALLOCATE(MON_MDS_CL_CAP_REVENUE|
00282 cla_objt.for                   (for) 09256 |         ALLOCATE(MON_MDS_CL_CLASS_CAPACITY|
00283 cla_objt.for                   (for) 09258 |         ALLOCATE(MON_MDS_CL_CLASS_PURCHASES|
00284 cla_objt.for                   (for) 09260 |         ALLOCATE(MON_MDS_CL_CAP_PURCHASES|
00285 cla_objt.for                   (for) 09262 |         ALLOCATE(MON_MDS_CL_CLASS_ENERGY|
00286 cla_objt.for                   (for) 09264 |         ALLOCATE(MON_MDS_CL_CLASS_EMISSIONS(NUMBER_OF_EMISSION_TYPES,|
00287 cla_objt.for                   (for) 09266 |         ALLOCATE(MON_MDS_CL_CLASS_EMISSIONS_COST(|
00288 cla_objt.for                   (for) 09270 |         ALLOCATE(MON_MDS_INTRA_COMPANY_REVENUES(0:1024,0:3,0:12))|
00289 cla_objt.for                   (for) 09271 |         ALLOCATE(MON_MDS_INTRA_COMPANY_NF_BURN(0:1024,0:12))|
00290 cla_objt.for                   (for) 09272 |         ALLOCATE(MON_MDS_NF_FUEL_LEASED_BC|
00291 cla_objt.for                   (for) 09274 |         ALLOCATE(MON_MDS_NF_FUEL_OWNED_BC|
00292 cla_objt.for                   (for) 09276 |         ALLOCATE(MON_MDS_NUC_FUEL_LEASE_BURN_BC|
00293 cla_objt.for                   (for) 09278 |         ALLOCATE(MON_MDS_NUC_FUEL_OWNED_BURN_BC|
00294 cla_objt.for                   (for) 09280 |         ALLOCATE(MON_MDS_NUCLEAR_MWH_BY_CLASS|
00295 cla_objt.for                   (for) 09282 |         ALLOCATE(MON_MDS_NUCLEAR_MMBTU_BY_CLASS|
00296 cla_objt.for                   (for) 09284 |         ALLOCATE(MON_MDS_ICAP_REV_BY_CLASS|
00297 cla_objt.for                   (for) 09286 |         ALLOCATE(MONTHLY_AC_WHOLESALE_PROD_COST|
00298 cla_objt.for                   (for) 09288 |         ALLOCATE(MONTHLY_AC_ECITY_VAR_PROD_COST|
00299 cla_objt.for                   (for) 09290 |         ALLOCATE(MONTHLY_AC_ECITY_NEW_FIX_COST|
00300 cla_objt.for                   (for) 09299 |            ALLOCATE(PURCHASE_ASSET_CLASS_ID(0:UPPER_TRANS_GROUP))|
00301 cla_objt.for                   (for) 09300 |            ALLOCATE(PURCHASE_POWER_ASSIGN(0:UPPER_TRANS_GROUP))|
00302 cla_objt.for                   (for) 09361 |         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))|
00303 cla_objt.for                   (for) 09362 |         ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))|
00304 cla_objt.for                   (for) 10561 |         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))|
00305 cla_objt.for                   (for) 10562 |         ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))|
00306 cla_objt_arrays.f90            (f90) 00170 |            ALLOCATE(cl_tg_cap(0:max_technology_counters, 0:MaxVariable, STUDY_PERIOD,2))|
00307 clm_objt.f90                   (f90) 00255 |      ALLOCATE(MAINT_RQ_DA_DEC(N_ACTIVE_UNITS),STAT=I4ST)|
00308 clm_objt.f90                   (f90) 00258 |      ALLOCATE(MAINT_WEIGHT(N_ACTIVE_UNITS),STAT=I4ST)|
00309 clm_objt.f90                   (f90) 00261 |      ALLOCATE(ORDER_DEC(N_ACTIVE_UNITS),STAT=I4ST)|
00310 clm_objt.f90                   (f90) 00264 |      ALLOCATE(OPT_RANGE_OFS(N_ACTIVE_UNITS),STAT=I4ST)|
00311 clm_objt.f90                   (f90) 00267 |      ALLOCATE(OPT_RANGE_DUR(N_ACTIVE_UNITS),STAT=I4ST)|
00312 clm_objt.f90                   (f90) 00598 |            ALLOCATE(HrsToState(0:1,12*MAX_FO_PER_MONTH),STAT=I4ST)|
00313 clm_objt.f90                   (f90) 00687 |      ALLOCATE(   &|
00314 clm_objt.f90                   (f90) 00699 |      ALLOCATE(SO_BEG_MODA(2,NUNITS),STAT=I4ST)|
00315 clm_objt.f90                   (f90) 00703 |      ALLOCATE(SO_END_MODA(2,NUNITS),STAT=I4ST)|
00316 clm_objt.f90                   (f90) 00707 |      ALLOCATE(FO_BEG_HR(MAX_FO_PER_MONTH,12,NUNITS),STAT=I4ST)|
00317 clm_objt.f90                   (f90) 00711 |      ALLOCATE(FO_END_HR(MAX_FO_PER_MONTH,12,NUNITS),STAT=I4ST)|
00318 clm_objt.f90                   (f90) 00715 |      ALLOCATE(PD_BEG_HR(MAX_PD_PER_MONTH,12,NUNITS),STAT=I4ST)|
00319 clm_objt.f90                   (f90) 00719 |      ALLOCATE(PD_END_HR(MAX_PD_PER_MONTH,12,NUNITS),STAT=I4ST)|
00320 clreport.for                   (for) 01169 |         ALLOCATE(|
00321 clreport.for                   (for) 01334 |            ALLOCATE(|
00322 clreport.for                   (for) 01430 |      ALLOCATE(MONTHLY_UNIT_EMISSIONS_COST(5,NUNITS))|
00323 clreport.for                   (for) 01650 |         ALLOCATE(ICAP_TOTAL_CAPACITY(CG),|
00324 clreport.for                   (for) 01723 |            ALLOCATE(|
00325 cls_runs.f90                   (f90) 00225 |         ALLOCATE(BC_TABLE_POSITION_FOR(0:MAX_BC_CLASS_NUMBER))|
00326 cls_runs.f90                   (f90) 00383 |         ALLOCATE(OL_TABLE_POSITION_FOR(0:MAX_OL_CLASS_NUMBER))|
00327 cls_runs.f90                   (f90) 00523 |            ALLOCATE(   &|
00328 cn_objt.f90                    (f90) 00120 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
00329 cn_objt.f90                    (f90) 00251 |            ALLOCATE(OPTIONS_BC_ASSET_CLASS_POINTER (MAX_BC_CLASS_ID_NUM))|
00330 cn_objt.f90                    (f90) 00293 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
00331 cn_objt.f90                    (f90) 00428 |         ALLOCATE(OPTIONS_OL_ASSET_CLASS_POINTER (MAX_OL_CLASS_ID_NUM))|
00332 cn_objt.f90                    (f90) 00822 |         allocate(ESCALATION_VECTOR(FOR_ALL_OPTIONS), STAT=astat)|
00333 cn_objt.f90                    (f90) 00831 |         ALLOCATE(unit_names(FOR_ALL_OPTIONS),LOADING_TYPE(FOR_ALL_OPTIONS),FILE_SOURCE(FOR_ALL_OPTIONS), &|
00334 cn_objt.f90                    (f90) 00853 |         ALLOCATE(THRESHOLD_CAPACITY(FOR_ALL_OPTIONS, &|
00335 cn_objt.f90                    (f90) 00862 |         IF(BASE_OPTIONS > 0) ALLOCATE(BASE_OPTION_LIST(BASE_OPTIONS))|
00336 cn_objt.f90                    (f90) 00863 |         IF(CYCLE_OPTIONS > 0) ALLOCATE(CYCL_OPTION_LIST(CYCLE_OPTIONS))|
00337 cn_objt.f90                    (f90) 00864 |         IF(PEAK_OPTIONS > 0) ALLOCATE(PEAK_OPTION_LIST(PEAK_OPTIONS))|
00338 cn_objt.f90                    (f90) 00865 |         IF(PEAK_REDUC_OPTIONS > 0) ALLOCATE(LOAD_OPTION_LIST(PEAK_REDUC_OPTIONS))|
00339 cn_objt.f90                    (f90) 00866 |         IF(FILL_OPTIONS > 0) ALLOCATE(FILL_OPTION_LIST(FILL_OPTIONS))|
00340 cn_objt.f90                    (f90) 00867 |         IF(TOTAL_ACTIVE_OPTIONS > 0) ALLOCATE(ACTIVE_OPTION_LIST(TOTAL_ACTIVE_OPTIONS))|
00341 cn_objt.f90                    (f90) 00869 |            ALLOCATE(HARD_WIRED_LIST(HARD_WIRED_OPTIONS),HARD_WIRED_ON_LINE_YEAR(HARD_WIRED_OPTIONS))|
00342 cn_objt.f90                    (f90) 00872 |         ALLOCATE(SAVE_NO_CL_ADDITIONS(MAX_SIMULATION_YEARS))|
00343 cn_objt.f90                    (f90) 00875 |         ALLOCATE(SAVE_NO_EL_ADDITIONS(MAX_SIMULATION_YEARS))|
00344 cn_objt.f90                    (f90) 01175 |            ALLOCATE(ADDED_LM_PROGRAMS(DSM_OPTIONS))|
00345 cn_objt.f90                    (f90) 01181 |            ALLOCATE(ADDED_CONTRACT_PROGRAMS(CONTRACT_OPTIONS))|
00346 cn_objt.f90                    (f90) 02246 |            ALLOCATE(TARGETS(3,STUDY_PERIOD), &|
00347 cn_objt.f90                    (f90) 02722 |            ALLOCATE(TARGETS(3,STUDY_PERIOD), &|
00348 co_objt.for                    (for) 00051 |         ALLOCATE(DEMAND_ALLOCATOR(AVAIL_DATA_YEARS),|
00349 co_objt.for                    (for) 00270 |      ALLOCATE(CLASS_MWH(SYSTEM_CLASS_NUM))|
00350 co_objt.for                    (for) 00271 |      ALLOCATE(CLASS_COSTS_TOTAL(SYSTEM_CLASS_NUM))|
00351 co_objt.for                    (for) 00272 |      ALLOCATE(CLASS_RATES(SYSTEM_CLASS_NUM))|
00352 coal_main_prg.f90              (f90) 01475 |            ALLOCATE (Supply_Production_By_Year(20,nArray,0:30), &|
00353 coal_main_prg.f90              (f90) 01655 |            ALLOCATE(GenericTransportPtrToTG(0:256,20)) ! TG,1 = paths TG,>1 pointer|
00354 coal_main_prg.f90              (f90) 01737 |               ALLOCATE(GenericPlantPtrByTG(0:256))|
00355 cpl_objt.f90                   (f90) 00134 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
00356 cpl_objt.f90                   (f90) 00236 |            ALLOCATE(BC_ASSET_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))|
00357 cpl_objt.f90                   (f90) 00276 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
00358 cpl_objt.f90                   (f90) 00405 |         ALLOCATE(OL_ASSET_CLASS_POINTER(MAX_OL_CLASS_ID_NUM))|
00359 ct_objt.f90                    (f90) 00438 |      ALLOCATE(CONTRACT_ADJUSTMENTS_TO_PEAK(STUDY_PERIOD), &|
00360 ct_objt.f90                    (f90) 00634 |      ALLOCATE(CONTRACT_BUY_CAPACITY(0:MAX_CONTRACT_CLASS_ID_NUM), &|
00361 dr_booth.f90                   (f90) 00534 |      ALLOCATE(AVAILABLE_SHADOW_CAPACITY(MAX(NUNITS,INT(1,2))))|
00362 dr_booth.f90                   (f90) 01285 |            ALLOCATE(CL_LOSSES(NUNITS))|
00363 dr_booth.f90                   (f90) 01294 |            ALLOCATE(CT_LOSSES(NUMBER_OF_CONTRACTS))|
00364 dr_booth.f90                   (f90) 02933 |         ALLOCATE(SOX_BY_FUEL_TYPE(3,LOCAL_MAX_CL_UNITS), &|
00365 dsm_objt.for                   (for) 00344 |            ALLOCATE(DEVICE_APPLICATION(MAX_DSM_DEVICE_NUM),|
00366 dsm_objt.for                   (for) 00360 |            ALLOCATE(DEVICE_APPLICATION(MAX_DSM_DEVICE_NUM))|
00367 dsm_objt.for                   (for) 00469 |            ALLOCATE(BASDEV(24,DSM_RESPONSE_CURVES),|
00368 dsm_objt.for                   (for) 00481 |            ALLOCATE(RESPONSE_DATA_LOCATION(12,MAX_RESPONSE_NUM),|
00369 dsm_objt.for                   (for) 00677 |      ALLOCATE(BASE_PROG_EXP_SAVE(DSM_FINANCIAL_RECORDS),|
00370 dsm_objt.for                   (for) 01013 |            ALLOCATE(MONTHLY_DSM_DATA_BASE(25,APPLICATION_NUM,3))|
00371 dsm_objt.for                   (for) 03550 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_DSM_CLASS_NUM))|
00372 dsm_objt.for                   (for) 03574 |         ALLOCATE(DSM_CLASS_EXPENSES(0:NUM_OF_DSM_CLASSES),|
00373 dsm_objt.for                   (for) 03596 |         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS),|
00374 dsm_objt.for                   (for) 03646 |         ALLOCATE(DSM_BOOK_DEP(0:NUM_OF_DSM_CLASSES,|
00375 dsslam.f90                     (f90) 00064 |      ALLOCATE(FRCLOD(8800),FORECAST_LOADS(24,3,12,SYSTEM_CLASS_NUM))|
00376 econintr.f90                   (f90) 00147 |         ALLOCATE(EXT_RR_AT_LOAD_POINT(800))|
00377 el_objt.f90                    (f90) 00528 |         ALLOCATE(PEAK_ADJ_2_OFF_SYSTEM_SALES(STUDY_PERIOD))|
00378 el_objt.f90                    (f90) 00542 |      ALLOCATE(ON_LINE_MONTH(MAX_EL_UNITS),ON_LINE_YEAR(MAX_EL_UNITS),OFF_LINE_MONTH(MAX_EL_UNITS),OFF_LINE_YEAR(MAX_EL_UNITS), &|
00379 el_objt.f90                    (f90) 00564 |         ALLOCATE(EL_OPTIONS_ID_NUM(EL_OPTIONS),EL_OPTIONS_POINTR(EL_OPTIONS))|
00380 el_objt.f90                    (f90) 00570 |      ALLOCATE(PEAK_ADJ_2_OFF_SYSTEM_SALES(STUDY_PERIOD))|
00381 el_objt.f90                    (f90) 01180 |         IF(.NOT. ALLOCATED(TEMP_RETIRED_UNIT_NO)) ALLOCATE(TEMP_RETIRED_UNIT_NO(20),TEMP_RETIRED_OFF_LINE(20))|
00382 en_objt.f90                    (f90) 00157 |         ALLOCATE(BC_TABLE_POSITION_FOR(0:MAX_BC_CLASS_NUMBER))|
00383 en_objt.f90                    (f90) 00243 |      ALLOCATE(OL_TABLE_POSITION_FOR(0:MAX_OL_CLASS_NUMBER))|
00384 en_objt.f90                    (f90) 00475 |         ALLOCATE(ENVIR_CLASS_2_TABLE(0:MAX_CLASS_NUM))|
00385 en_objt.f90                    (f90) 00510 |         ALLOCATE(CLASS_EPA_EAS(0:MAX_TABLE_NUM),   &|
00386 en_objt.f90                    (f90) 00540 |         ALLOCATE(CLASS_NET_ANN_EMIS(NUMBER_OF_EMISSION_TYPES),   &|
00387 enrglimt.f90                   (f90) 00698 |         ALLOCATE(TRANS_ROR_CAPACITY(0:R_NUMBER_OF_TRANS_GROUPS))|
00388 enrglimt.f90                   (f90) 00699 |         ALLOCATE(TRANS_PEAK_CAPACITY(0:R_NUMBER_OF_TRANS_GROUPS))|
00389 enrglimt.f90                   (f90) 00700 |         ALLOCATE(TRANS_PUMP_CAPACITY(0:R_NUMBER_OF_TRANS_GROUPS))|
00390 enrglimt.f90                   (f90) 02453 |            ALLOCATE( &|
00391 enrglimt.f90                   (f90) 02466 |            ALLOCATE(MON_MDS_EL_ENERGY(HYDRO_UNITS,2,0:12))|
00392 enrglimt.f90                   (f90) 02467 |            ALLOCATE(MON_MDS_EL_CAPACITY(HYDRO_UNITS,2,0:12))|
00393 enrglimt.f90                   (f90) 02468 |            ALLOCATE(MON_MDS_EL_VAR_COST(HYDRO_UNITS,2,0:12))|
00394 enrglimt.f90                   (f90) 02469 |            ALLOCATE(MON_MDS_EL_FIXED_COST(HYDRO_UNITS,2,0:12))|
00395 enrglimt.f90                   (f90) 02470 |            ALLOCATE(MON_MDS_EL_SO2_EMIS(HYDRO_UNITS,2,0:12))|
00396 enrglimt.f90                   (f90) 02471 |            ALLOCATE(MON_MDS_EL_MARKET_REVENUE(HYDRO_UNITS,2,0:12))|
00397 enrglimt.f90                   (f90) 02479 |            ALLOCATE(ANNUAL_EL_ENERGY(HYDRO_UNITS,2))|
00398 enrglimt.f90                   (f90) 02480 |            ALLOCATE(ANNUAL_EL_CAPACITY(HYDRO_UNITS,2))|
00399 enrglimt.f90                   (f90) 02481 |            ALLOCATE(ANNUAL_EL_VAR_COST(HYDRO_UNITS,2))|
00400 enrglimt.f90                   (f90) 02482 |            ALLOCATE(ANNUAL_EL_FIXED_COST(HYDRO_UNITS,2))|
00401 enrglimt.f90                   (f90) 02483 |            ALLOCATE(ANNUAL_EL_SO2_EMIS(HYDRO_UNITS,2))|
00402 enrglimt.f90                   (f90) 03152 |            ALLOCATE(MONTHLY_EL_GROUP_REPORT(0:12,0:MAX_MONTHLY_GROUPS, &|
00403 enrglimt.f90                   (f90) 03179 |               ALLOCATE(FISCAL_EL_ENERGY(HYDRO_UNITS,2))|
00404 enrglimt.f90                   (f90) 03180 |               ALLOCATE(FISCAL_EL_CAPACITY(HYDRO_UNITS,2))|
00405 enrglimt.f90                   (f90) 03181 |               ALLOCATE(FISCAL_EL_VAR_COST(HYDRO_UNITS,2))|
00406 enrglimt.f90                   (f90) 03182 |               ALLOCATE(FISCAL_EL_FIXED_COST(HYDRO_UNITS,2))|
00407 enrglimt.f90                   (f90) 03183 |               ALLOCATE(FISCAL_EL_SO2_EMIS(HYDRO_UNITS,2))|
00408 enrglimt.f90                   (f90) 03184 |               ALLOCATE(FISCAL_EL_MARKET_REVENUE(HYDRO_UNITS,2))|
00409 enrglimt.f90                   (f90) 03238 |         ALLOCATE(ASSET_CLASS_NUM(MAX_EL_UNITS))|
00410 enrglimt.f90                   (f90) 03239 |         ALLOCATE(INTRA_COMPANY_CLASS_ID(MAX_EL_UNITS))|
00411 enrglimt.f90                   (f90) 03240 |         ALLOCATE(ASSET_CLASS_VECTOR(MAX_EL_UNITS))|
00412 enrglimt.f90                   (f90) 03844 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_HYDRO_CLASS_NUM))|
00413 enrglimt.f90                   (f90) 03863 |         ALLOCATE(EL_ANN_CLASS_VAR_COST(0:NUM_OF_HYDRO_CLASSES,4))|
00414 enrglimt.f90                   (f90) 03864 |         ALLOCATE(EL_ANN_CLASS_FIXED_COST(0:NUM_OF_HYDRO_CLASSES,4))|
00415 enrglimt.f90                   (f90) 03865 |         ALLOCATE(EL_ANN_CLASS_REVENUE(0:NUM_OF_HYDRO_CLASSES,4))|
00416 enrglimt.f90                   (f90) 03866 |         ALLOCATE(EL_ANN_CLASS_CAPACITY(0:NUM_OF_HYDRO_CLASSES,4))|
00417 enrglimt.f90                   (f90) 03867 |         ALLOCATE(EL_ANN_CLASS_ENERGY(0:NUM_OF_HYDRO_CLASSES,4))|
00418 enrglimt.f90                   (f90) 03868 |         ALLOCATE(EL_ANN_CLASS_SELL_CAPACITY(0:NUM_OF_HYDRO_CLASSES,4))|
00419 enrglimt.f90                   (f90) 03869 |         ALLOCATE(EL_ANN_CLASS_SELL_ENERGY(0:NUM_OF_HYDRO_CLASSES,4))|
00420 enrglimt.f90                   (f90) 03870 |         ALLOCATE(EL_ANN_CLASS_PURCHASES(0:NUM_OF_HYDRO_CLASSES,4))|
00421 enrglimt.f90                   (f90) 03871 |         ALLOCATE(EL_ANN_CLASS_SO2(0:NUM_OF_HYDRO_CLASSES))|
00422 enrglimt.f90                   (f90) 03872 |         ALLOCATE(INTRA_COMPANY_SALES_REVENUE(0:1024))|
00423 enrglimt.f90                   (f90) 03873 |         ALLOCATE(INTRA_COMPANY_PURCHASE_EXPENSE(0:1024))|
00424 enrglimt.f90                   (f90) 03875 |         ALLOCATE(EL_ANN_CLASS_MARKET_REVENUES(0:NUM_OF_HYDRO_CLASSES))|
00425 enrglimt.f90                   (f90) 03876 |         ALLOCATE(EL_ANN_CLASS_MARKET_PURCHASE(0:NUM_OF_HYDRO_CLASSES))|
00426 enrglimt.f90                   (f90) 03897 |         ALLOCATE(EL_MON_MDS_VAR_COST(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00427 enrglimt.f90                   (f90) 03901 |         ALLOCATE(EL_MON_MDS_FIXED_COST(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00428 enrglimt.f90                   (f90) 03902 |         ALLOCATE(EL_MON_MDS_REVENUE(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00429 enrglimt.f90                   (f90) 03903 |         ALLOCATE(EL_MON_MDS_CAPACITY(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00430 enrglimt.f90                   (f90) 03904 |         ALLOCATE(EL_MON_MDS_ENERGY(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00431 enrglimt.f90                   (f90) 03905 |         ALLOCATE(EL_MON_MDS_SELL_CAPACITY( &|
00432 enrglimt.f90                   (f90) 03907 |         ALLOCATE(EL_MON_MDS_SELL_ENERGY(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00433 enrglimt.f90                   (f90) 03908 |         ALLOCATE(EL_MON_MDS_PURCHASES(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00434 enrglimt.f90                   (f90) 03909 |         ALLOCATE(EL_MON_MDS_SO2(0:NUM_OF_HYDRO_CLASSES,0:12))|
00435 enrglimt.f90                   (f90) 03910 |         ALLOCATE(MON_MDS_INCO_SALES_REVENUE(0:1024,0:12))|
00436 enrglimt.f90                   (f90) 03911 |         ALLOCATE(MON_MDS_INCO_PURCHASE_EXPENSE(0:1024,0:12))|
00437 enrglimt.f90                   (f90) 03912 |         ALLOCATE(MON_MDS_IN_SALES_REVENUE(0:12))|
00438 enrglimt.f90                   (f90) 03913 |         ALLOCATE(MON_MDS_IN_PURCHASE_EXPENSES(0:12))|
00439 enrglimt.f90                   (f90) 03914 |         ALLOCATE(MON_MDS_IN_VARIABLE_EXPENSES(0:12))|
00440 enrglimt.f90                   (f90) 03915 |         ALLOCATE(MON_MDS_IN_FIXED_EXPENSES(0:12))|
00441 enrglimt.f90                   (f90) 03917 |         ALLOCATE(EL_MON_MDS_MARKET_REVENUES( &|
00442 enrglimt.f90                   (f90) 03919 |         ALLOCATE(EL_MON_MDS_MARKET_PURCHASE( &|
00443 enrglimt.f90                   (f90) 03925 |         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))|
00444 enrglimt.f90                   (f90) 03926 |         ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))|
00445 enrglimt.f90                   (f90) 04950 |            ALLOCATE(CLASS_NAME(0:MAX_CLASS_NUM))|
00446 enrglimt.f90                   (f90) 05245 |      ALLOCATE(SORT_POS(LOAD_HOURS_IN_PERIOD))|
00447 enrglimt.f90                   (f90) 05466 |         ALLOCATE(HOURLY_HYDRO(LOAD_HOURS_IN_PERIOD, &|
00448 enrglimt.for                   (for) 00698 |         ALLOCATE(TRANS_ROR_CAPACITY(0:R_NUMBER_OF_TRANS_GROUPS))|
00449 enrglimt.for                   (for) 00699 |         ALLOCATE(TRANS_PEAK_CAPACITY(0:R_NUMBER_OF_TRANS_GROUPS))|
00450 enrglimt.for                   (for) 00700 |         ALLOCATE(TRANS_PUMP_CAPACITY(0:R_NUMBER_OF_TRANS_GROUPS))|
00451 enrglimt.for                   (for) 02453 |            ALLOCATE( &|
00452 enrglimt.for                   (for) 02466 |            ALLOCATE(MON_MDS_EL_ENERGY(HYDRO_UNITS,2,0:12))|
00453 enrglimt.for                   (for) 02467 |            ALLOCATE(MON_MDS_EL_CAPACITY(HYDRO_UNITS,2,0:12))|
00454 enrglimt.for                   (for) 02468 |            ALLOCATE(MON_MDS_EL_VAR_COST(HYDRO_UNITS,2,0:12))|
00455 enrglimt.for                   (for) 02469 |            ALLOCATE(MON_MDS_EL_FIXED_COST(HYDRO_UNITS,2,0:12))|
00456 enrglimt.for                   (for) 02470 |            ALLOCATE(MON_MDS_EL_SO2_EMIS(HYDRO_UNITS,2,0:12))|
00457 enrglimt.for                   (for) 02471 |            ALLOCATE(MON_MDS_EL_MARKET_REVENUE(HYDRO_UNITS,2,0:12))|
00458 enrglimt.for                   (for) 02479 |            ALLOCATE(ANNUAL_EL_ENERGY(HYDRO_UNITS,2))|
00459 enrglimt.for                   (for) 02480 |            ALLOCATE(ANNUAL_EL_CAPACITY(HYDRO_UNITS,2))|
00460 enrglimt.for                   (for) 02481 |            ALLOCATE(ANNUAL_EL_VAR_COST(HYDRO_UNITS,2))|
00461 enrglimt.for                   (for) 02482 |            ALLOCATE(ANNUAL_EL_FIXED_COST(HYDRO_UNITS,2))|
00462 enrglimt.for                   (for) 02483 |            ALLOCATE(ANNUAL_EL_SO2_EMIS(HYDRO_UNITS,2))|
00463 enrglimt.for                   (for) 03152 |            ALLOCATE(MONTHLY_EL_GROUP_REPORT(0:12,0:MAX_MONTHLY_GROUPS, &|
00464 enrglimt.for                   (for) 03179 |               ALLOCATE(FISCAL_EL_ENERGY(HYDRO_UNITS,2))|
00465 enrglimt.for                   (for) 03180 |               ALLOCATE(FISCAL_EL_CAPACITY(HYDRO_UNITS,2))|
00466 enrglimt.for                   (for) 03181 |               ALLOCATE(FISCAL_EL_VAR_COST(HYDRO_UNITS,2))|
00467 enrglimt.for                   (for) 03182 |               ALLOCATE(FISCAL_EL_FIXED_COST(HYDRO_UNITS,2))|
00468 enrglimt.for                   (for) 03183 |               ALLOCATE(FISCAL_EL_SO2_EMIS(HYDRO_UNITS,2))|
00469 enrglimt.for                   (for) 03184 |               ALLOCATE(FISCAL_EL_MARKET_REVENUE(HYDRO_UNITS,2))|
00470 enrglimt.for                   (for) 03238 |         ALLOCATE(ASSET_CLASS_NUM(MAX_EL_UNITS))|
00471 enrglimt.for                   (for) 03239 |         ALLOCATE(INTRA_COMPANY_CLASS_ID(MAX_EL_UNITS))|
00472 enrglimt.for                   (for) 03240 |         ALLOCATE(ASSET_CLASS_VECTOR(MAX_EL_UNITS))|
00473 enrglimt.for                   (for) 03844 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_HYDRO_CLASS_NUM))|
00474 enrglimt.for                   (for) 03863 |         ALLOCATE(EL_ANN_CLASS_VAR_COST(0:NUM_OF_HYDRO_CLASSES,4))|
00475 enrglimt.for                   (for) 03864 |         ALLOCATE(EL_ANN_CLASS_FIXED_COST(0:NUM_OF_HYDRO_CLASSES,4))|
00476 enrglimt.for                   (for) 03865 |         ALLOCATE(EL_ANN_CLASS_REVENUE(0:NUM_OF_HYDRO_CLASSES,4))|
00477 enrglimt.for                   (for) 03866 |         ALLOCATE(EL_ANN_CLASS_CAPACITY(0:NUM_OF_HYDRO_CLASSES,4))|
00478 enrglimt.for                   (for) 03867 |         ALLOCATE(EL_ANN_CLASS_ENERGY(0:NUM_OF_HYDRO_CLASSES,4))|
00479 enrglimt.for                   (for) 03868 |         ALLOCATE(EL_ANN_CLASS_SELL_CAPACITY(0:NUM_OF_HYDRO_CLASSES,4))|
00480 enrglimt.for                   (for) 03869 |         ALLOCATE(EL_ANN_CLASS_SELL_ENERGY(0:NUM_OF_HYDRO_CLASSES,4))|
00481 enrglimt.for                   (for) 03870 |         ALLOCATE(EL_ANN_CLASS_PURCHASES(0:NUM_OF_HYDRO_CLASSES,4))|
00482 enrglimt.for                   (for) 03871 |         ALLOCATE(EL_ANN_CLASS_SO2(0:NUM_OF_HYDRO_CLASSES))|
00483 enrglimt.for                   (for) 03872 |         ALLOCATE(INTRA_COMPANY_SALES_REVENUE(0:1024))|
00484 enrglimt.for                   (for) 03873 |         ALLOCATE(INTRA_COMPANY_PURCHASE_EXPENSE(0:1024))|
00485 enrglimt.for                   (for) 03875 |         ALLOCATE(EL_ANN_CLASS_MARKET_REVENUES(0:NUM_OF_HYDRO_CLASSES))|
00486 enrglimt.for                   (for) 03876 |         ALLOCATE(EL_ANN_CLASS_MARKET_PURCHASE(0:NUM_OF_HYDRO_CLASSES))|
00487 enrglimt.for                   (for) 03897 |         ALLOCATE(EL_MON_MDS_VAR_COST(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00488 enrglimt.for                   (for) 03901 |         ALLOCATE(EL_MON_MDS_FIXED_COST(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00489 enrglimt.for                   (for) 03902 |         ALLOCATE(EL_MON_MDS_REVENUE(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00490 enrglimt.for                   (for) 03903 |         ALLOCATE(EL_MON_MDS_CAPACITY(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00491 enrglimt.for                   (for) 03904 |         ALLOCATE(EL_MON_MDS_ENERGY(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00492 enrglimt.for                   (for) 03905 |         ALLOCATE(EL_MON_MDS_SELL_CAPACITY( &|
00493 enrglimt.for                   (for) 03907 |         ALLOCATE(EL_MON_MDS_SELL_ENERGY(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00494 enrglimt.for                   (for) 03908 |         ALLOCATE(EL_MON_MDS_PURCHASES(0:NUM_OF_HYDRO_CLASSES,4,0:12))|
00495 enrglimt.for                   (for) 03909 |         ALLOCATE(EL_MON_MDS_SO2(0:NUM_OF_HYDRO_CLASSES,0:12))|
00496 enrglimt.for                   (for) 03910 |         ALLOCATE(MON_MDS_INCO_SALES_REVENUE(0:1024,0:12))|
00497 enrglimt.for                   (for) 03911 |         ALLOCATE(MON_MDS_INCO_PURCHASE_EXPENSE(0:1024,0:12))|
00498 enrglimt.for                   (for) 03912 |         ALLOCATE(MON_MDS_IN_SALES_REVENUE(0:12))|
00499 enrglimt.for                   (for) 03913 |         ALLOCATE(MON_MDS_IN_PURCHASE_EXPENSES(0:12))|
00500 enrglimt.for                   (for) 03914 |         ALLOCATE(MON_MDS_IN_VARIABLE_EXPENSES(0:12))|
00501 enrglimt.for                   (for) 03915 |         ALLOCATE(MON_MDS_IN_FIXED_EXPENSES(0:12))|
00502 enrglimt.for                   (for) 03917 |         ALLOCATE(EL_MON_MDS_MARKET_REVENUES( &|
00503 enrglimt.for                   (for) 03919 |         ALLOCATE(EL_MON_MDS_MARKET_PURCHASE( &|
00504 enrglimt.for                   (for) 03925 |         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))|
00505 enrglimt.for                   (for) 03926 |         ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))|
00506 enrglimt.for                   (for) 04950 |            ALLOCATE(CLASS_NAME(0:MAX_CLASS_NUM))|
00507 enrglimt.for                   (for) 05245 |      ALLOCATE(SORT_POS(LOAD_HOURS_IN_PERIOD))|
00508 enrglimt.for                   (for) 05466 |         ALLOCATE(HOURLY_HYDRO(LOAD_HOURS_IN_PERIOD, &|
00509 esrn_obj.f90                   (f90) 00371 |         ALLOCATE(I4_VECTOR_MAP(-1:MAX_VECTOR_NUM),VECTOR_VALUES(AVAIL_DATA_YEARS+1,-1:MAX_VECTORS),I4_REFERENCE_VECTOR_MAP(0:9), &|
00510 esrn_obj.f90                   (f90) 00531 |         ALLOCATE(ACTIVE_VECTOR_VALUES(-1:MAXIMUM_VECTORS))|
00511 facalg.f90                     (f90) 00616 |        allocate(iOfFacAlgs(nVr),stat=AS)|
00512 facalg.f90                     (f90) 00620 |        allocate(x         (nVr),stat=AS)|
00513 facalg.f90                     (f90) 00622 |        allocate(cOrg      (nVr),stat=AS)|
00514 facalg.f90                     (f90) 00624 |        allocate(cMod      (nVr),stat=AS)|
00515 facalg.f90                     (f90) 00626 |        allocate(GLB       (nVr),stat=AS)|
00516 facalg.f90                     (f90) 00628 |        allocate(LUB       (nVr),stat=AS)|
00517 facalg.f90                     (f90) 00630 |        allocate(Capacity  (nVr),stat=AS)|
00518 facalg.f90                     (f90) 00632 |        allocate(EquivAvail(nVr),stat=AS)|
00519 facalg.f90                     (f90) 00634 |        allocate(UnitOwning(nVr),stat=AS)|
00520 facalg.f90                     (f90) 00636 |        allocate(BlockPos  (nVr),stat=AS)|
00521 facalg.f90                     (f90) 00638 |        allocate(AuxRow    (nVr),stat=AS)|
00522 facalg.f90                     (f90) 00640 |        allocate(AuxCycled (nVr),stat=AS)|
00523 facalg.f90                     (f90) 00642 |        allocate(AuxiCovers(nVr),stat=AS)|
00524 facalg.f90                     (f90) 00644 |        allocate(BlockOfSameUnitAs(nVr,2),stat=AS)|
00525 facalg.f90                     (f90) 00646 |        allocate(VarOfDispRank(nVr),stat=AS)|
00526 facalg.f90                     (f90) 00656 |        allocate(AuxRHS    (nAux),stat=AS)|
00527 facalg.f90                     (f90) 00658 |        allocate(AuxCovers (nAux,nVrAux),stat=AS)|
00528 facalg.f90                     (f90) 00660 |        allocate(AColumn   (nAux,nVrAux),stat=AS) ! data varies 1st across rows|
00529 facalg.f90                     (f90) 00716 |        allocate(PrevState(nVar),stat=AS)|
00530 facalg.f90                     (f90) 00718 |        allocate(VarState(nVar),stat=AS)|
00531 facalg.f90                     (f90) 00720 |        allocate(PrevBDIRow(nVar),stat=AS)|
00532 facalg.f90                     (f90) 00722 |        allocate(BasisDIRow(nVar),stat=AS)|
00533 facalg.f90                     (f90) 00724 |        allocate(PrevBKyVar(nVar),stat=AS)|
00534 facalg.f90                     (f90) 00726 |        allocate(BasisKyVar(nVar),stat=AS)|
00535 facalg.f90                     (f90) 00728 |        allocate(FctOwning(nVar),stat=AS)|
00536 facalg.f90                     (f90) 00730 |        allocate(KeyVar(nVar),stat=AS)|
00537 facalg.f90                     (f90) 00732 |        allocate(d(nVar),stat=AS)|
00538 facalg.f90                     (f90) 00734 |        allocate(Phi(nVar),stat=AS)|
00539 facalg.f90                     (f90) 00736 |        allocate(DualVar(nVar),stat=AS)|
00540 facalg.f90                     (f90) 00738 |        allocate(BkOwns(nVar),stat=AS)|
00541 facalg.f90                     (f90) 00740 |        allocate(BindSetOwns(nVar),stat=AS)|
00542 facalg.f90                     (f90) 00742 |        allocate(TestSetOwns(nVar),stat=AS)|
00543 facalg.f90                     (f90) 00744 |        allocate(WouldBeZero(nVar),stat=AS)|
00544 facalg.f90                     (f90) 00746 |        allocate(VarInFctOrder(nVar),stat=AS)|
00545 facalg.f90                     (f90) 00748 |        allocate(VFOFirstInFct(0:nVar),stat=AS) ! 0 allows room for InvalidFacet|
00546 facalg.f90                     (f90) 00751 |          allocate(BNKinRow(nAux),stat=AS)|
00547 facalg.f90                     (f90) 00753 |          allocate(AuxDualMult(nAux),stat=AS)|
00548 facalg.f90                     (f90) 00755 |          allocate(AuxColumn(nAux),stat=AS)|
00549 facalg.f90                     (f90) 00757 |          allocate(DColumn(nAux,nAux),stat=AS)|
00550 facalg.f90                     (f90) 00759 |          allocate(DOrgRow(nAux,nAux),stat=AS)|
00551 facalg.f90                     (f90) 00761 |          allocate(DInvRow(nAux,nAux),stat=AS)|
00552 facalg.f90                     (f90) 00765 |          allocate(VarCumul(nKappa,-1:nVar),stat=AS)|
00553 facalg.f90                     (f90) 00994 |        allocate(AuxCoeff(nVarUB),stat=AS)|
00554 facalg.f90                     (f90) 00996 |        allocate(CFLB    (nVarUB),stat=AS)|
00555 facalg.f90                     (f90) 00998 |        allocate(CFUB    (nVarUB),stat=AS)|
00556 facalg.f90                     (f90) 01000 |        allocate(BlockDispCost(NUNITS,2),stat=AS)|
00557 facalg.f90                     (f90) 01002 |        allocate(BlockHeatRate(NUNITS,2),stat=AS)|
00558 facalg.f90                     (f90) 02474 |        allocate(ArrayOld(nRows,nRows),stat=AS)|
00559 facalg.f90                     (f90) 04074 |          allocate(jMarginal(LimUnknowns),stat=AS) ! need to count even if FctVoidOfDj|
00560 facalg.f90                     (f90) 04077 |          allocate(GrayOrder(0:SupiGr),stat=AS)|
00561 facalg.f90                     (f90) 04151 |                  allocate(HoldOrder(0:SupiGr),stat=AS)|
00562 facalg.f90                     (f90) 04160 |                  allocate(GrayOrder(0:SupiGr),stat=AS)|
00563 fc_objt.f90                    (f90) 00135 |            ALLOCATE(FUEL_PRICE(0:AVAIL_DATA_YEARS))|
00564 fc_objt.f90                    (f90) 00136 |            ALLOCATE(WRITE_FUEL_PRICE(0:AVAIL_DATA_YEARS))|
00565 fc_objt.f90                    (f90) 00216 |      ALLOCATE(FUEL_PRICE(0:AVAIL_DATA_YEARS))|
00566 fe_objt.f90                    (f90) 00243 |      ALLOCATE( P_EMIS_REPORT(MAX_EMISSION_REPORT_GROUPS,NUNITS))|
00567 fe_objt.f90                    (f90) 00244 |      ALLOCATE( S_EMIS_REPORT(MAX_EMISSION_REPORT_GROUPS,NUNITS))|
00568 fe_objt.f90                    (f90) 00245 |      ALLOCATE( EMISS_FUEL_EMIS_REPORT(MAX_EMISSION_REPORT_GROUPS,NUNITS))|
00569 fe_objt.f90                    (f90) 00387 |      ALLOCATE(POINTR_RECORD(0:1024),STAT=ERROR) ! SAVE POINTR_RECORD|
00570 fe_objt.f90                    (f90) 00391 |      ALLOCATE(EMISSION_RATES(0:R_RECORDS,TOTAL_EMISSION_GROUPS)) ! SAVE POINTR_RECORD|
00571 finacelm.for                   (for) 00082 |            ALLOCATE(CLASSIFICATIONS(8),SINGLE_YEAR(8),|
00572 finacelm.for                   (for) 00143 |         ALLOCATE(CLASSIFICATIONS(8),SINGLE_YEAR(8),|
00573 finacelm.for                   (for) 00204 |            ALLOCATE(CLASSIFICATIONS(8),SINGLE_YEAR(8),|
00574 finacelm.for                   (for) 00210 |            ALLOCATE(YEARLY_ELIMINATIONS_VALUES(0:MAX_ELIMINATION_NUM,      |
00575 finacelm.for                   (for) 00363 |         ALLOCATE(CLASSIFICATION_TYPES(0:MAX_ELIMINATION_NUM))|
00576 finanbse.for                   (for) 00136 |            ALLOCATE(VARIABLE_VALUE(0:LAST_OUTPUT_VARIABLE))|
00577 finanbse.for                   (for) 00278 |         ALLOCATE(AI_ANNUAL_DEP(MAX_FINANCIAL_SIMULATION_YEARS),|
00578 finanbse.for                   (for) 00304 |         ALLOCATE(AI_ANNUAL_CLASS_DEP(MAX_FINANCIAL_SIMULATION_YEARS,|
00579 finanbse.for                   (for) 00335 |         ALLOCATE(ASSET_CLASS_NUM(NUNITS),ASSET_VECTOR(NUNITS))|
00580 finanbse.for                   (for) 00359 |         ALLOCATE(ASSET_CLASS_NUM(HYDRO_UNITS),|
00581 finanbse.for                   (for) 00533 |      ALLOCATE(AI_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS),|
00582 findovls.f90                   (f90) 01412 |            ALLOCATE(FAMILY_NAMES(NUMBER_OF_DEFINITIONS),OVERLAY_FILES_IN_FAMILY(NUMBER_OF_DATA_FILES,NUMBER_OF_DEFINITIONS),   &|
00583 finnucfl.f90                   (f90) 00111 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024), &|
00584 finnucfl.f90                   (f90) 00204 |            ALLOCATE(BC_NUC_FUEL_CLASS_POINTER(MAX_BC_NUC_FUEL_ID_NUM))|
00585 finnucfl.f90                   (f90) 00245 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024), &|
00586 finnucfl.f90                   (f90) 00328 |         ALLOCATE(OL_NUC_FUEL_CLASS_POINTER(MAX_OL_NUC_FUEL_ID_NUM))|
00587 finnucfl.f90                   (f90) 00636 |      ALLOCATE(FIRST_YEARS_VEC(AVAIL_DATA_YEARS), &|
00588 finnucfl.f90                   (f90) 00641 |      ALLOCATE(NFCE(MAX_FINANCIAL_SIMULATION_YEARS), &|
00589 finnucfl.f90                   (f90) 00655 |      ALLOCATE(NFIS(FINANCIAL_SIMULATION_YEARS), &|
00590 finnucfl.f90                   (f90) 00680 |      ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &|
00591 finnucfl.f90                   (f90) 00685 |      ALLOCATE( &|
00592 finnucfl.f90                   (f90) 01108 |         ALLOCATE(ID_POINTER(0:10000), &|
00593 finnucfl.f90                   (f90) 01732 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
00594 finnucfl.f90                   (f90) 01748 |         ALLOCATE(TNNFV(FINANCIAL_SIMULATION_YEARS, &|
00595 fuelused.f90                   (f90) 00144 |      ALLOCATE(FUEL_INVENTORY_IS_MONTHLY(0:MAX_FUEL_INV_SERIES))|
00596 fuelused.f90                   (f90) 00145 |      ALLOCATE(ANNUAL_FILE_POINTR(0:MAX_FUEL_INV_SERIES))|
00597 fuelused.f90                   (f90) 00146 |      ALLOCATE(MONTHLY_FILE_POINTR(0:MAX_FUEL_INV_SERIES))|
00598 fuelused.f90                   (f90) 00147 |      ALLOCATE(MMBTU_BALANCE_POINTR(MAXIMUM_FUEL_TYPES))|
00599 fuelused.f90                   (f90) 00156 |      ALLOCATE(PREVIOUS_ANNUAL_AMOUNT(0:AVAIL_DATA_YEARS))|
00600 fuelused.f90                   (f90) 00157 |      ALLOCATE(ANNUAL_AMOUNT(0:AVAIL_DATA_YEARS))|
00601 fuelused.f90                   (f90) 00158 |      ALLOCATE(MONTHLY_AMOUNT(0:AVAIL_DATA_YEARS))|
00602 fuelused.f90                   (f90) 00159 |      ALLOCATE(FIRST_TIME_FOR_THIS_MONTHLY(12,0:MAX_FUEL_INV_SERIES))|
00603 fuelused.f90                   (f90) 00160 |      ALLOCATE(FUEL_INVENTORY_IS_ANNUAL(0:MAX_FUEL_INV_SERIES))|
00604 fuelused.f90                   (f90) 00615 |      ALLOCATE(FIRST_TIME_FOR_THIS(0:MAX_FUEL_PRICE_SERIES),STAT=ERROR)|
00605 fuelused.f90                   (f90) 00656 |      ALLOCATE (NEW_FUEL_PRICE &|
00606 fuelused.f90                   (f90) 00669 |         ALLOCATE(FUEL_PRICE_POINTR(0:MAX_FUEL_PRICE_SERIES),STAT=ERROR)|
00607 fuelused.f90                   (f90) 00679 |      ALLOCATE(FUEL_PRICE(0:FUEL_PRICE_YEARS))|
00608 fuelused.f90                   (f90) 00900 |         ALLOCATE(P_BTU_COST_POINTR(NUNITS))|
00609 fuelused.f90                   (f90) 00901 |         ALLOCATE(P_BTU_CONTENT(NUNITS))|
00610 fuelused.f90                   (f90) 00902 |         ALLOCATE(S_BTU_CONTENT(NUNITS))|
00611 fuelused.f90                   (f90) 00903 |         ALLOCATE(E_BTU_CONTENT(NUNITS))|
00612 fuelused.f90                   (f90) 00904 |         ALLOCATE(P_SHIPPING_UNITS(NUNITS))|
00613 fuelused.f90                   (f90) 00905 |         ALLOCATE(S_SHIPPING_UNITS(NUNITS))|
00614 fuelused.f90                   (f90) 00906 |         ALLOCATE(E_SHIPPING_UNITS(NUNITS))|
00615 fuelused.f90                   (f90) 00907 |         ALLOCATE(S_BTU_COST_POINTR(NUNITS))|
00616 fuelused.f90                   (f90) 00908 |         ALLOCATE(E_BTU_COST_POINTR(NUNITS))|
00617 fuelused.f90                   (f90) 00909 |         ALLOCATE(P_BTU_COST_IS_A_POINTR(NUNITS))|
00618 fuelused.f90                   (f90) 00910 |         ALLOCATE(S_BTU_COST_IS_A_POINTR(NUNITS))|
00619 fuelused.f90                   (f90) 00911 |         ALLOCATE(E_BTU_COST_IS_A_POINTR(NUNITS))|
00620 fuelused.f90                   (f90) 01162 |         ALLOCATE(PRIMARY_DELIVERY_COST(R_UNIT))|
00621 fuelused.f90                   (f90) 01163 |         ALLOCATE(SECONDARY_DELIVERY_COST(R_UNIT))|
00622 gas_objt.for                   (for) 00221 |               ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023),|
00623 gas_objt.for                   (for) 00763 |            ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023),|
00624 gas_objt.for                   (for) 02115 |         ALLOCATE(POWER_DAILY_VOLUME_BY_MONTH(0:12,|
00625 gas_objt.for                   (for) 02217 |            ALLOCATE(CUSTOMER_GROUP(MAX_TRANS_LOAD_TABLES)) ! INT2|
00626 gas_objt.for                   (for) 02218 |            ALLOCATE(iPort(MAX_TRANS_LOAD_TABLES),|
00627 gas_objt.for                   (for) 02220 |            ALLOCATE(CUSTOMER_GROUP_2(MAX_TRANS_LOAD_TABLES))|
00628 gas_objt.for                   (for) 02221 |            ALLOCATE(|
00629 gas_objt.for                   (for) 02231 |            ALLOCATE(GAS_GROUP_ID(MAX_TRANS_LOAD_TABLES)) ! INT2|
00630 gas_objt.for                   (for) 02232 |            ALLOCATE(REFERENCE_LOAD_NUMBER(MAX_TRANS_LOAD_TABLES)) ! INT2|
00631 gas_objt.for                   (for) 02233 |            ALLOCATE(MARKET_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00632 gas_objt.for                   (for) 02234 |            ALLOCATE(ANNUAL_TG_PEAK(0:12,MAX_TRANS_GROUPS))|
00633 gas_objt.for                   (for) 02235 |            ALLOCATE(ANNUAL_TG_VOLUME(0:MAX_TRANS_GROUPS))|
00634 gas_objt.for                   (for) 02237 |               ALLOCATE(ANNUAL_TG_POWER_VOLUME(0:MAX_TRANS_GROUPS))|
00635 gas_objt.for                   (for) 02239 |            ALLOCATE(MONTHLY_ENERGY_PRICE_PATTERN(|
00636 gas_objt.for                   (for) 02241 |            ALLOCATE(MARKET_DEMAND_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00637 gas_objt.for                   (for) 02242 |            ALLOCATE(MONTHLY_DEMAND_PRICE_PATTERN(|
00638 gas_objt.for                   (for) 02244 |            ALLOCATE(MARKET_CUSTOMER_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00639 gas_objt.for                   (for) 02245 |            ALLOCATE(ASSET_CLASS_ID(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00640 gas_objt.for                   (for) 02246 |            ALLOCATE(ASSET_CLASS_REV_ALLOC_VECTOR(|
00641 gas_objt.for                   (for) 02248 |            ALLOCATE(ANNUAL_ENERGY(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00642 gas_objt.for                   (for) 02249 |            ALLOCATE(ANNUAL_PEAK(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00643 gas_objt.for                   (for) 02250 |            ALLOCATE(ANNUAL_CUSTOMERS(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00644 gas_objt.for                   (for) 02251 |            ALLOCATE(ANNUAL_MULTIPLIER(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00645 gas_objt.for                   (for) 02252 |            ALLOCATE(MONTHLY_ENERGY(12,MAX_TRANS_LOAD_TABLES)) ! (12) REAL4|
00646 gas_objt.for                   (for) 02253 |            ALLOCATE(MONTHLY_PEAK(12,MAX_TRANS_LOAD_TABLES)) ! (12) REAL4|
00647 gas_objt.for                   (for) 02254 |            ALLOCATE(MONTHLY_CUSTOMERS(12,MAX_TRANS_LOAD_TABLES)) ! (12) REAL4|
00648 gas_objt.for                   (for) 02255 |            ALLOCATE(DIST_ENERGY_LOSS_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00649 gas_objt.for                   (for) 02256 |            ALLOCATE(TRANS_ENERGY_LOSS_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00650 gas_objt.for                   (for) 02257 |            ALLOCATE(PEAK_LOSS_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00651 gas_objt.for                   (for) 02258 |            ALLOCATE(PEAK_COIN_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00652 gas_objt.for                   (for) 02259 |            ALLOCATE(DISTRIBUTION_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4|
00653 gas_objt.for                   (for) 02260 |            ALLOCATE(TRANSMISSION_PRICE(MAX_TRANS_LOAD_TABLES)) !REAL4!|
00654 gas_objt.for                   (for) 02261 |            ALLOCATE(CUSTOMER_CLASS_NAME(MAX_TRANS_LOAD_TABLES)) ! CHAR*30|
00655 gas_objt.for                   (for) 02262 |            ALLOCATE(CALCULATION_MODE(MAX_TRANS_LOAD_TABLES)) ! CHAR*1|
00656 gas_objt.for                   (for) 02263 |            ALLOCATE(REFERENCE_LOAD_NAME(MAX_TRANS_LOAD_TABLES)) ! CHAR*5|
00657 gas_objt.for                   (for) 02264 |            ALLOCATE(TRANS_MONTHLY_ENERGY(12,MAX_TRANS_LOAD_TABLES))|
00658 gas_objt.for                   (for) 02265 |            ALLOCATE(DEMAND_GROUP_AVAILABLE(MAX_TRANS_GROUPS))|
00659 gas_objt.for                   (for) 02266 |            ALLOCATE(WH_TRANS_MONTHLY_ENERGY(12,|
00660 gas_objt.for                   (for) 02268 |            ALLOCATE(WH_TRANS_MONTHLY_CAPACITY(12,|
00661 gas_objt.for                   (for) 02270 |            ALLOCATE(TRANS_MONTHLY_PEAK(12,MAX_TRANS_LOAD_TABLES))|
00662 gas_objt.for                   (for) 02271 |            ALLOCATE(TRANS_MONTHLY_CUSTOMERS(12,MAX_TRANS_LOAD_TABLES))|
00663 gas_objt.for                   (for) 02272 |            ALLOCATE(TABLE_ACTIVE(MAX_TRANS_LOAD_TABLES))|
00664 gas_objt.for                   (for) 02273 |            ALLOCATE(BASECASE_MARKET_AREA_ID(MAX_TRANS_LOAD_TABLES))|
00665 gas_objt.for                   (for) 02274 |            ALLOCATE(WD_INDEX(MAX_TRANS_LOAD_TABLES))|
00666 gas_objt.for                   (for) 02275 |            ALLOCATE(GW_INDEX(MAX_TRANS_LOAD_TABLES))|
00667 gas_objt.for                   (for) 02276 |            ALLOCATE(BASECASE_TRANS_AREA_ID(MAX_TRANS_LOAD_TABLES))|
00668 gas_objt.for                   (for) 02277 |            ALLOCATE(BASECASE_NERC_SUB_ID(MAX_TRANS_LOAD_TABLES))|
00669 gas_objt.for                   (for) 02278 |            ALLOCATE(MONTHLY_UNITS(MAX_TRANS_LOAD_TABLES))|
00670 gas_objt.for                   (for) 02279 |            ALLOCATE(PRICE_INDEX_ACTIVE(MAX_TRANS_LOAD_TABLES))|
00671 gas_objt.for                   (for) 02280 |            ALLOCATE(THREE_FACTOR_TRANSFORM(MAX_TRANS_LOAD_TABLES))|
00672 gas_objt.for                   (for) 02281 |            ALLOCATE(JURISDICTIONAL_CUSTOMER(MAX_TRANS_LOAD_TABLES))|
00673 gas_objt.for                   (for) 02282 |            ALLOCATE(FUEL_COST_RECOVERY_THROUGH_FAC(|
00674 gas_objt.for                   (for) 02284 |            ALLOCATE(BASE_COST_OF_FAC_FUEL(MAX_TRANS_LOAD_TABLES))|
00675 gas_objt.for                   (for) 02285 |            ALLOCATE(MINIMUM_MARKET_PRICE(MAX_TRANS_LOAD_TABLES))|
00676 gas_objt.for                   (for) 02286 |            ALLOCATE(MAXIMUM_MARKET_PRICE(MAX_TRANS_LOAD_TABLES))|
00677 gas_objt.for                   (for) 02287 |            ALLOCATE(INDEXED_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES))|
00678 gas_objt.for                   (for) 02288 |            ALLOCATE(TG_COUNTER(MAX_TRANS_GROUPS))|
00679 gas_objt.for                   (for) 02289 |            ALLOCATE(LOAD_DISPATCH_POSITION(MAX_TRANS_LOAD_TABLES,|
00680 gas_objt.for                   (for) 02291 |            ALLOCATE(LOAD_DISPATCH_INDEX(MAX_TRANS_LOAD_TABLES,|
00681 gas_objt.for                   (for) 02294 |            ALLOCATE(GAS_LOAD_GROUPS_INDEX(MAX_TRANS_GROUPS))|
00682 gas_objt.for                   (for) 02295 |            ALLOCATE(CUST_CLASS_GROUPS_INDEX(0:MAX_CLASS_GROUPS))|
00683 gas_objt.for                   (for) 02296 |            ALLOCATE(CUST2_CLASS_GROUPS_INDEX(0:MAX_CLASS2_GROUPS))|
00684 gas_objt.for                   (for) 02297 |            ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))|
00685 gas_objt.for                   (for) 02298 |            ALLOCATE(ASSET_2_TRANS_INDEX(0:MAX_ASSET_GROUPS,|
00686 gas_objt.for                   (for) 02300 |            ALLOCATE(NUMBER_ASSET_2_TRANS(MAX_TRANS_GROUPS))|
00687 gas_objt.for                   (for) 02301 |            ALLOCATE(ASSET_TRANSACTION_CROSS_INDEX(0:MAX_ASSET_GROUPS,|
00688 gas_objt.for                   (for) 02303 |            ALLOCATE(NUMBER_TRANS_PER_AC_TG(0:MAX_ASSET_GROUPS,|
00689 gas_objt.for                   (for) 02305 |            ALLOCATE(TRANS_WITHIN_AC_TG(0:MAX_ASSET_GROUPS,|
00690 gas_objt.for                   (for) 02307 |            ALLOCATE(FIRST_AC_TG(0:MAX_ASSET_GROUPS))|
00691 gas_objt.for                   (for) 02308 |            ALLOCATE(FIRST_TABLE_TG(0:MAX_TRANS_GROUPS))|
00692 gas_objt.for                   (for) 02309 |            ALLOCATE(GAS_LOAD_2_TRANS_GROUPS(MAX_TRANS_GROUPS))|
00693 gas_objt.for                   (for) 02310 |            ALLOCATE(GAS_LOAD_GROUP_2_TG(MAX_TRANS_GROUPS))|
00694 gas_objt.for                   (for) 02311 |            ALLOCATE(CUST_CLASS_GROUP_2_CG(MAX_CLASS_GROUPS+1))|
00695 gas_objt.for                   (for) 02312 |            ALLOCATE(CUST2_CLASS_GROUP_2_CG(MAX_CLASS_GROUPS+1))|
00696 gas_objt.for                   (for) 02313 |            ALLOCATE(ASSET_CLASS_GROUP_2_AC(0:MAX_ASSET_GROUPS+1))|
00697 gas_objt.for                   (for) 02314 |            ALLOCATE(LAST_TABLE_FOR_TG(MAX_TRANS_GROUPS))|
00698 gas_objt.for                   (for) 02315 |            ALLOCATE(LAST_TABLE_FOR_CG(MAX_CLASS_GROUPS+1))|
00699 gas_objt.for                   (for) 02316 |            ALLOCATE(LAST_TABLE_FOR_CG2(MAX_CLASS_GROUPS+1))|
00700 gas_objt.for                   (for) 02317 |            ALLOCATE(TG_CG_DATABASE(0:12,0:MAX_TRANS_GROUPS,|
00701 gas_objt.for                   (for) 02319 |            ALLOCATE(TG_CG2_DATABASE(0:12,0:MAX_TRANS_GROUPS,|
00702 gas_objt.for                   (for) 02321 |            ALLOCATE(TABLE_DAY_SHIFT(MAX_TRANS_LOAD_TABLES))|
00703 gas_objt.for                   (for) 02322 |            ALLOCATE(GAS_DEMAND_PLANNING_PEAK(0:MAX_TRANS_GROUPS,12))|
00704 gas_objt.for                   (for) 02323 |            ALLOCATE(PA_PLANNING_PEAK(0:NUMBER_OF_ACTIVE_GROUPS,12))|
00705 gas_objt.for                   (for) 02324 |            ALLOCATE(GLOBAL_PA_PEAK(0:NUMBER_OF_ACTIVE_GROUPS))|
00706 gas_objt.for                   (for) 02325 |            ALLOCATE(GLOBAL_PA_PEAK_MONTH(0:NUMBER_OF_ACTIVE_GROUPS))|
00707 gas_objt.for                   (for) 02326 |            ALLOCATE(TG_2_PLANNING_AREA(MAX_TRANS_GROUPS_FROM_TG))|
00708 gas_objt.for                   (for) 02327 |            ALLOCATE(REV_CLASS_INDEX(0:MAX_TRANS_LOAD_TABLES,4))|
00709 gas_objt.for                   (for) 02328 |            ALLOCATE(ENERGY_LOSS_MULT(MAX_TRANS_LOAD_TABLES))|
00710 gas_objt.for                   (for) 02329 |            ALLOCATE(REF_LEAP_YEAR_DAY_SHIFT(MAX_TRANS_LOAD_TABLES))|
00711 gas_objt.for                   (for) 02330 |            ALLOCATE(DEMAND_PRICING_METHOD(MAX_TRANS_LOAD_TABLES))|
00712 gas_objt.for                   (for) 02331 |            ALLOCATE(MONTHLY_TABLE_PEAK_SALES(MAX_TRANS_LOAD_TABLES,12))|
00713 gas_objt.for                   (for) 02332 |            ALLOCATE(INTRA_COMPANY_TRANSACTION(MAX_TRANS_LOAD_TABLES))|
00714 gas_objt.for                   (for) 02333 |            ALLOCATE(INTRA_ASSET_CLASS_ID(MAX_TRANS_LOAD_TABLES))|
00715 gas_objt.for                   (for) 02334 |            ALLOCATE(INTRA_ASSET_CLASS_ALLOCATION(|
00716 gas_objt.for                   (for) 02336 |            ALLOCATE(INTRA_ACCOUNT_CLASSIFICATION(|
00717 gas_objt.for                   (for) 02338 |            ALLOCATE(INTRA_EXPENSE_COLLECTION(MAX_TRANS_LOAD_TABLES))|
00718 gas_objt.for                   (for) 02344 |         ALLOCATE(TARGET_DAILY_STORAGE(0:12,0:MAX_TRANS_LOAD_TABLES),|
00719 gas_objt.for                   (for) 02407 |         ALLOCATE(MONTHLY_INTERRUPTIBLE_REVENUE(|
00720 gas_objt.for                   (for) 02443 |            ALLOCATE(LAST_THIS_YR_ENERGY(2,12,0:MAX_TRANS_LOAD_TABLES)) ! 1=LAST_YEAR, 2=THIS YEAR|
00721 gas_objt.for                   (for) 02444 |            ALLOCATE(LAST_THIS_YR_PEAK(2,12,0:MAX_TRANS_LOAD_TABLES))|
00722 gas_objt.for                   (for) 03234 |         ALLOCATE(MONTHLY_AC_COST_AT_MARKET(|
00723 gas_objt.for                   (for) 03236 |         ALLOCATE(MONTHLY_AC_CONTRACT_REVENUE(|
00724 gas_objt.for                   (for) 03239 |         ALLOCATE(MONTHLY_AC_CONTRACT_EXPENSE(|
00725 gas_objt.for                   (for) 03308 |         ALLOCATE(DAY_OF_WEEK(31,0:MAX_TRANS_LOAD_TABLES))|
00726 gas_objt.for                   (for) 03320 |            ALLOCATE(|
00727 gas_objt.for                   (for) 03322 |            ALLOCATE(|
00728 gas_objt.for                   (for) 03324 |            ALLOCATE(|
00729 gas_objt.for                   (for) 03326 |            ALLOCATE(|
00730 gas_objt.for                   (for) 03328 |            ALLOCATE(|
00731 gas_objt.for                   (for) 03330 |            ALLOCATE(|
00732 gas_objt.for                   (for) 03332 |            ALLOCATE(|
00733 gas_objt.for                   (for) 03380 |         ALLOCATE(TRANS_HOURLY_LOAD(|
00734 gas_objt.for                   (for) 03382 |         ALLOCATE(TRANS_DAILY_LOAD(|
00735 gas_objt.for                   (for) 03384 |         ALLOCATE(INTER_DAILY_AVAIL(|
00736 gas_objt.for                   (for) 03386 |         ALLOCATE(CUSTOMER_DAILY_LOAD(|
00737 gas_objt.for                   (for) 03388 |         ALLOCATE(WH_LOADS_PER_HOUR(|
00738 gas_objt.for                   (for) 03390 |         ALLOCATE(HYDRO_HOURLY_LOAD( ! NEED TO COUNT HYDRO GROUPS|
00739 gas_objt.for                   (for) 03392 |         ALLOCATE(TABLE_HOURLY_LOAD(|
00740 gas_objt.for                   (for) 03394 |         ALLOCATE(MONTHLY_TRANS_ENERGY(0:MAX_GAS_LOAD_GROUPS))|
00741 gas_objt.for                   (for) 03395 |         ALLOCATE(MONTHLY_HYDRO_ENERGY(0:NUMBER_OF_HYDRO_GROUPS))|
00742 gas_objt.for                   (for) 03396 |         ALLOCATE(MONTHLY_TRANS_PEAK(0:MAX_GAS_LOAD_GROUPS))|
00743 gas_objt.for                   (for) 03397 |         ALLOCATE(MONTHLY_TRANS_BASE(0:MAX_GAS_LOAD_GROUPS))|
00744 gas_objt.for                   (for) 03399 |         ALLOCATE(MONTHLY_HYDRO_PEAK(0:NUMBER_OF_HYDRO_GROUPS))|
00745 gas_objt.for                   (for) 03400 |         ALLOCATE(MONTHLY_HYDRO_BASE(0:NUMBER_OF_HYDRO_GROUPS))|
00746 gas_objt.for                   (for) 03402 |         ALLOCATE(MONTHLY_TABLE_ENERGY(0:MAX_TRANS_LOAD_TABLES))|
00747 gas_objt.for                   (for) 03403 |         ALLOCATE(MONTHLY_TABLE_SALES_ENERGY(|
00748 gas_objt.for                   (for) 03405 |         ALLOCATE(MONTHLY_TABLE_PEAK(0:MAX_TRANS_LOAD_TABLES))|
00749 gas_objt.for                   (for) 03406 |         ALLOCATE(TABLE_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES))|
00750 gas_objt.for                   (for) 03407 |         ALLOCATE(TABLE_ENERGY_REVENUE(0:MAX_TRANS_LOAD_TABLES))|
00751 gas_objt.for                   (for) 03408 |         ALLOCATE(TABLE_DEMAND_REVENUE(0:MAX_TRANS_LOAD_TABLES))|
00752 gas_objt.for                   (for) 03409 |         ALLOCATE(TABLE_CUSTOMER_REVENUE(0:MAX_TRANS_LOAD_TABLES))|
00753 gas_objt.for                   (for) 03411 |         ALLOCATE(TRANS_ENERGY_REVENUE(0:MAX_GAS_LOAD_GROUPS))|
00754 gas_objt.for                   (for) 03412 |         ALLOCATE(TRANS_DEMAND_REVENUE(0:MAX_GAS_LOAD_GROUPS))|
00755 gas_objt.for                   (for) 03413 |         ALLOCATE(TRANS_CUSTOMER_REVENUE(0:MAX_GAS_LOAD_GROUPS))|
00756 gas_objt.for                   (for) 03415 |         ALLOCATE(CLASS_ENERGY_REVENUE(0:MAX_CUST_CLASS_GROUPS))|
00757 gas_objt.for                   (for) 03416 |         ALLOCATE(TRANS_INDEXED_REVENUE(0:MAX_CUST_CLASS_GROUPS))|
00758 gas_objt.for                   (for) 03418 |         ALLOCATE(CLASS_PEAK_REVENUE(0:MAX_CUST_CLASS_GROUPS))|
00759 gas_objt.for                   (for) 03419 |         ALLOCATE(CLASS_CUSTOMER_REVENUE(0:MAX_CUST_CLASS_GROUPS))|
00760 gas_objt.for                   (for) 03420 |         ALLOCATE(MONTHLY_CLASS_ENERGY(0:MAX_CUST_CLASS_GROUPS))|
00761 gas_objt.for                   (for) 03421 |         ALLOCATE(MONTHLY_CLASS_PEAK(0:MAX_CUST_CLASS_GROUPS))|
00762 gas_objt.for                   (for) 03422 |         ALLOCATE(MONTHLY_CLASS_CUSTOMERS(0:MAX_CUST_CLASS_GROUPS))|
00763 gas_objt.for                   (for) 03423 |         ALLOCATE(LOCAL_CUSTOMER_NAME(0:MAX_CUST_CLASS_GROUPS))|
00764 gas_objt.for                   (for) 03424 |         ALLOCATE(ASSET_CLASS_HOURLY_LOAD(MAX_HOURS_IN_MONTH,|
00765 gas_objt.for                   (for) 03497 |         ALLOCATE (WH_TRANS_ALLOC(0:MAX_TRANS_LOAD_TABLES,|
00766 gas_objt.for                   (for) 06843 |         ALLOCATE(SUPPLY_GROUP_AVAILABLE(MAX_GAS_GROUP_INDEX))|
00767 gas_objt.for                   (for) 06844 |         ALLOCATE(GS_GROUP_INDEX(MAX_GAS_GROUP_INDEX))|
00768 gas_objt.for                   (for) 06845 |         ALLOCATE(GS_SCENARIO_VARIABLE_INDEX(MAX_GAS_GROUP_INDEX))|
00769 gas_objt.for                   (for) 06846 |         ALLOCATE(HYDRO_GROUP_2_GS(MAX_GAS_GROUP_INDEX))|
00770 gas_objt.for                   (for) 06848 |         ALLOCATE(GAS_GROUP_POSITION(MAX_GAS_GROUP_INDEX))|
00771 gas_objt.for                   (for) 06849 |         ALLOCATE(REPORT_THIS_GROUP(MAX_GAS_GROUP_INDEX))|
00772 gas_objt.for                   (for) 06850 |         ALLOCATE(GS_2_HYDRO_GROUP(MAX_GAS_GROUP_INDEX))|
00773 gas_objt.for                   (for) 06851 |         ALLOCATE(GS_2_PLANNING_AREA(MAX_GAS_GROUP_INDEX))|
00774 gas_objt.for                   (for) 06852 |         ALLOCATE(HYDRO_AGGREGATION_POSITION(MAX_GAS_GROUP_INDEX))|
00775 gas_objt.for                   (for) 06853 |         ALLOCATE(HYDRO_AGGREGATION_INDEX(MAX_GAS_GROUP_INDEX))|
00776 gas_objt.for                   (for) 06854 |         ALLOCATE(PLANNING_AREA_POSITION(MAX_GAS_GROUP_INDEX))|
00777 gas_objt.for                   (for) 06855 |         ALLOCATE(PLANNING_AREA_INDEX(MAX_GAS_GROUP_INDEX))|
00778 gas_objt.for                   (for) 06856 |         ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))|
00779 gas_objt.for                   (for) 06857 |         ALLOCATE(ASSET_CLASS_2_GS(0:MAX_ASSET_GROUPS))|
00780 gas_objt.for                   (for) 06858 |         ALLOCATE(ASSET_CLASS_GROUP_2_AC(MAX_GAS_GROUP_INDEX))|
00781 gas_objt.for                   (for) 06958 |         ALLOCATE(GROUP_NAME(GAS_GROUPS_RECORDS))|
00782 gas_objt.for                   (for) 06959 |         ALLOCATE(GROUP_ACTIVE(GAS_GROUPS_RECORDS))|
00783 gas_objt.for                   (for) 06960 |         ALLOCATE(GAS_GROUP_ID(MAX_GAS_GROUP_INDEX))|
00784 gas_objt.for                   (for) 06961 |         ALLOCATE(GAS_GROUP_ID_POSITION(0:GAS_GROUPS_RECORDS))|
00785 gas_objt.for                   (for) 06962 |         ALLOCATE(BASECASE_MARKET_AREA_ID(GAS_GROUPS_RECORDS))|
00786 gas_objt.for                   (for) 06963 |         ALLOCATE(BASE_CASE_GAS_AREA_ID(GAS_GROUPS_RECORDS))|
00787 gas_objt.for                   (for) 06964 |         ALLOCATE(BASECASE_SUBREGION_ID(GAS_GROUPS_RECORDS))|
00788 gas_objt.for                   (for) 06965 |         ALLOCATE(SPINNING_UNITS(GAS_GROUPS_RECORDS))|
00789 gas_objt.for                   (for) 06966 |         ALLOCATE(OFF_PEAK_SPINNING_UNITS(GAS_GROUPS_RECORDS))|
00790 gas_objt.for                   (for) 06967 |         ALLOCATE(SPINNING_RESERVE(GAS_GROUPS_RECORDS))|
00791 gas_objt.for                   (for) 06968 |         ALLOCATE(OFF_PEAK_SPINNING_RESERVE(GAS_GROUPS_RECORDS))|
00792 gas_objt.for                   (for) 06969 |         ALLOCATE(MAX_HOURLY_RAMP_UP(GAS_GROUPS_RECORDS))|
00793 gas_objt.for                   (for) 06970 |         ALLOCATE(MAX_HOURLY_RAMP_DOWN(GAS_GROUPS_RECORDS))|
00794 gas_objt.for                   (for) 06971 |         ALLOCATE(FIRST_CAPACITY_VALUE(0:GAS_GROUPS_RECORDS))|
00795 gas_objt.for                   (for) 06972 |         ALLOCATE(FIRST_CAPACITY_PERCENT(0:GAS_GROUPS_RECORDS))|
00796 gas_objt.for                   (for) 06973 |         ALLOCATE(SECOND_CAPACITY_VALUE(0:GAS_GROUPS_RECORDS))|
00797 gas_objt.for                   (for) 06974 |         ALLOCATE(SECOND_CAPACITY_PERCENT(0:GAS_GROUPS_RECORDS))|
00798 gas_objt.for                   (for) 06975 |         ALLOCATE(THIRD_CAPACITY_VALUE(0:GAS_GROUPS_RECORDS))|
00799 gas_objt.for                   (for) 06976 |         ALLOCATE(THIRD_CAPACITY_PERCENT(0:GAS_GROUPS_RECORDS))|
00800 gas_objt.for                   (for) 06977 |         ALLOCATE(ADDITIONAL_CAPACITY_VALUE(0:GAS_GROUPS_RECORDS,7))|
00801 gas_objt.for                   (for) 06978 |         ALLOCATE(ADDITIONAL_CAPACITY_PERCENT(0:GAS_GROUPS_RECORDS,7))|
00802 gas_objt.for                   (for) 06979 |         ALLOCATE(RTO_GROUP(GAS_GROUPS_RECORDS))|
00803 gas_objt.for                   (for) 06980 |         ALLOCATE(NOX_YEAR(GAS_GROUPS_RECORDS))|
00804 gas_objt.for                   (for) 06981 |         ALLOCATE(END_NOX_YEAR(GAS_GROUPS_RECORDS))|
00805 gas_objt.for                   (for) 06982 |         ALLOCATE(BASIN_START_YEAR(GAS_GROUPS_RECORDS))|
00806 gas_objt.for                   (for) 06983 |         ALLOCATE(ST_LHS_FOR_PRICES(GAS_GROUPS_RECORDS))|
00807 gas_objt.for                   (for) 06984 |         ALLOCATE(MRX_VOLATILITY_MULT(GAS_GROUPS_RECORDS))|
00808 gas_objt.for                   (for) 06985 |         ALLOCATE(NIGHT_SCARCITY_MULT(GAS_GROUPS_RECORDS))|
00809 gas_objt.for                   (for) 06986 |         ALLOCATE(WEEKEND_SCARCITY_MULT(GAS_GROUPS_RECORDS))|
00810 gas_objt.for                   (for) 06987 |         ALLOCATE(PRICE_CAP(GAS_GROUPS_RECORDS))|
00811 gas_objt.for                   (for) 06988 |         ALLOCATE(MAX_HOURLY_GS_IMPORT(GAS_GROUPS_RECORDS))|
00812 gas_objt.for                   (for) 06989 |         ALLOCATE(MAX_HOURLY_GS_EXPORT(GAS_GROUPS_RECORDS))|
00813 gas_objt.for                   (for) 06990 |         ALLOCATE(HYDRO_LOAD_AGGREGATION(GAS_GROUPS_RECORDS))|
00814 gas_objt.for                   (for) 06991 |         ALLOCATE(GS_REGIONAL_PLANNING_AREA(GAS_GROUPS_RECORDS))|
00815 gas_objt.for                   (for) 06992 |         ALLOCATE(REPORT_CL_CAPACITY(GAS_GROUPS_RECORDS))|
00816 gas_objt.for                   (for) 06993 |         ALLOCATE(ASSET_CLASS_ID(GAS_GROUPS_RECORDS))|
00817 gas_objt.for                   (for) 06994 |         ALLOCATE(ASSET_CLASS_REV_ALLOC_VECTOR(GAS_GROUPS_RECORDS))|
00818 gas_objt.for                   (for) 06995 |         ALLOCATE(TIME_ZONE(GAS_GROUPS_RECORDS))|
00819 gas_objt.for                   (for) 06996 |         ALLOCATE(CAPACITY_ADDER(GAS_GROUPS_RECORDS))|
00820 gas_objt.for                   (for) 06997 |         ALLOCATE(NOX_SEASON(GAS_GROUPS_RECORDS))|
00821 gas_objt.for                   (for) 06998 |         ALLOCATE(PURCHASE_POWER_ASSIGN(GAS_GROUPS_RECORDS))|
00822 gas_objt.for                   (for) 06999 |         ALLOCATE(CREATE_HOURLY_PRICE(GAS_GROUPS_RECORDS))|
00823 gas_objt.for                   (for) 07000 |         ALLOCATE(SUPPLY_TYPE(GAS_GROUPS_RECORDS))|
00824 gas_objt.for                   (for) 07001 |         ALLOCATE(HOURLY_PRICE_NAME(GAS_GROUPS_RECORDS))|
00825 gas_objt.for                   (for) 07002 |         ALLOCATE(PURCHASE_ASSET_CLASS_ID(GAS_GROUPS_RECORDS))|
00826 gas_objt.for                   (for) 07003 |         ALLOCATE(PURCHASE_ASSET_ALLOC_VECTOR(GAS_GROUPS_RECORDS))|
00827 gas_objt.for                   (for) 07004 |         ALLOCATE(OPERATING_COSTS(30,GAS_GROUPS_RECORDS)) ! (25,GAS_GROUPS_RECORDS))|
00828 gas_objt.for                   (for) 07005 |         ALLOCATE(CAPACITY_VALUES(30,GAS_GROUPS_RECORDS)) ! (25,GAS_GROUPS_RECORDS))|
00829 gas_objt.for                   (for) 07006 |         ALLOCATE(INT_RES_REMAIN(30,GAS_GROUPS_RECORDS)) ! (25,GAS_GROUPS_RECORDS))|
00830 gas_objt.for                   (for) 07007 |         ALLOCATE(POTENTIAL_RESERVES(GAS_GROUPS_RECORDS))|
00831 gas_objt.for                   (for) 07008 |         ALLOCATE(PROVEN_RESERVES(GAS_GROUPS_RECORDS))|
00832 gas_objt.for                   (for) 07009 |         ALLOCATE(MAX_DAILY_EXTRACTION_RATE(GAS_GROUPS_RECORDS))|
00833 gas_objt.for                   (for) 07010 |         ALLOCATE(MAX_DAILY_EXTRACTION_IN_MONTH(GAS_GROUPS_RECORDS))|
00834 gas_objt.for                   (for) 07011 |         ALLOCATE(MAX_MONTHLY_EXTRACTION_RATE(GAS_GROUPS_RECORDS))|
00835 gas_objt.for                   (for) 07012 |         ALLOCATE(SHRINKAGE_PERCENT(GAS_GROUPS_RECORDS))|
00836 gas_objt.for                   (for) 07013 |         ALLOCATE(SHRINKAGE_COST(GAS_GROUPS_RECORDS)) |
00837 gas_objt.for                   (for) 07014 |         ALLOCATE(EXTRACTION_COST_ESC_RATE(GAS_GROUPS_RECORDS))|
00838 gas_objt.for                   (for) 07015 |         ALLOCATE(RESERVE_APPRECIATION_PERCENT(GAS_GROUPS_RECORDS))|
00839 gas_objt.for                   (for) 07016 |         ALLOCATE(CUMULATIVE_RESERVE_IMPACT(GAS_GROUPS_RECORDS))|
00840 gas_objt.for                   (for) 07017 |         ALLOCATE(MAX_DAILY_HARD_LIMIT(GAS_GROUPS_RECORDS))|
00841 gas_objt.for                   (for) 07018 |         ALLOCATE(MIN_DAILY_LIMIT(GAS_GROUPS_RECORDS))|
00842 gas_objt.for                   (for) 07019 |         ALLOCATE(DYNAMIC_EXTRACT_RATE(GAS_GROUPS_RECORDS))|
00843 gas_objt.for                   (for) 07020 |         ALLOCATE(MIN_BASIN_COST(GAS_GROUPS_RECORDS))|
00844 gas_objt.for                   (for) 07021 |         ALLOCATE(X_SUPPLY_CURVE_COEFFICIENT(GAS_GROUPS_RECORDS))|
00845 gas_objt.for                   (for) 07022 |         ALLOCATE(X2_SUPPLY_CURVE_COEFFICIENT(GAS_GROUPS_RECORDS))|
00846 gas_objt.for                   (for) 07023 |         ALLOCATE(X2_SUPPLY_CURVE_INTERCEPT(GAS_GROUPS_RECORDS))|
00847 gas_objt.for                   (for) 07024 |         ALLOCATE(SUPPLY_CURVE_MULT(GAS_GROUPS_RECORDS))|
00848 gas_objt.for                   (for) 07025 |         ALLOCATE(SUPPLY_BASIS_DIFFERENTIAL(0:GAS_GROUPS_RECORDS))|
00849 gas_objt.for                   (for) 07026 |         ALLOCATE(DYN_SUPPLY_BASIS_DIFF(0:GAS_GROUPS_RECORDS))|
00850 gas_objt.for                   (for) 07028 |         ALLOCATE(ANNUAL_SUPPLY_QUANTITY(0:GAS_GROUPS_RECORDS))|
00851 gas_objt.for                   (for) 07029 |         ALLOCATE(ANNUAL_SUPPLY_PRICE(0:GAS_GROUPS_RECORDS))|
00852 gas_objt.for                   (for) 07425 |            ALLOCATE(FIRST_GS(GS),|
00853 gas_objt.for                   (for) 07476 |         ALLOCATE(TEMP_GS_SUPPLY_CAPACITY_CURVE(MAX_POINTS_IN_GS),|
00854 gas_objt.for                   (for) 07490 |         ALLOCATE(   GS_SUPPLY_CAPACITY_CURVE(MAX_POINTS_IN_GS,GS),|
00855 gas_objt.for                   (for) 09331 |         ALLOCATE(Storage_Name(MAX_OUT_GAS_STORAGE),|
00856 gas_objt.for                   (for) 10520 |         ALLOCATE(TRANS_GROUP_POSITION(-1:MAX_GAS_GROUP_NUMBER),|
00857 gas_objt.for                   (for) 10906 |         ALLOCATE(|
00858 gas_objt.for                   (for) 12563 |         ALLOCATE(GAS_GROUP_INDEX(MAX_GAS_GROUP_INDEX))|
00859 gas_objt.for                   (for) 12564 |         ALLOCATE(GN_SCENARIO_VARIABLE_INDEX(MAX_GAS_GROUP_INDEX))|
00860 gas_objt.for                   (for) 12565 |         ALLOCATE(HYDRO_GROUP_2_GN(MAX_GAS_GROUP_INDEX))|
00861 gas_objt.for                   (for) 12567 |         ALLOCATE(GAS_GROUP_POSITION(MAX_GAS_GROUP_INDEX))|
00862 gas_objt.for                   (for) 12568 |         ALLOCATE(GAS_GROUP_STATE_POSITION(MAX_GAS_GROUP_INDEX))|
00863 gas_objt.for                   (for) 12569 |         ALLOCATE(NODE_FROM_STATE_POSITION(MAX_GAS_REGION_NO))|
00864 gas_objt.for                   (for) 12570 |         ALLOCATE(REPORT_THIS_GROUP(MAX_GAS_GROUP_INDEX))|
00865 gas_objt.for                   (for) 12571 |         ALLOCATE(GN_2_HYDRO_GROUP(MAX_GAS_GROUP_INDEX))|
00866 gas_objt.for                   (for) 12572 |         ALLOCATE(GN_2_PLANNING_AREA(MAX_GAS_GROUP_INDEX))|
00867 gas_objt.for                   (for) 12573 |         ALLOCATE(HYDRO_AGGREGATION_POSITION(MAX_GAS_GROUP_INDEX))|
00868 gas_objt.for                   (for) 12574 |         ALLOCATE(HYDRO_AGGREGATION_INDEX(MAX_GAS_GROUP_INDEX))|
00869 gas_objt.for                   (for) 12575 |         ALLOCATE(PLANNING_AREA_POSITION(MAX_GAS_GROUP_INDEX))|
00870 gas_objt.for                   (for) 12576 |         ALLOCATE(PLANNING_AREA_INDEX(MAX_GAS_GROUP_INDEX))|
00871 gas_objt.for                   (for) 12577 |         ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))|
00872 gas_objt.for                   (for) 12578 |         ALLOCATE(ASSET_CLASS_2_GN(0:MAX_ASSET_GROUPS))|
00873 gas_objt.for                   (for) 12579 |         ALLOCATE(ASSET_CLASS_GROUP_2_AC(MAX_GAS_GROUP_INDEX))|
00874 gas_objt.for                   (for) 12670 |         ALLOCATE(GROUP_NAME(GAS_GROUPS_RECORDS))|
00875 gas_objt.for                   (for) 12671 |         ALLOCATE(iPort(GAS_GROUPS_RECORDS),|
00876 gas_objt.for                   (for) 12673 |         ALLOCATE(WORLD_DATABASE_INDEX(GAS_GROUPS_RECORDS))|
00877 gas_objt.for                   (for) 12674 |         ALLOCATE(GROUP_ACTIVE(GAS_GROUPS_RECORDS))|
00878 gas_objt.for                   (for) 12675 |         ALLOCATE(GAS_NODE_GROUP(GAS_GROUPS_RECORDS))|
00879 gas_objt.for                   (for) 12676 |         ALLOCATE(GAS_ID_2_GSP(GAS_GROUPS_RECORDS))|
00880 gas_objt.for                   (for) 12677 |         ALLOCATE(COUNTRY_NODE_INDEX(MAX_GAS_GROUP_INDEX))|
00881 gas_objt.for                   (for) 12678 |         ALLOCATE(BASECASE_MARKET_AREA_ID(GAS_GROUPS_RECORDS))|
00882 gas_objt.for                   (for) 12679 |         ALLOCATE(BASE_CASE_GAS_AREA_ID(GAS_GROUPS_RECORDS))|
00883 gas_objt.for                   (for) 12680 |         ALLOCATE(BASECASE_SUBREGION_ID(GAS_GROUPS_RECORDS))|
00884 gas_objt.for                   (for) 12681 |         ALLOCATE(SPINNING_UNITS(GAS_GROUPS_RECORDS))|
00885 gas_objt.for                   (for) 12682 |         ALLOCATE(OFF_PEAK_SPINNING_UNITS(GAS_GROUPS_RECORDS))|
00886 gas_objt.for                   (for) 12683 |         ALLOCATE(SPINNING_RESERVE(GAS_GROUPS_RECORDS))|
00887 gas_objt.for                   (for) 12684 |         ALLOCATE(OFF_PEAK_SPINNING_RESERVE(GAS_GROUPS_RECORDS))|
00888 gas_objt.for                   (for) 12685 |         ALLOCATE(MAX_HOURLY_RAMP_UP(GAS_GROUPS_RECORDS))|
00889 gas_objt.for                   (for) 12686 |         ALLOCATE(MAX_HOURLY_RAMP_DOWN(GAS_GROUPS_RECORDS))|
00890 gas_objt.for                   (for) 12687 |         ALLOCATE(FIRST_CAPACITY_VALUE(0:GAS_GROUPS_RECORDS))|
00891 gas_objt.for                   (for) 12688 |         ALLOCATE(FIRST_CAPACITY_PERCENT(0:GAS_GROUPS_RECORDS))|
00892 gas_objt.for                   (for) 12689 |         ALLOCATE(SECOND_CAPACITY_VALUE(0:GAS_GROUPS_RECORDS))|
00893 gas_objt.for                   (for) 12690 |         ALLOCATE(SECOND_CAPACITY_PERCENT(0:GAS_GROUPS_RECORDS))|
00894 gas_objt.for                   (for) 12691 |         ALLOCATE(THIRD_CAPACITY_VALUE(0:GAS_GROUPS_RECORDS))|
00895 gas_objt.for                   (for) 12692 |         ALLOCATE(THIRD_CAPACITY_PERCENT(0:GAS_GROUPS_RECORDS))|
00896 gas_objt.for                   (for) 12693 |         ALLOCATE(ADDITIONAL_CAPACITY_VALUE(0:GAS_GROUPS_RECORDS,7))|
00897 gas_objt.for                   (for) 12694 |         ALLOCATE(ADDITIONAL_CAPACITY_PERCENT(0:GAS_GROUPS_RECORDS,7))|
00898 gas_objt.for                   (for) 12695 |         ALLOCATE(RTO_GROUP(GAS_GROUPS_RECORDS))|
00899 gas_objt.for                   (for) 12696 |         ALLOCATE(NOX_YEAR(GAS_GROUPS_RECORDS))|
00900 gas_objt.for                   (for) 12697 |         ALLOCATE(END_NOX_YEAR(GAS_GROUPS_RECORDS))|
00901 gas_objt.for                   (for) 12698 |         ALLOCATE(ST_LHS_FOR_PRICES(GAS_GROUPS_RECORDS))|
00902 gas_objt.for                   (for) 12699 |         ALLOCATE(GAS_NODE_LATITUDE(GAS_GROUPS_RECORDS))|
00903 gas_objt.for                   (for) 12700 |         ALLOCATE(GAS_NODE_LONGITUDE(GAS_GROUPS_RECORDS))|
00904 gas_objt.for                   (for) 12702 |         ALLOCATE(GAS_NODE_STATE_PROVINCE(MAX_GAS_GROUP_INDEX)) |
00905 gas_objt.for                   (for) 12703 |         ALLOCATE(GAS_NODE_COUNTRY(MAX_GAS_GROUP_INDEX))|
00906 gas_objt.for                   (for) 12704 |         ALLOCATE(GAS_STORAGE_ALLOC_FACTOR(MAX_GAS_GROUP_INDEX))|
00907 gas_objt.for                   (for) 12705 |         ALLOCATE(GAS_NODE_TYPE_INDEX(MAX_GAS_GROUP_INDEX))|
00908 gas_objt.for                   (for) 12706 |         ALLOCATE(GAS_STORAGE_INDEX(MAX_GAS_GROUP_INDEX,2))|
00909 gas_objt.for                   (for) 12707 |         ALLOCATE(GAS_STORAGE_POSITION(700,2))|
00910 gas_objt.for                   (for) 12708 |         ALLOCATE(GAS_HUB_REFERENCE_INDEX(MAX_GAS_GROUP_INDEX))|
00911 gas_objt.for                   (for) 12710 |         ALLOCATE(MRX_VOLATILITY_MULT(GAS_GROUPS_RECORDS))|
00912 gas_objt.for                   (for) 12711 |         ALLOCATE(NIGHT_SCARCITY_MULT(GAS_GROUPS_RECORDS))|
00913 gas_objt.for                   (for) 12712 |         ALLOCATE(WEEKEND_SCARCITY_MULT(GAS_GROUPS_RECORDS))|
00914 gas_objt.for                   (for) 12713 |         ALLOCATE(PRICE_CAP(GAS_GROUPS_RECORDS))|
00915 gas_objt.for                   (for) 12714 |         ALLOCATE(MAX_HOURLY_GN_IMPORT(GAS_GROUPS_RECORDS))|
00916 gas_objt.for                   (for) 12715 |         ALLOCATE(MAX_HOURLY_GN_EXPORT(GAS_GROUPS_RECORDS))|
00917 gas_objt.for                   (for) 12716 |         ALLOCATE(HYDRO_LOAD_AGGREGATION(GAS_GROUPS_RECORDS))|
00918 gas_objt.for                   (for) 12717 |         ALLOCATE(GN_REGIONAL_PLANNING_AREA(GAS_GROUPS_RECORDS))|
00919 gas_objt.for                   (for) 12718 |         ALLOCATE(REPORT_CL_CAPACITY(GAS_GROUPS_RECORDS))|
00920 gas_objt.for                   (for) 12719 |         ALLOCATE(ASSET_CLASS_ID(GAS_GROUPS_RECORDS))|
00921 gas_objt.for                   (for) 12720 |         ALLOCATE(ASSET_CLASS_REV_ALLOC_VECTOR(GAS_GROUPS_RECORDS))|
00922 gas_objt.for                   (for) 12721 |         ALLOCATE(TIME_ZONE(GAS_GROUPS_RECORDS))|
00923 gas_objt.for                   (for) 12722 |         ALLOCATE(CAPACITY_ADDER(GAS_GROUPS_RECORDS))|
00924 gas_objt.for                   (for) 12723 |         ALLOCATE(NOX_SEASON(GAS_GROUPS_RECORDS))|
00925 gas_objt.for                   (for) 12724 |         ALLOCATE(PURCHASE_POWER_ASSIGN(GAS_GROUPS_RECORDS))|
00926 gas_objt.for                   (for) 12725 |         ALLOCATE(CREATE_HOURLY_PRICE(GAS_GROUPS_RECORDS))|
00927 gas_objt.for                   (for) 12726 |         ALLOCATE(HOURLY_PRICE_NAME(GAS_GROUPS_RECORDS))|
00928 gas_objt.for                   (for) 12727 |         ALLOCATE(PURCHASE_ASSET_CLASS_ID(GAS_GROUPS_RECORDS))|
00929 gas_objt.for                   (for) 12728 |         ALLOCATE(PURCHASE_ASSET_ALLOC_VECTOR(GAS_GROUPS_RECORDS))|
00930 gas_objt.for                   (for) 12729 |         ALLOCATE(OPERATING_COSTS(25,GAS_GROUPS_RECORDS))|
00931 gas_objt.for                   (for) 12730 |         ALLOCATE(CAPACITY_VALUES(25,GAS_GROUPS_RECORDS))|
00932 gas_objt.for                   (for) 12731 |         ALLOCATE(CUM_RES_REMAIN(25,GAS_GROUPS_RECORDS))|
00933 gas_objt.for                   (for) 12732 |         ALLOCATE(POTENTIAL_RESERVES(GAS_GROUPS_RECORDS))|
00934 gas_objt.for                   (for) 12733 |         ALLOCATE(PROVEN_RESERVES(GAS_GROUPS_RECORDS))|
00935 gas_objt.for                   (for) 12734 |         ALLOCATE(MAX_DAILY_EXTRACTION_RATE(GAS_GROUPS_RECORDS))|
00936 gas_objt.for                   (for) 12735 |         ALLOCATE(MAX_MONTHLY_EXTRACTION_RATE(GAS_GROUPS_RECORDS))|
00937 gas_objt.for                   (for) 12736 |         ALLOCATE(SHRINKAGE_PERCENT(GAS_GROUPS_RECORDS))|
00938 gas_objt.for                   (for) 12737 |         ALLOCATE(SHRINKAGE_COST(GAS_GROUPS_RECORDS)) |
00939 gas_objt.for                   (for) 14241 |         ALLOCATE(GS_POS(MAX_NODE_ID),GT_POS(MAX_LINK_ID))|
00940 gas_objt.for                   (for) 14271 |         ALLOCATE(   LNG_SCENARIO_PRICE(NUM_LNG,12,30),|
00941 gas_objt.for                   (for) 14344 |         ALLOCATE(ANNUAL_NODE_PRICE(MAX_NODES),|
00942 gas_objt.for                   (for) 14358 |         ALLOCATE(MONTHLY_GAS_BALANCE(|
00943 gas_objt.for                   (for) 14430 |         allocate (|
00944 gas_objt.for                   (for) 15773 |         ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023),|
00945 gas_objt.for                   (for) 15929 |      ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023),|
00946 gas_objt.for                   (for) 16203 |            ALLOCATE(|
00947 gas_objt.for                   (for) 16260 |            ALLOCATE(|
00948 gas_objt.for                   (for) 16687 |      allocate(NodeID  (nNode))|
00949 gas_objt.for                   (for) 16688 |      allocate(NodeType(nNode))|
00950 gas_objt.for                   (for) 16689 |      allocate(NodeQtyLoB(nNode)) ! min quantity at supply MMCF/D|
00951 gas_objt.for                   (for) 16690 |      allocate(NodeQtyLim(nNode)) ! max quantity at supply MMCF/D|
00952 gas_objt.for                   (for) 16691 |      allocate(NodeQtyHardLim(nNode)) ! max quantity at supply MMCF/D|
00953 gas_objt.for                   (for) 16692 |      allocate(NodeQty (nNode)) ! quantity demand or supply MMCF/D|
00954 gas_objt.for                   (for) 16693 |      allocate(NodeQtyOfLR(nNode)) ! to/from LastResort; useful only if AmortizeUE|
00955 gas_objt.for                   (for) 16694 |      allocate(NodePri (nNode)) ! $/MCF|
00956 gas_objt.for                   (for) 16695 |      allocate(NodeMgC (nNode)) ! $/MCF|
00957 gas_objt.for                   (for) 16696 |      allocate(NodeName(nNode))|
00958 gas_objt.for                   (for) 16711 |      allocate(LinkID (nLink))|
00959 gas_objt.for                   (for) 16712 |      allocate(LinkCap(nLink)) ! A-to-B capacity (nominal 100%)|
00960 gas_objt.for                   (for) 16713 |      allocate(LinkChg(nLink)) ! A-to-B commodity charge at 0%|
00961 gas_objt.for                   (for) 16714 |      allocate(LinkEff(nLink)) ! A-to_B fractional retention of inlet quantity|
00962 gas_objt.for                   (for) 16715 |      allocate(LinkAvC(nLink)) ! weighted average cost $/MCF|
00963 gas_objt.for                   (for) 16716 |      allocate(LinkMgC(nLink)) ! marginal cost $/MCF|
00964 gas_objt.for                   (for) 16717 |      allocate(LinkNdID(0:1,nLink)) ! A-to-B IDs|
00965 gas_objt.for                   (for) 16718 |      allocate(LinkNdSN(0:1,nLink)) ! A-to-B IDs' serial numbers (contiguous)|
00966 gas_objt.for                   (for) 16719 |      allocate(LinkQty(nLink)) ! A-to-B quantity at inlet in MMCF|
00967 gas_objt.for                   (for) 16720 |      allocate(LinkQOF(nLink)) ! A-to-B over-limit quantity at inlet in MMCF|
00968 gas_objt.for                   (for) 16721 |      allocate(LinkExpQty(nLink)) ! economic expansion quantity|
00969 gas_objt.for                   (for) 16722 |      allocate(LinkQtySeg(nCCSegs,nLink)) ! segment usage in MMCF|
00970 gas_objt.for                   (for) 16723 |      allocate(LinkAvQSeg(nCCSegs,nLink)) ! segment usage in MMCF, accumulated|
00971 gas_objt.for                   (for) 16724 |      allocate(LinkFunc(nCCSegs,0:2,nLink)) ! segment x(EoS fract cap),y($/MCF),DeltaX|
00972 gas_objt.for                   (for) 17359 |      allocate(NodeSNofDFRank(nNode))|
00973 gas_objt.for                   (for) 17441 |      allocate(DNodName(nNode))|
00974 gas_objt.for                   (for) 17511 |      allocate(LinkExpCst(nLink))|
00975 gas_objt.for                   (for) 17512 |      allocate(LinkExpLim(nLink))|
00976 gas_objt.for                   (for) 17513 |      allocate(LinkCCRF  (nLink))|
00977 gas_objt.for                   (for) 17514 |      allocate(DFValue(SupDFRank))|
00978 gas_objt.for                   (for) 17911 |      allocate(LkCstNode(nNode))|
00979 gas_objt.for                   (for) 17912 |      allocate(SpQtyNode(nNode))|
00980 gas_objt.for                   (for) 17913 |      allocate(LkUsdFrom(nNode))|
00981 gas_objt.for                   (for) 17914 |      allocate(nLinkInto(nNode))|
00982 gas_objt.for                   (for) 17915 |      allocate(NdLkdInto(SupnLkAtNd,nNode))|
00983 gas_objt.for                   (for) 17916 |      allocate(LkUsdInto(SupnLkAtNd,nNode))|
00984 gas_objt.for                   (for) 17917 |      allocate(nPriorSegs(nLink))|
00985 gas_objt.for                   (for) 17918 |      allocate(LinkType  (nLink))|
00986 gas_objt.for                   (for) 17919 |      allocate(LinkChgOrg(nLink))|
00987 gas_objt.for                   (for) 17920 |      allocate(LnkQtyP   (nLink))|
00988 gas_objt.for                   (for) 17921 |      allocate(LinkNdBi  (nLink))|
00989 gas_objt.for                   (for) 18209 |      allocate(A4Equiv(nVar))|
00990 gas_objt.for                   (for) 18210 |      allocate(VarDesc(nVar))|
00991 gas_objt.for                   (for) 18211 |      allocate(TNofVar(nTVar))|
00992 gas_objt.for                   (for) 18212 |      allocate(cVect(nVar))|
00993 gas_objt.for                   (for) 18213 |      allocate(xVect(nVar))|
00994 gas_objt.for                   (for) 18214 |      allocate(bVect(mReq))|
00995 gas_objt.for                   (for) 18215 |      allocate(SpVec(mReq))|
00996 gas_objt.for                   (for) 18216 |      allocate(ReVec(mReq))|
00997 gas_objt.for                   (for) 18217 |      allocate(ConsMtxNze(ConsMtSLim))|
00998 gas_objt.for                   (for) 18218 |      allocate(zbColOfNze(ConsMtSLim))|
00999 gas_objt.for                   (for) 18219 |      allocate(zbRowOfNze(ConsMtSLim))|
01000 gas_objt.for                   (for) 18220 |      allocate(jUbVec(nUbV))|
01001 gas_objt.for                   (for) 18221 |      allocate(UbValu(nUbV))|
01002 gas_objt.for                   (for) 18222 |      allocate(UbVOrg(nUbV))|
01003 gas_objt.for                   (for) 18224 |      allocate(jLbVec(nLbV))|
01004 gas_objt.for                   (for) 18225 |      allocate(LbValu(nLbV))|
01005 gas_objt.for                   (for) 18227 |      allocate(NodeSrcNip(mNode,nSNod)) ! number of source paths in cost pool for iNode with iSNod as Supply|
01006 gas_objt.for                   (for) 18228 |      allocate(NodeSrcQty(mNode,nSNod))|
01007 gas_objt.for                   (for) 18229 |      allocate(NodeSrcCst(mNode,nSNod))|
01008 gas_objt.for                   (for) 18230 |      allocate(CycleOrgD(mNode))|
01009 gas_objt.for                   (for) 18474 |        allocate(zbColHold(ConsMtSLim)) ! temporary swap-space|
01010 gas_objt.for                   (for) 18475 |        allocate(zbRowHold(ConsMtSLim))|
01011 gas_objt.for                   (for) 18476 |        allocate(ConsEHold(ConsMtSLim))|
01012 gas_objt.for                   (for) 18483 |        allocate(zbColOfNze(ConsMtSLim))|
01013 gas_objt.for                   (for) 18484 |        allocate(zbRowOfNze(ConsMtSLim))|
01014 gas_objt.for                   (for) 18485 |        allocate(ConsMtxNze(ConsMtSLim))|
01015 grx_planning_routines.f90      (f90) 00180 |               ALLOCATE(SAVED_CL_ANN_CAP(3,STUDY_PERIOD,2))|
01016 grx_planning_routines.f90      (f90) 00185 |              ALLOCATE(SAVED_CL_TG_CAP(0:6,&|
01017 grx_planning_routines.f90      (f90) 00224 |              ALLOCATE(GRX_BOP_RETROFIT_ACTIVE(MAX_CL_UNITS))|
01018 grx_planning_routines.f90      (f90) 00250 |           ALLOCATE(GRX_ITER_CO2_PRICE(0:GRX_MAX_ITERS,0:TG_MAX))|
01019 grx_planning_routines.f90      (f90) 00253 |           ALLOCATE(GRX_ITER_TG_CO2_EMISS(0:GRX_MAX_ITERS,0:TG_MAX))|
01020 grx_planning_routines.f90      (f90) 00257 |           ALLOCATE(GRX_CO2_EMISS_REDUCT_REQUIRED(0:TG_MAX))|
01021 grx_planning_routines.f90      (f90) 00266 |              ALLOCATE(GRX_ITER_CO2_PRICE(0:GRX_MAX_ITERS,0:TG_MAX))|
01022 grx_planning_routines.f90      (f90) 00270 |              ALLOCATE(GRX_ITER_TG_CO2_EMISS(0:GRX_MAX_ITERS,0:TG_MAX))|
01023 grx_planning_routines.f90      (f90) 00274 |              ALLOCATE(GRX_CO2_EMISS_REDUCT_REQUIRED(0:TG_MAX))|
01024 in_objt.f90                    (f90) 00165 |            ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024),SBU_LINKED_COUNTER(1024),BC_CLASS_LINKAGE(0:1024), &|
01025 in_objt.f90                    (f90) 00339 |               ALLOCATE(BC_INIT_CLASS_POINTER(MAX_BC_INIT_CLASS_ID_NUM))|
01026 in_objt.f90                    (f90) 00394 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024),SBU_LINKED_COUNTER(1024),OL_CLASS_LINKAGE(0:1024), &|
01027 in_objt.f90                    (f90) 00527 |            ALLOCATE(OL_INIT_CLASS_POINTER(MAX_OL_INIT_CLASS_ID_NUM))|
01028 lpsolver.f90                   (f90) 00219 |        allocate(BMatrix(0:mSup,0:mSup)    ,stat=AS)|
01029 lpsolver.f90                   (f90) 00221 |        allocate(BMatInv(0:mSup,0:mSup)    ,stat=AS)|
01030 lpsolver.f90                   (f90) 00223 |        allocate(SwapVector (0:mSup)       ,stat=AS)|
01031 lpsolver.f90                   (f90) 00225 |        allocate(Phi(0:mSup)               ,stat=AS)|
01032 lpsolver.f90                   (f90) 00227 |        allocate(yj (0:mSup)               ,stat=AS)|
01033 lpsolver.f90                   (f90) 00229 |        allocate(yk (0:mSup)               ,stat=AS)|
01034 lpsolver.f90                   (f90) 00231 |        allocate(z_c(0:nSup)               ,stat=AS)|
01035 lpsolver.f90                   (f90) 00233 |        allocate(zStar_c(0:nSup)           ,stat=AS)|
01036 lpsolver.f90                   (f90) 00235 |        allocate(zOrg_c(0:nSup)            ,stat=AS)|
01037 lpsolver.f90                   (f90) 00237 |        allocate(GMatrix(0:siRank,0:siRank),stat=AS)|
01038 lpsolver.f90                   (f90) 00239 |        allocate(GMatLUF(0:siRank,0:siRank),stat=AS)|
01039 lpsolver.f90                   (f90) 00274 |        allocate(UpperBounded(0:nSup)    ,stat=AS)|
01040 lpsolver.f90                   (f90) 00276 |        allocate(SymmetricLimits(0:nSup) ,stat=AS)|
01041 lpsolver.f90                   (f90) 00278 |        allocate(LineEnd(0:1,0:siLine)   ,stat=AS)|
01042 lpsolver.f90                   (f90) 00280 |        allocate(BasisCol(0:mSup)        ,stat=AS)|
01043 lpsolver.f90                   (f90) 00282 |        allocate(BasisPos(0:nSup)        ,stat=AS)|
01044 lpsolver.f90                   (f90) 00284 |        allocate(b(0:siCons)             ,stat=AS)|
01045 lpsolver.f90                   (f90) 00286 |        allocate(c(0:nSup)               ,stat=AS)|
01046 lpsolver.f90                   (f90) 00288 |        allocate(UB(0:nSup)              ,stat=AS)|
01047 lpsolver.f90                   (f90) 00290 |        allocate(xB(0:mSup)              ,stat=AS)|
01048 lpsolver.f90                   (f90) 00292 |        allocate(ConstrtRow(0:nSup)      ,stat=AS)|
01049 lpsolver.f90                   (f90) 00294 |        allocate(yLine(0:siLine)         ,stat=AS)|
01050 lpsolver.f90                   (f90) 00296 |        allocate(ej(0:mSup)              ,stat=AS)|
01051 lpsolver.f90                   (f90) 00298 |        allocate(ConsRHS(0:siCons)       ,stat=AS)|
01052 lpsolver.f90                   (f90) 00300 |        allocate(SegWeight(0:nSup)       ,stat=AS)|
01053 lpsolver.f90                   (f90) 00302 |        allocate(CurrLimit(0:1,0:siLine) ,stat=AS)|
01054 lpsolver.f90                   (f90) 00304 |        allocate(AMatrix(0:siCons,0:nSup),stat=AS)|
01055 lpsolver.f90                   (f90) 00306 |        allocate(LineName(0:siLine)      ,stat=AS)|
01056 lpsolver.f90                   (f90) 00504 |        allocate(cB(0:mSup)           ,stat=AS)|
01057 lpsolver.f90                   (f90) 00506 |        allocate(NodeVoltage(0:siNode),stat=AS)|
01058 lpsolver.f90                   (f90) 00508 |        allocate(NodeCurrent(0:siNode),stat=AS)|
01059 lpsolver.f90                   (f90) 00648 |          allocate(BMatLUF(0:mSup,0:mSup),stat=AS)|
01060 lpsolver.f90                   (f90) 00650 |          allocate(PivotRow      (0:mSup),stat=AS)|
01061 lpsolver.f90                   (f90) 00652 |          allocate(SupMagInRow   (0:mSup),stat=AS)|
01062 margnobj.f90                   (f90) 00626 |               ALLOCATE(CUM_UNIT_UTIL(TRANS_GROUP_POINTS, &|
01063 margnobj.f90                   (f90) 00628 |               ALLOCATE(CURRENT_UNIT_UTIL(TRANS_GROUP_POINTS, &|
01064 margnobj.f90                   (f90) 00630 |               ALLOCATE(TOT_EMBED_COST_BY_POINT(TRANS_GROUP_POINTS, &|
01065 margnobj.f90                   (f90) 00632 |               ALLOCATE(CUM_UNIT_UTIL_zero(TRANS_GROUP_POINTS, &|
01066 margnobj.f90                   (f90) 00634 |               ALLOCATE(CURRENT_UNIT_UTIL_zero(TRANS_GROUP_POINTS, &|
01067 margnobj.f90                   (f90) 00636 |               ALLOCATE(BlockIncrCost(BLOK_ARRAY_ALLOCATOR))|
01068 margnobj.f90                   (f90) 00637 |               ALLOCATE(COST_CURVE_POINTER_BY(0:BLOK_ARRAY_ALLOCATOR, &|
01069 margnobj.f90                   (f90) 00640 |               ALLOCATE( &|
01070 margnobj.f90                   (f90) 00663 |            ALLOCATE(DEPTH_PRICE(DEPTH_MARKET_INTERVALS,24))|
01071 margnobj.f90                   (f90) 00676 |               ALLOCATE(TRANS_DEPTH_PRICE( &|
01072 margnobj.f90                   (f90) 00709 |         ALLOCATE ( &|
01073 margnobj.f90                   (f90) 00712 |         ALLOCATE(NEW_MARGINAL_DATA_BASE(TRANS_GROUP_POINTS, &|
01074 margnobj.f90                   (f90) 00714 |         ALLOCATE(LODDUR_FOR_MC(0:1000,MAX_DATA_BASES))|
01075 margnobj.f90                   (f90) 00715 |         ALLOCATE(LPROB_FOR_MC(0:1000,2,MAX_DATA_BASES))|
01076 margnobj.f90                   (f90) 00716 |         ALLOCATE(ACTIVE_UNIT(NUNITS))|
01077 margnobj.f90                   (f90) 00717 |         ALLOCATE(START_UP_POSITION(NUNITS,SAVE_UPPER_TRANS_GROUP))|
01078 margnobj.f90                   (f90) 00718 |         ALLOCATE(SCARCITY_MULT(SAVE_UPPER_TRANS_GROUP,2))|
01079 margnobj.f90                   (f90) 00719 |         ALLOCATE(SALES_B4_AFTER(0:TRANS_GROUP_POINTS,2,MAX_DATA_BASES))|
01080 margnobj.f90                   (f90) 00720 |         ALLOCATE(WHOLESALE_REV_AND_EXP(TRANS_GROUP_POINTS,2, &|
01081 margnobj.f90                   (f90) 00722 |         ALLOCATE(WHOLESALE_SAL_AND_PUR(TRANS_GROUP_POINTS,2, &|
01082 margnobj.f90                   (f90) 00724 |         ALLOCATE(RETAIL_REV_AND_EXP(TRANS_GROUP_POINTS,2, &|
01083 margnobj.f90                   (f90) 00726 |         ALLOCATE(RETAIL_SAL_AND_PUR(TRANS_GROUP_POINTS,2, &|
01084 margnobj.f90                   (f90) 00728 |         ALLOCATE(NEW_POINT_COUNT(TRANS_GROUP_POINTS,MAX_DATA_BASES))|
01085 margnobj.f90                   (f90) 00729 |         ALLOCATE(NEW_LODDUR(TRANS_GROUP_POINTS,MAX_DATA_BASES))|
01086 margnobj.f90                   (f90) 00730 |         ALLOCATE(EXPECTED_MARGINAL_COST(TRANS_GROUP_POINTS, &|
01087 margnobj.f90                   (f90) 00732 |         ALLOCATE(TRANSITION_MARGINAL_COST(TRANS_GROUP_POINTS, &|
01088 margnobj.f90                   (f90) 00734 |         ALLOCATE(CAPACITY_GIVEN_MARKET(TRANS_GROUP_POINTS, &|
01089 margnobj.f90                   (f90) 00736 |         ALLOCATE(BLOCK_B4_MARKET(TRANS_GROUP_POINTS,MAX_DATA_BASES))|
01090 margnobj.f90                   (f90) 00737 |         ALLOCATE(REMAINING_CONTRIBUTION_MW(TRANS_GROUP_POINTS, &|
01091 margnobj.f90                   (f90) 00739 |         ALLOCATE( &|
01092 margnobj.f90                   (f90) 00741 |         ALLOCATE(CUM_CAP(MAX_DATA_BASES))|
01093 margnobj.f90                   (f90) 00742 |         ALLOCATE(LAST_DB_POINT(MAX_DATA_BASES))|
01094 margnobj.f90                   (f90) 00743 |         ALLOCATE(CumDbDx(MAX_DATA_BASES))|
01095 margnobj.f90                   (f90) 00744 |         ALLOCATE(DB_ADJ_TOTAL_CAP(MAX_DATA_BASES))|
01096 margnobj.f90                   (f90) 00745 |         ALLOCATE(DB_CAP_AFTER_OUTAGES(MAX_DATA_BASES))|
01097 margnobj.f90                   (f90) 00746 |         ALLOCATE(DB_DX(MAX_DATA_BASES))|
01098 margnobj.f90                   (f90) 00747 |         ALLOCATE(PREVIOUS_INTERVAL(MAX_DATA_BASES))|
01099 margnobj.f90                   (f90) 00748 |         ALLOCATE(I_CORRECT(MAX_DATA_BASES))|
01100 margnobj.f90                   (f90) 00749 |         ALLOCATE(ACTIVE_DATA_BASE(MAX_DATA_BASES))|
01101 margnobj.f90                   (f90) 00750 |         ALLOCATE(DETAILED_OUTAGE_DATA_BASE(MAX_DATA_BASES))|
01102 margnobj.f90                   (f90) 00751 |         ALLOCATE(CURRENT(MAX_DATA_BASES))|
01103 margnobj.f90                   (f90) 00752 |         ALLOCATE(NEXT(MAX_DATA_BASES))|
01104 margnobj.f90                   (f90) 00753 |         ALLOCATE(POS(MAX_DATA_BASES))|
01105 margnobj.f90                   (f90) 00754 |         ALLOCATE(NATIVE_POS(MAX_DATA_BASES))|
01106 margnobj.f90                   (f90) 00755 |         ALLOCATE(CUM_COST(MAX_DATA_BASES))|
01107 margnobj.f90                   (f90) 00756 |         ALLOCATE(CUM_PROB(MAX_DATA_BASES))|
01108 margnobj.f90                   (f90) 00757 |         ALLOCATE(SAVE_LOAD(MAX_DATA_BASES))|
01109 margnobj.f90                   (f90) 00758 |         ALLOCATE(NATIVE_POS_PERCENT(MAX_DATA_BASES))|
01110 margnobj.f90                   (f90) 00759 |         ALLOCATE(CUM_SALES_AFTER(MAX_DATA_BASES))|
01111 margnobj.f90                   (f90) 00760 |         ALLOCATE(CUM_MUST_RUN_MW(MAX_DATA_BASES))|
01112 margnobj.f90                   (f90) 00761 |         ALLOCATE(CUM_MUST_RUN_POSITION(MAX_DATA_BASES))|
01113 margnobj.f90                   (f90) 00762 |         ALLOCATE(UNIT_ENERGY_B4_AFTER(BLOK_ARRAY_ALLOCATOR,2, &|
01114 margnobj.f90                   (f90) 00808 |      ALLOCATE(UNIT_SAVE(NBLOK2),BLKNO_SAVE(NBLOK2)) ! 11/28/00.|
01115 margnobj.f90                   (f90) 00855 |      ALLOCATE(UNIT_DISP_COST(NUNITS,2),BLOCK_DISP_COST(NBLOK2))|
01116 margnobj.f90                   (f90) 00861 |      ALLOCATE(SAVE_TRANS_FOR_DATA_BASE(MAX_DATA_BASES))|
01117 margnobj.f90                   (f90) 00905 |      ALLOCATE(OUTAGE_UNIT_INDEX(0:DATA_BASIS))|
01118 margnobj.f90                   (f90) 00906 |      ALLOCATE(OUTAGE_BLOCK_INDEX(0:DATA_BASIS))|
01119 margnobj.f90                   (f90) 00907 |      ALLOCATE(TRANS_BLOCK_CAPACITY(0:DATA_BASIS))|
01120 margnobj.f90                   (f90) 00908 |      ALLOCATE(CUM_TC_UNIT_MWH(NUNITS))|
01121 margnobj.f90                   (f90) 00936 |      ALLOCATE(FIRST_ECONOMY_COST(MAX_DATA_BASES))|
01122 margnobj.f90                   (f90) 00939 |      ALLOCATE(MUST_RUN_COST(MAX_DATA_BASES))|
01123 margnobj.f90                   (f90) 00942 |      ALLOCATE(MW_AFTER_OUTAGES(NBLOK2,MAX_DATA_BASES))|
01124 margnobj.f90                   (f90) 00946 |      ALLOCATE(MW_DIFF(0:NBLOK2,MAX_DATA_BASES))|
01125 margnobj.f90                   (f90) 01371 |      ALLOCATE(START_UP_UNIT_BY_TG(SAVE_UPPER_TRANS_GROUP))|
01126 margnobj.f90                   (f90) 01374 |      ALLOCATE(MARGINAL_UNIT_BY_TECH(SAVE_UPPER_TRANS_GROUP, &|
01127 margnobj.f90                   (f90) 01394 |      ALLOCATE(OUTAGE_DB_USED(MAX_TRANS_GROUP_NUMBER))|
01128 margnobj.f90                   (f90) 01395 |      ALLOCATE(COMMITMENT_DB(MAX_TRANS_GROUP_NUMBER))|
01129 margnobj.f90                   (f90) 01544 |                  ALLOCATE(TEMP_TRANSFER_ARRAY1(0:ALLOCATED_DATA_BASES))|
01130 margnobj.f90                   (f90) 01545 |                  ALLOCATE(TEMP_TRANSFER_ARRAY2(0:ALLOCATED_DATA_BASES))|
01131 margnobj.f90                   (f90) 01546 |                  ALLOCATE(TEMP_TRANSFER_ARRAY3(0:ALLOCATED_DATA_BASES))|
01132 margnobj.f90                   (f90) 01554 |                  ALLOCATE( &|
01133 margnobj.f90                   (f90) 01556 |                  ALLOCATE( &|
01134 margnobj.f90                   (f90) 01558 |                  ALLOCATE( &|
01135 margnobj.f90                   (f90) 02270 |      ALLOCATE(OUTAGE_SINGULAR(0:SAVE_OUTAGE_BLOCKS))|
01136 margnobj.f90                   (f90) 02271 |      ALLOCATE(ADJUST_BLOCK(0:SAVE_OUTAGE_BLOCKS))|
01137 margnobj.f90                   (f90) 02272 |      ALLOCATE(DERATE_BLOCK(0:SAVE_OUTAGE_BLOCKS))|
01138 margnobj.f90                   (f90) 02273 |      ALLOCATE(OUTAGE_TYPE(0:SAVE_OUTAGE_BLOCKS))|
01139 margnobj.f90                   (f90) 02294 |         ALLOCATE(HOURLY_OUTAGE_STATE(NBLOK2,SAVE_HOURS_IN_MONTH))|
01140 margnobj.f90                   (f90) 02295 |         ALLOCATE(HOURLY_OUTAGE_TYPE(NBLOK2,SAVE_HOURS_IN_MONTH))|
01141 margnobj.f90                   (f90) 02296 |         ALLOCATE(HOURLY_LAST_STATE(NBLOK2))|
01142 margnobj.f90                   (f90) 02297 |         ALLOCATE(HOURLY_LAST_DERATE(NBLOK2))|
01143 margnobj.f90                   (f90) 02298 |         ALLOCATE(HOURLY_LAST_TYPE(NBLOK2))|
01144 margnobj.f90                   (f90) 05203 |            ALLOCATE(OUTAGE_ENERGY(SAVE_OUTAGE_BLOCKS))|
01145 margnobj.f90                   (f90) 05204 |            ALLOCATE(OUTAGE_WHOLESALE_ENERGY(SAVE_OUTAGE_BLOCKS,2))|
01146 margnobj.f90                   (f90) 05205 |            ALLOCATE(OUTAGE_WHOLESALE_REVENUE(SAVE_OUTAGE_BLOCKS,2))|
01147 margnobj.f90                   (f90) 06128 |      ALLOCATE(SORT_POS(LOAD_HOURS_IN_PERIOD), &|
01148 margnobj.f90                   (f90) 06426 |      allocate(a_temp_array(0:nUnique),b_temp_array(0:nUnique))|
01149 midasmod.for                   (for) 01951 |         ALLOCATE(TABLE_POSITION_FOR(1024),|
01150 msgmmadm.f90                   (f90) 00085 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01151 msgmmadm.f90                   (f90) 00173 |            ALLOCATE(ADDENDUM_BC_ASSET_CLASS_POINTER(MAX_BC_ADDENDUM_CLASS_ID_NUM))|
01152 msgmmadm.f90                   (f90) 00214 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01153 msgmmadm.f90                   (f90) 00280 |         ALLOCATE(ADDENDUM_OL_ASSET_CLASS_POINTER(MAX_OL_ADDENDUM_CLASS_ID_NUM))|
01154 msgmmadm.f90                   (f90) 00514 |      ALLOCATE(ESCALATION_RATES(AVAIL_DATA_YEARS),ADDENDUM_VALUES(0:MAX(FINANCIAL_SIMULATION_YEARS,AVAIL_DATA_YEARS)), &|
01155 msgmmadm.f90                   (f90) 00806 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
01156 msgmmadm.f90                   (f90) 00827 |         ALLOCATE(PAYMENTS(0:12,0:FINANCIAL_SIMULATION_YEARS,-1:NUM_OF_ASSET_CLASSES,LAST_PAYMENT_ITEM), &|
01157 msgmmdbt.for                   (for) 00088 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01158 msgmmdbt.for                   (for) 00201 |            ALLOCATE(DEBT_BC_ASSET_CLASS_POINTER|
01159 msgmmdbt.for                   (for) 00242 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01160 msgmmdbt.for                   (for) 00354 |         ALLOCATE(DEBT_OL_ASSET_CLASS_POINTER(MAX_OL_DEBT_CLASS_ID_NUM))|
01161 msgmmdbt.for                   (for) 00768 |      ALLOCATE(BAL(0:12,0:FINANCIAL_SIMULATION_YEARS),|
01162 msgmmdbt.for                   (for) 03203 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
01163 msgmmdbt.for                   (for) 03236 |         ALLOCATE(ISSPS(0:12,0:FINANCIAL_SIMULATION_YEARS,|
01164 msgmmdfd.f90                   (f90) 00081 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01165 msgmmdfd.f90                   (f90) 00146 |            ALLOCATE(DEBIT_BC_ASSET_CLASS_POINTER(MAX_BC_DEBIT_CLASS_ID_NUM))|
01166 msgmmdfd.f90                   (f90) 00187 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01167 msgmmdfd.f90                   (f90) 00257 |         ALLOCATE(DEBIT_OL_ASSET_CLASS_POINTER(MAX_OL_DEBIT_CLASS_ID_NUM))|
01168 msgmmdfd.f90                   (f90) 00548 |      ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &|
01169 msgmmdfd.f90                   (f90) 01831 |         ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
01170 msgmmdfd.f90                   (f90) 01849 |      ALLOCATE(TDDB(0:12,0:FINANCIAL_SIMULATION_YEARS,0:NUM_OF_ASSET_CLASSES,0:BAL_SHEET_OPTIONS,UNIQUE_ACCT_TYPE), &|
01171 msgmmea.f90                    (f90) 00089 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01172 msgmmea.f90                    (f90) 00165 |            ALLOCATE(BC_EXISTING_ASSET_CLASS_POINTER &|
01173 msgmmea.f90                    (f90) 00210 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01174 msgmmea.f90                    (f90) 00268 |         ALLOCATE(OL_EXISTING_ASSET_CLASS_POINTER &|
01175 msgmmea.f90                    (f90) 00477 |      ALLOCATE(BOKDP(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01176 msgmmea.f90                    (f90) 00478 |      ALLOCATE(GPV(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01177 msgmmea.f90                    (f90) 00479 |      ALLOCATE(CUM_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01178 msgmmea.f90                    (f90) 00480 |      ALLOCATE(NPV(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01179 msgmmea.f90                    (f90) 00481 |      ALLOCATE(RETIRE(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01180 msgmmea.f90                    (f90) 00482 |      ALLOCATE(TAXDP(0:FINANCIAL_SIMULATION_YEARS))|
01181 msgmmea.f90                    (f90) 00483 |      ALLOCATE(BOKTAX(0:FINANCIAL_SIMULATION_YEARS))|
01182 msgmmea.f90                    (f90) 00484 |      ALLOCATE(ASSET_DEFERRED_TAXES(0:FINANCIAL_SIMULATION_YEARS))|
01183 msgmmea.f90                    (f90) 00485 |      ALLOCATE(ASSET_DEFERRED_TAX_BASIS(0:FINANCIAL_SIMULATION_YEARS))|
01184 msgmmea.f90                    (f90) 01002 |      ALLOCATE(MONTHLY_BOOK_DEP_VECTOR_ADDER(0:12, &|
01185 msgmmea.f90                    (f90) 01358 |         ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
01186 msgmmea.f90                    (f90) 01372 |      ALLOCATE(TRETIE(0:12,FINANCIAL_SIMULATION_YEARS, &|
01187 msgmmea.f90                    (f90) 01374 |      ALLOCATE(TBOKDP(0:12,FINANCIAL_SIMULATION_YEARS, &|
01188 msgmmea.f90                    (f90) 01376 |      ALLOCATE(TGPV(0:12,FINANCIAL_SIMULATION_YEARS, &|
01189 msgmmea.f90                    (f90) 01378 |      ALLOCATE(CUMULATIVE_BOOK_DEP(0:12,FINANCIAL_SIMULATION_YEARS, &|
01190 msgmmea.f90                    (f90) 01380 |      ALLOCATE(TOTAL_NPV_RATEBASE(0:12,FINANCIAL_SIMULATION_YEARS, &|
01191 msgmmea.f90                    (f90) 01382 |      ALLOCATE(TTAXDP(FINANCIAL_SIMULATION_YEARS, &|
01192 msgmmea.f90                    (f90) 01385 |      ALLOCATE(PROPERTY_TAX_GPV(FINANCIAL_SIMULATION_YEARS, &|
01193 msgmmea.f90                    (f90) 01387 |      ALLOCATE(PROPERTY_TAX_NPV(FINANCIAL_SIMULATION_YEARS, &|
01194 msgmmea.f90                    (f90) 01389 |      ALLOCATE(EATAXBOKDEP(FINANCIAL_SIMULATION_YEARS, &|
01195 msgmmea.f90                    (f90) 01391 |      ALLOCATE(EANORMTAXDP(FINANCIAL_SIMULATION_YEARS, &|
01196 msgmmea.f90                    (f90) 01393 |      ALLOCATE(EA_DEF_TAX(FINANCIAL_SIMULATION_YEARS, &|
01197 msgmmea.f90                    (f90) 01395 |      ALLOCATE(EA_DEF_TAX_BASIS(FINANCIAL_SIMULATION_YEARS, &|
01198 msgmmea.f90                    (f90) 01397 |      ALLOCATE(TOTAL_DEF_TAX_RATEBASE(FINANCIAL_SIMULATION_YEARS))|
01199 msgmmea.f90                    (f90) 01398 |      ALLOCATE(EA_ACE_DEPRECIATION(FINANCIAL_SIMULATION_YEARS))|
01200 msgmmea.f90                    (f90) 01399 |      ALLOCATE(TOTAL_DEF_TAX_BASIS_RATEBASE(FINANCIAL_SIMULATION_YEARS))|
01201 msgmmea.f90                    (f90) 01437 |      ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))|
01202 msgmmea.f90                    (f90) 01438 |      ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))|
01203 msgmmexprevcash.f90            (f90) 00887 |      ALLOCATE(READ_EXPEN(0:AVAIL_DATA_YEARS))|
01204 msgmmexprevcash.f90            (f90) 00888 |      ALLOCATE(EXPENSES(0:FINANCIAL_SIMULATION_YEARS))|
01205 msgmmexprevcash.f90            (f90) 00889 |      ALLOCATE(MIDAS_ANNUAL_CASH(0:FINANCIAL_SIMULATION_YEARS))|
01206 msgmmexprevcash.f90            (f90) 00890 |      ALLOCATE(ACCOUNT_PAYABLE_VALUES(0:FINANCIAL_SIMULATION_YEARS))|
01207 msgmmexprevcash.f90            (f90) 00891 |      ALLOCATE(OUTPUT_VALUE(0:FINANCIAL_SIMULATION_YEARS))|
01208 msgmmexprevcash.f90            (f90) 00892 |      ALLOCATE(ALLOC_EXPEN(0:FINANCIAL_SIMULATION_YEARS))|
01209 msgmmexprevcash.f90            (f90) 00893 |      ALLOCATE(ALLOC_CASH_EXPEN(0:FINANCIAL_SIMULATION_YEARS))|
01210 msgmmexprevcash.f90            (f90) 00894 |      ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))|
01211 msgmmexprevcash.f90            (f90) 00895 |      ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))|
01212 msgmmexprevcash.f90            (f90) 02100 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
01213 msgmmexprevcash.f90            (f90) 02105 |         ALLOCATE(EXPENSES_MONTHLY(0:12,0:FINANCIAL_SIMULATION_YEARS, &|
01214 msgmmexprevcash.f90            (f90) 02115 |         ALLOCATE(BUDGET_EXPENSE(0:12,0:FINANCIAL_SIMULATION_YEARS,10))|
01215 msgmmexprevcash.f90            (f90) 02124 |         ALLOCATE(REVENUES_MONTHLY(0:12,0:FINANCIAL_SIMULATION_YEARS, &|
01216 msgmmexprevcash.f90            (f90) 02129 |         ALLOCATE(SECONDARY_SALES_NOT_IN_RATES(0:12, &|
01217 msgmmexprevcash.f90            (f90) 02132 |         ALLOCATE(OTHER_REVENUES_NOT_IN_RATES(0:12, &|
01218 msgmmexprevcash.f90            (f90) 02143 |         ALLOCATE(CASH_MONTHLY(0:12, &|
01219 msgmmexprevcash.f90            (f90) 02150 |         ALLOCATE(CASH_REV_EXP_MONTHLY(0:12, &|
01220 msgmmexprevcash.f90            (f90) 05869 |         ALLOCATE(ANNUAL_CASH_ROLL_OVER(1:12,CASH_VARS, &|
01221 msgmmfa.f90                    (f90) 00134 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01222 msgmmfa.f90                    (f90) 00292 |         ALLOCATE(BC_FA_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))|
01223 msgmmfa.f90                    (f90) 00335 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01224 msgmmfa.f90                    (f90) 00431 |         ALLOCATE(OL_FA_CLASS_POINTER(MAX_OL_CLASS_ID_NUM))|
01225 msgmmfa.f90                    (f90) 00642 |         ALLOCATE(CEP(MAX_FINANCIAL_SIMULATION_YEARS))|
01226 msgmmfa.f90                    (f90) 00643 |         ALLOCATE(CE(MAX_FINANCIAL_SIMULATION_YEARS))|
01227 msgmmfa.f90                    (f90) 00644 |         ALLOCATE(ESCALATION_RATES(AVAIL_DATA_YEARS))|
01228 msgmmfa.f90                    (f90) 00761 |         ALLOCATE(CEP(MAX_FINANCIAL_SIMULATION_YEARS))|
01229 msgmmfa.f90                    (f90) 00762 |         ALLOCATE(CE(MAX_FINANCIAL_SIMULATION_YEARS))|
01230 msgmmfa.f90                    (f90) 00785 |         ALLOCATE(ESCALATION_RATES(AVAIL_DATA_YEARS))|
01231 msgmmfa.f90                    (f90) 00814 |            ALLOCATE(TEMP_HOLDING_ARRAY(MAX_FINANCIAL_SIMULATION_YEARS))|
01232 msgmmfa.f90                    (f90) 01273 |      ALLOCATE(CEPTXBOK(FINANCIAL_SIMULATION_YEARS))|
01233 msgmmfa.f90                    (f90) 01274 |      ALLOCATE(TOTAL_BKDPTX(FINANCIAL_SIMULATION_YEARS))|
01234 msgmmfa.f90                    (f90) 01275 |      ALLOCATE(CURRENT_INTEREST_CAP(FINANCIAL_SIMULATION_YEARS))|
01235 msgmmfa.f90                    (f90) 01276 |      ALLOCATE(PCAPINRST(FINANCIAL_SIMULATION_YEARS))|
01236 msgmmfa.f90                    (f90) 01277 |      ALLOCATE(TAXDPALT(FINANCIAL_SIMULATION_YEARS))|
01237 msgmmfa.f90                    (f90) 01278 |      ALLOCATE(CAPINRST(FINANCIAL_SIMULATION_YEARS))|
01238 msgmmfa.f90                    (f90) 01279 |      ALLOCATE(TXPREFDEP(FINANCIAL_SIMULATION_YEARS))|
01239 msgmmfa.f90                    (f90) 01280 |      ALLOCATE(BOOK_VALUE(FINANCIAL_SIMULATION_YEARS))|
01240 msgmmfa.f90                    (f90) 01281 |      ALLOCATE(ACE_BOOK_DEP(FINANCIAL_SIMULATION_YEARS))|
01241 msgmmfa.f90                    (f90) 01282 |      ALLOCATE(TAX_VALUE_BEGINNING_90(FINANCIAL_SIMULATION_YEARS))|
01242 msgmmfa.f90                    (f90) 01283 |      ALLOCATE(DUMMY(FINANCIAL_SIMULATION_YEARS))|
01243 msgmmfa.f90                    (f90) 01289 |      ALLOCATE(BOOK_EXPEN(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01244 msgmmfa.f90                    (f90) 01290 |      ALLOCATE(CASH_OM_ADDER(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01245 msgmmfa.f90                    (f90) 01291 |      ALLOCATE(NON_CASH_OM_ADDER(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01246 msgmmfa.f90                    (f90) 01292 |      ALLOCATE(PLANT_2_SERVICE(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01247 msgmmfa.f90                    (f90) 01293 |      ALLOCATE(PERCENT_PLANT_2_SERVICE(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01248 msgmmfa.f90                    (f90) 01294 |      ALLOCATE(MONTHLY_AFUDC_ON_CASH(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01249 msgmmfa.f90                    (f90) 01295 |      ALLOCATE(MONTHLY_AFUDC_ON_PLANT(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01250 msgmmfa.f90                    (f90) 01296 |      ALLOCATE(MONTHLY_CWIP(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01251 msgmmfa.f90                    (f90) 01297 |      ALLOCATE(MONTHLY_AFUDC_IN_CWIP(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01252 msgmmfa.f90                    (f90) 01298 |      ALLOCATE(MONTHLY_CWIP_IN_RATEBASE(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01253 msgmmfa.f90                    (f90) 01299 |      ALLOCATE(MONTHLY_CAPITALIZED_INTEREST(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01254 msgmmfa.f90                    (f90) 01300 |      ALLOCATE(MONTHLY_CURRENT_INTEREST(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01255 msgmmfa.f90                    (f90) 01301 |      ALLOCATE(CASH_EXPENDITURES(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01256 msgmmfa.f90                    (f90) 01302 |      ALLOCATE(MONTHLY_INTEREST_TO_TAX_VALUE(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01257 msgmmfa.f90                    (f90) 01303 |      ALLOCATE(MONTHLY_TAX_VALUE_OF_ASSET(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01258 msgmmfa.f90                    (f90) 01304 |      ALLOCATE(BONUS_DEP_MONTHLY_2001(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01259 msgmmfa.f90                    (f90) 01305 |      ALLOCATE(MONTHLY_TAX_DEP_PREFERENCE(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01260 msgmmfa.f90                    (f90) 01306 |      ALLOCATE(MONTHLY_TAX_DEPRECIATION_ALT(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01261 msgmmfa.f90                    (f90) 01307 |      ALLOCATE(MONTHLY_TAX_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01262 msgmmfa.f90                    (f90) 01308 |      ALLOCATE(MONTHLY_ACE_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01263 msgmmfa.f90                    (f90) 01309 |      ALLOCATE(DUMMY_MONTHLY_ARRAY(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01264 msgmmfa.f90                    (f90) 01310 |      ALLOCATE(MONTHLY_TAX_DEPRECIATION(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01265 msgmmfa.f90                    (f90) 01311 |      ALLOCATE(MONTHLY_DEFERRED_TAX_BASIS(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01266 msgmmfa.f90                    (f90) 01312 |      ALLOCATE(MONTHLY_TAX_EXPENSE(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01267 msgmmfa.f90                    (f90) 01313 |      ALLOCATE(MONTHLY_CURRENT_INTEREST_CAP(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01268 msgmmfa.f90                    (f90) 01314 |      ALLOCATE(MONTHLY_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01269 msgmmfa.f90                    (f90) 01315 |      ALLOCATE(CUM_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01270 msgmmfa.f90                    (f90) 01316 |      ALLOCATE(GPV(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01271 msgmmfa.f90                    (f90) 01317 |      ALLOCATE(NPV(0:12,0:FINANCIAL_SIMULATION_YEARS))|
01272 msgmmfa.f90                    (f90) 01318 |      ALLOCATE(TEMP_PLANT_2_SERVICE(0:FINANCIAL_SIMULATION_YEARS))|
01273 msgmmfa.f90                    (f90) 01351 |      ALLOCATE(VECTOR_DATA_CASH(AVAIL_DATA_YEARS))|
01274 msgmmfa.f90                    (f90) 01352 |      ALLOCATE(VECTOR_DATA_PLNT(AVAIL_DATA_YEARS))|
01275 msgmmfa.f90                    (f90) 01353 |      ALLOCATE(VECTOR_DATA(AVAIL_DATA_YEARS))|
01276 msgmmfa.f90                    (f90) 02823 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
01277 msgmmfa.f90                    (f90) 02842 |         ALLOCATE(TCE(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01278 msgmmfa.f90                    (f90) 02843 |         ALLOCATE(TCEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01279 msgmmfa.f90                    (f90) 02844 |         ALLOCATE(CLASS_GPV(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01280 msgmmfa.f90                    (f90) 02845 |         ALLOCATE(CLASS_CUMULATIVE_DEPRECIATION(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01281 msgmmfa.f90                    (f90) 02846 |         ALLOCATE(CLASS_AFUDC_IN_CWIP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01282 msgmmfa.f90                    (f90) 02847 |         ALLOCATE(TTAXDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01283 msgmmfa.f90                    (f90) 02848 |         ALLOCATE(TBOKDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01284 msgmmfa.f90                    (f90) 02849 |         ALLOCATE(TAFCDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01285 msgmmfa.f90                    (f90) 02850 |         ALLOCATE(TAFDC1(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01286 msgmmfa.f90                    (f90) 02851 |         ALLOCATE(TAFDC2(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01287 msgmmfa.f90                    (f90) 02852 |         ALLOCATE(TCWIP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01288 msgmmfa.f90                    (f90) 02853 |         ALLOCATE(TRBCWP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01289 msgmmfa.f90                    (f90) 02854 |         ALLOCATE(TITCDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01290 msgmmfa.f90                    (f90) 02855 |         ALLOCATE(TITCDF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01291 msgmmfa.f90                    (f90) 02856 |         ALLOCATE(TTXEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01292 msgmmfa.f90                    (f90) 02857 |         ALLOCATE(TTXDEF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01293 msgmmfa.f90                    (f90) 02858 |         ALLOCATE(TDDB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01294 msgmmfa.f90                    (f90) 02859 |         ALLOCATE(TRBDDB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01295 msgmmfa.f90                    (f90) 02860 |         ALLOCATE(TAMRTE(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01296 msgmmfa.f90                    (f90) 02861 |         ALLOCATE(TEXEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01297 msgmmfa.f90                    (f90) 02862 |         ALLOCATE(TAFDPA(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01298 msgmmfa.f90                    (f90) 02863 |         ALLOCATE(TCEPTX(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01299 msgmmfa.f90                    (f90) 02864 |         ALLOCATE(TAFCAJ(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01300 msgmmfa.f90                    (f90) 02865 |         ALLOCATE(TBKDPT(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01301 msgmmfa.f90                    (f90) 02866 |         ALLOCATE(TITCPY(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01302 msgmmfa.f90                    (f90) 02867 |         ALLOCATE(TAFEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01303 msgmmfa.f90                    (f90) 02868 |         ALLOCATE(TBKDPA(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01304 msgmmfa.f90                    (f90) 02869 |         ALLOCATE(TPCAPINRST(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01305 msgmmfa.f90                    (f90) 02870 |         ALLOCATE(TTXPREFDEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01306 msgmmfa.f90                    (f90) 02871 |         ALLOCATE(TOTAL_ACE_BOOK_DEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01307 msgmmfa.f90                    (f90) 02872 |         ALLOCATE(TAFDCB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01308 msgmmfa.f90                    (f90) 02873 |         ALLOCATE(TWODFT(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01309 msgmmfa.f90                    (f90) 02874 |         ALLOCATE(TAFDCF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01310 msgmmfa.f90                    (f90) 02875 |         ALLOCATE(PROPERTY_TAX_GPV(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01311 msgmmfa.f90                    (f90) 02876 |         ALLOCATE(PROPERTY_TAX_NPV(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01312 msgmmfa.f90                    (f90) 02878 |         ALLOCATE(RB_NPV(FINANCIAL_SIMULATION_YEARS))|
01313 msgmmfa.f90                    (f90) 02879 |         ALLOCATE(RB_TAXDP(FINANCIAL_SIMULATION_YEARS))|
01314 msgmmfa.f90                    (f90) 02880 |         ALLOCATE(RB_BOKDP(FINANCIAL_SIMULATION_YEARS))|
01315 msgmmfa.f90                    (f90) 02881 |         ALLOCATE(RB_PCAPINRST(FINANCIAL_SIMULATION_YEARS))|
01316 msgmmfa.f90                    (f90) 02882 |         ALLOCATE(RBCWIP_AFDC_METH2(FINANCIAL_SIMULATION_YEARS))|
01317 msgmmfa.f90                    (f90) 02883 |         ALLOCATE(RB_TXEXP(FINANCIAL_SIMULATION_YEARS))|
01318 msgmmfa.f90                    (f90) 02884 |         ALLOCATE(RB_TXDEF(FINANCIAL_SIMULATION_YEARS))|
01319 msgmmfa.f90                    (f90) 02885 |         ALLOCATE(RB_TXPREFDEP(FINANCIAL_SIMULATION_YEARS))|
01320 msgmmfa.f90                    (f90) 02886 |         ALLOCATE(RB_AMRTE(FINANCIAL_SIMULATION_YEARS))|
01321 msgmmfa.f90                    (f90) 02887 |         ALLOCATE(RB_ITCPY(FINANCIAL_SIMULATION_YEARS))|
01322 msgmmfa.f90                    (f90) 02888 |         ALLOCATE(RB_ITC(FINANCIAL_SIMULATION_YEARS))|
01323 msgmmfa.f90                    (f90) 02889 |         ALLOCATE(RB_AFDC1(FINANCIAL_SIMULATION_YEARS))|
01324 msgmmfa.f90                    (f90) 04897 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
01325 msgmmfa.f90                    (f90) 04917 |         ALLOCATE(TCE(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01326 msgmmfa.f90                    (f90) 04918 |         ALLOCATE(TCEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01327 msgmmfa.f90                    (f90) 04919 |         ALLOCATE(CLASS_GPV(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01328 msgmmfa.f90                    (f90) 04920 |         ALLOCATE(CLASS_CUMULATIVE_DEPRECIATION(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01329 msgmmfa.f90                    (f90) 04921 |         ALLOCATE(CLASS_AFUDC_IN_CWIP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01330 msgmmfa.f90                    (f90) 04922 |         ALLOCATE(TTAXDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01331 msgmmfa.f90                    (f90) 04923 |         ALLOCATE(TBOKDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01332 msgmmfa.f90                    (f90) 04924 |         ALLOCATE(TAFCDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01333 msgmmfa.f90                    (f90) 04925 |         ALLOCATE(TAFDC1(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01334 msgmmfa.f90                    (f90) 04926 |         ALLOCATE(TAFDC2(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01335 msgmmfa.f90                    (f90) 04927 |         ALLOCATE(TCWIP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01336 msgmmfa.f90                    (f90) 04928 |         ALLOCATE(TRBCWP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01337 msgmmfa.f90                    (f90) 04929 |         ALLOCATE(TITCDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01338 msgmmfa.f90                    (f90) 04930 |         ALLOCATE(TITCDF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01339 msgmmfa.f90                    (f90) 04931 |         ALLOCATE(TTXEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01340 msgmmfa.f90                    (f90) 04932 |         ALLOCATE(TTXDEF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01341 msgmmfa.f90                    (f90) 04933 |         ALLOCATE(TDDB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01342 msgmmfa.f90                    (f90) 04934 |         ALLOCATE(TRBDDB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01343 msgmmfa.f90                    (f90) 04935 |         ALLOCATE(TAMRTE(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01344 msgmmfa.f90                    (f90) 04936 |         ALLOCATE(TEXEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01345 msgmmfa.f90                    (f90) 04937 |         ALLOCATE(TAFDPA(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01346 msgmmfa.f90                    (f90) 04938 |         ALLOCATE(TCEPTX(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01347 msgmmfa.f90                    (f90) 04939 |         ALLOCATE(TAFCAJ(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01348 msgmmfa.f90                    (f90) 04940 |         ALLOCATE(TBKDPT(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01349 msgmmfa.f90                    (f90) 04941 |         ALLOCATE(TITCPY(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01350 msgmmfa.f90                    (f90) 04942 |         ALLOCATE(TAFEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01351 msgmmfa.f90                    (f90) 04943 |         ALLOCATE(TBKDPA(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01352 msgmmfa.f90                    (f90) 04944 |         ALLOCATE(TPCAPINRST(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01353 msgmmfa.f90                    (f90) 04945 |         ALLOCATE(TTXPREFDEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01354 msgmmfa.f90                    (f90) 04946 |         ALLOCATE(TOTAL_ACE_BOOK_DEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01355 msgmmfa.f90                    (f90) 04947 |         ALLOCATE(TAFDCB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01356 msgmmfa.f90                    (f90) 04948 |         ALLOCATE(TWODFT(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01357 msgmmfa.f90                    (f90) 04949 |         ALLOCATE(TAFDCF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01358 msgmmfa.f90                    (f90) 04950 |         ALLOCATE(PROPERTY_TAX_GPV(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01359 msgmmfa.f90                    (f90) 04951 |         ALLOCATE(PROPERTY_TAX_NPV(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))|
01360 msgmmfa.f90                    (f90) 04953 |         ALLOCATE(RB_NPV(FINANCIAL_SIMULATION_YEARS), &|
01361 msgmmfa.f90                    (f90) 05594 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
01362 msgmmfa.f90                    (f90) 05595 |            ALLOCATE(TOTAL_MONTHLY_FA_VALUES(0:12, &|
01363 msgmmfv.f90                    (f90) 00097 |         ALLOCATE(TEMP_POINTER(0:10000))|
01364 msgmmfv.f90                    (f90) 00154 |            ALLOCATE(BASE_VECTOR_POINTER(0:MAX_BASE_VECTOR_NUM))|
01365 msgmmfv.f90                    (f90) 00195 |      ALLOCATE(TEMP_POINTER(0:10000))|
01366 msgmmfv.f90                    (f90) 00253 |         ALLOCATE(OVLY_VECTOR_POINTER(0:MAX_OVERLAY_VECTOR_NUM))|
01367 msgmmfv.f90                    (f90) 00737 |         ALLOCATE(TEMP_POINTER(0:10000))|
01368 msgmmfv.f90                    (f90) 00787 |            ALLOCATE(BASE_VECTOR_POINTER(0:MAX_BASE_VECTOR_NUM))|
01369 msgmmfv.f90                    (f90) 00829 |      ALLOCATE(TEMP_POINTER(0:10000))|
01370 msgmmfv.f90                    (f90) 00887 |         ALLOCATE(OVLY_VECTOR_POINTER(0:MAX_OVERLAY_VECTOR_NUM))|
01371 msgmminp.for                   (for) 00125 |         ALLOCATE(CLASS_TABLE_POINTER(0:MAX_PARM_FILE_CLASS_NUMBER))|
01372 msgmmout.for                   (for) 06688 |         ALLOCATE(BALANCE_SHEET_VARIABLES(0:12,BAL_SHEET_VARS))|
01373 msgmmout.for                   (for) 06689 |         ALLOCATE(INCOME_VARIABLES(0:12,INCOME_VARS))|
01374 msgmmout.for                   (for) 06690 |         ALLOCATE(CASH_VARIABLES(0:12,CASH_VARS))|
01375 msgmmout.for                   (for) 06691 |         ALLOCATE(TAX_VARIABLES(0:12,TAX_VARS))|
01376 msgmmout.for                   (for) 06692 |         ALLOCATE(CPL_TAX__VARIABLES(0:12,CPL_TAX_VARS,3))|
01377 msgmmout.for                   (for) 06693 |         ALLOCATE(INCOME_LINKED_LEVEL(0:12,INCOME_VARS,|
01378 msgmmout.for                   (for) 06695 |         ALLOCATE(BALANCE_LINKED_LEVEL(0:12,BAL_SHEET_VARS,|
01379 msgmmout.for                   (for) 06697 |         ALLOCATE(BALANCE_SHEET_DEACTIVE_VARS(0:12,BAL_SHEET_VARS,|
01380 msgmmout.for                   (for) 06699 |         ALLOCATE(TRANSFER_LINKED_LEVEL_VARIABLES(0:12,BAL_SHEET_VARS,|
01381 msgmmout.for                   (for) 06701 |         ALLOCATE(TAX_TRANSFER_VARIABLES(0:12,TAX_VARS,|
01382 msgmmout.for                   (for) 06703 |         ALLOCATE(PAYABLE_LINKED_LOWER_LEVEL(0:12,-1:MAX_LINKED_LEVEL))|
01383 msgmmout.for                   (for) 06704 |         ALLOCATE(RECEIVABLES_LINKED_LOWER_LEVEL(0:12,|
01384 msgmmout.for                   (for) 06706 |         ALLOCATE(TAXES_LINKED_LEVEL(0:12,TAX_VARS,-1:MAX_LINKED_LEVEL))|
01385 msgmmout.for                   (for) 06707 |         ALLOCATE(CASH_LINKED_LEVEL(0:12,CASH_VARS,-1:MAX_LINKED_LEVEL))|
01386 msgmmout.for                   (for) 06708 |         ALLOCATE(OPENING_CASH_BALANCES(0:R_NUM_OF_ACTIVE_CLASSES))|
01387 msgmmout.for                   (for) 06709 |         ALLOCATE(TRANSFER_VARIABLES(0:12,BAL_SHEET_VARS))|
01388 msgmmout.for                   (for) 06711 |         ALLOCATE(CLASS_GOODWILL_EOY(-1:R_NUM_OF_ACTIVE_CLASSES))|
01389 msgmmout.for                   (for) 06791 |         ALLOCATE(|
01390 msgmmout.for                   (for) 06793 |         ALLOCATE(|
01391 msgmmout.for                   (for) 06803 |         ALLOCATE(PAYABLE_MONTHLY_VALUES(0:12,PAYMENT_VARS,|
01392 msgmmout.for                   (for) 06805 |         ALLOCATE(TEMP_PAYABLE_MONTHLY_VALUES(0:12,PAYMENT_VARS,|
01393 msgmmout.for                   (for) 07513 |         ALLOCATE(EXCLUDED_OTHER_TAXES_REV(0:12,-1:MAX_LINKED_LEVEL))|
01394 msgmmout.for                   (for) 07514 |         ALLOCATE(EXCLUDED_OTHER_TAXES_EXP(0:12,-1:MAX_LINKED_LEVEL))|
01395 msgmmout.for                   (for) 07515 |         ALLOCATE(OTHER_TAXES_MOVING_UP(0:12,-1:MAX_LINKED_LEVEL))|
01396 msgmmout.for                   (for) 07521 |         ALLOCATE(EXCLUDED_REVENUE_TAX_BASIS(0:12,-1:MAX_LINKED_LEVEL))|
01397 msgmmout.for                   (for) 07522 |         ALLOCATE(EXCLUDED_REVENUE_TAX_BASIS_A(0:12,|
01398 msgmmout.for                   (for) 07524 |         ALLOCATE(EXCLUDED_REVENUE_TAX_BASIS_B(0:12,|
01399 msgmmout.for                   (for) 07526 |         ALLOCATE(REVENUE_TAXES_MOVING_UP(0:12,-1:MAX_LINKED_LEVEL))|
01400 msgmmout.for                   (for) 09436 |         ALLOCATE(MONTHLY_PRIOR_OPT_METHOD_REVS(0:12,|
01401 msgmmpyb.f90                   (f90) 00388 |         ALLOCATE(FILE_PAYABLE_VALUES(12,5,PAYMENT_VARS, &|
01402 msgmmrev.f90                   (f90) 00118 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01403 msgmmrev.f90                   (f90) 00240 |            ALLOCATE(BC_ASSET_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))|
01404 msgmmrev.f90                   (f90) 00268 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01405 msgmmrev.f90                   (f90) 00374 |            ALLOCATE(OL_ASSET_CLASS_POINTER(MAX_OL_CLASS_ID_NUM))|
01406 msgmmrev.f90                   (f90) 00804 |            ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &|
01407 msgmmrev.f90                   (f90) 00827 |               ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
01408 msgmmrev.f90                   (f90) 00844 |            ALLOCATE(REVENUES_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES,LAST_INCOME_LINE), &|
01409 msgmmrev.f90                   (f90) 00852 |            ALLOCATE(ASSET_CLASS_CUSTOMERS(0:12,-1:NUM_OF_ASSET_CLASSES))|
01410 msgmmrev.f90                   (f90) 00855 |            ALLOCATE(ASSET_CLASS_ENERGY(0:12,-1:NUM_OF_ASSET_CLASSES))|
01411 msgmmrev.f90                   (f90) 03680 |            ALLOCATE(STATE_RPS_DB(13,600,0:12), &|
01412 msgmmrev.f90                   (f90) 03829 |               ALLOCATE(ANN_RPS_INTRA_REGION_SURPLUS(0:6,0:RPS_REGION_COUNT))|
01413 msgmmrev.f90                   (f90) 04478 |            ALLOCATE( &|
01414 msgmmrev.f90                   (f90) 05413 |         ALLOCATE(TRANS_GROUP_POSITION(-1:MAX_GAS_GROUP_NUMBER), &|
01415 msgmmrev.f90                   (f90) 05625 |         ALLOCATE(RESOURCES_PER_TYPE(MAX_STATES),RPS_RESOURCES_INDEX(R_RESOURCE_NUM))|
01416 msgmmrev.f90                   (f90) 05645 |         ALLOCATE(RPS_SUPPLY_CURVES( &|
01417 msgmmrev.f90                   (f90) 05652 |         ALLOCATE(RPS_NET_REV(NUM_RPS_RESOURCES), &|
01418 msgmmrev.f90                   (f90) 06107 |            ALLOCATE(CO2_Emission_Cap(YR1_DATA:YR2_DATA),Residential_Emissions(YR1_DATA:YR2_DATA), &|
01419 msgmmrev.f90                   (f90) 06616 |         ALLOCATE(      Initial_Bank(NUM_RPS_PROGRAMS), &|
01420 msgmmrev.f90                   (f90) 06672 |            ALLOCATE(PROGRAM_POSITION(MAX_PROGRAM_NUMBER))|
01421 msgmmsb6.for                   (for) 01320 |         ALLOCATE(MASTER_CLASS_LIST(0:MAX_CLASS_NUM))|
01422 msgmmsb6.for                   (for) 01322 |         ALLOCATE(TEMP_LIST(MAX_CLASS_NUM))|
01423 msgmmsb6.for                   (for) 01507 |         ALLOCATE(CLASS_GPV(2,0:NUM_OF_ACTIVE_CLASSES))|
01424 msgmmsb6.for                   (for) 01508 |         ALLOCATE(NET_UTILITY_PLANT(2,0:NUM_OF_ACTIVE_CLASSES))|
01425 msgmmsb6.for                   (for) 01509 |         ALLOCATE(CLASS_CWIP(2,0:NUM_OF_ACTIVE_CLASSES))|
01426 msgmmsb6.for                   (for) 01510 |         ALLOCATE(RATE_BASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01427 msgmmsb6.for                   (for) 01511 |         ALLOCATE(DEFERRED_DEBITS_RATE_BASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01428 msgmmsb6.for                   (for) 01512 |         ALLOCATE(|
01429 msgmmsb6.for                   (for) 01514 |         ALLOCATE(NUC_FUEL_RATE_BASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01430 msgmmsb6.for                   (for) 01515 |         ALLOCATE(CWIP_IN_RATE_BASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01431 msgmmsb6.for                   (for) 01516 |         ALLOCATE(NUC_DECOM_RATE_BASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01432 msgmmsb6.for                   (for) 01517 |         ALLOCATE(CIAC_RATE_BASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01433 msgmmsb6.for                   (for) 01518 |         ALLOCATE(PLANT_IN_SERVICE_RATE_BASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01434 msgmmsb6.for                   (for) 01519 |         ALLOCATE(ASSET_NEC_IN_RATE_BASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01435 msgmmsb6.for                   (for) 01520 |         ALLOCATE(CLASS_ADDENDUM_2_RATE_BASE(0:NUM_OF_ACTIVE_CLASSES))|
01436 msgmmsb6.for                   (for) 01521 |         ALLOCATE(POST_RETIREMENT_MEDICAL_FUND(0:NUM_OF_ACTIVE_CLASSES))|
01437 msgmmsb6.for                   (for) 01522 |         ALLOCATE(ACCOUNTS_RECEIVABLE(0:NUM_OF_ACTIVE_CLASSES))|
01438 msgmmsb6.for                   (for) 01523 |         ALLOCATE(CLASS_NUC_DECOM_TRUST_BAL(0:NUM_OF_ACTIVE_CLASSES))|
01439 msgmmsb6.for                   (for) 01524 |         ALLOCATE(CLASS_NUC_DECOM_INC_EARNINGS(0:NUM_OF_ACTIVE_CLASSES))|
01440 msgmmsb6.for                   (for) 01525 |         ALLOCATE(CLASS_NUC_DECOM_BAL_EARNINGS(0:NUM_OF_ACTIVE_CLASSES))|
01441 msgmmsb6.for                   (for) 01526 |         ALLOCATE(FUEL_INVENTORY(0:NUM_OF_ACTIVE_CLASSES))|
01442 msgmmsb6.for                   (for) 01527 |         ALLOCATE(CLASS_MATERIALS_N_SUPPLIES(0:NUM_OF_ACTIVE_CLASSES))|
01443 msgmmsb6.for                   (for) 01528 |         ALLOCATE(CLASS_GAS_STORAGE(0:NUM_OF_ACTIVE_CLASSES))|
01444 msgmmsb6.for                   (for) 01529 |         ALLOCATE(CLASS_DEFERRED_PURCHASE_GAS(0:NUM_OF_ACTIVE_CLASSES))|
01445 msgmmsb6.for                   (for) 01530 |         ALLOCATE(ACQUISITION_PREMIUM(0:NUM_OF_ACTIVE_CLASSES))|
01446 msgmmsb6.for                   (for) 01531 |         ALLOCATE(DEFERRED_FUEL_BALANCE(0:NUM_OF_ACTIVE_CLASSES))|
01447 msgmmsb6.for                   (for) 01532 |         ALLOCATE(ACCOUNTS_PAYABLE(0:NUM_OF_ACTIVE_CLASSES))|
01448 msgmmsb6.for                   (for) 01533 |         ALLOCATE(COMMON_DIVIDEND_PAYMENT(0:NUM_OF_ACTIVE_CLASSES))|
01449 msgmmsb6.for                   (for) 01534 |         ALLOCATE(POST_RETIREMENT_MED_PAYABLE(0:NUM_OF_ACTIVE_CLASSES))|
01450 msgmmsb6.for                   (for) 01535 |         ALLOCATE(NUC_DECOM_FUND_LIABILITY(0:NUM_OF_ACTIVE_CLASSES))|
01451 msgmmsb6.for                   (for) 01536 |         ALLOCATE(LONG_TERM_INVESTMENTS(0:NUM_OF_ACTIVE_CLASSES))|
01452 msgmmsb6.for                   (for) 01537 |         ALLOCATE(DEBT_FILE_INVESTMENTS(0:NUM_OF_ACTIVE_CLASSES))|
01453 msgmmsb6.for                   (for) 01538 |         ALLOCATE(NOTES_RECEIVABLE(0:NUM_OF_ACTIVE_CLASSES))|
01454 msgmmsb6.for                   (for) 01539 |         ALLOCATE(GRE_OTH_PROP_TAXES(0:NUM_OF_ACTIVE_CLASSES))|
01455 msgmmsb6.for                   (for) 01540 |         ALLOCATE(OTHER_DEFERRED_CREDITS_BAL(2,0:NUM_OF_ACTIVE_CLASSES))|
01456 msgmmsb6.for                   (for) 01541 |         ALLOCATE(REGULATORY_DEFERRED_CREDITS_BAL|
01457 msgmmsb6.for                   (for) 01543 |         ALLOCATE(GAIN_ON_REAQUIRED_DEBT_BAL(2,0:NUM_OF_ACTIVE_CLASSES))|
01458 msgmmsb6.for                   (for) 01544 |         ALLOCATE(NOTES_PAYABLE(0:NUM_OF_ACTIVE_CLASSES))|
01459 msgmmsb6.for                   (for) 01545 |         ALLOCATE(|
01460 msgmmsb6.for                   (for) 01547 |         ALLOCATE(|
01461 msgmmsb6.for                   (for) 01549 |         ALLOCATE(CLASS_NET_NF_VALUE(2,0:NUM_OF_ACTIVE_CLASSES))|
01462 msgmmsb6.for                   (for) 01550 |         ALLOCATE(CUMULATIVE_NUCLEAR_BURN(0:NUM_OF_ACTIVE_CLASSES))|
01463 msgmmsb6.for                   (for) 01551 |         ALLOCATE(NET_NUCLEAR_FUEL_ADJ(0:NUM_OF_ACTIVE_CLASSES))|
01464 msgmmsb6.for                   (for) 01552 |         ALLOCATE(CUM_RATE_BASE_NUCLEAR_BURN(0:NUM_OF_ACTIVE_CLASSES))|
01465 msgmmsb6.for                   (for) 01553 |         ALLOCATE(CUM_CAPTURED_OPT_METHOD_REVS(0:NUM_OF_ACTIVE_CLASSES))|
01466 msgmmsb6.for                   (for) 01554 |         ALLOCATE(CLASS_REVENUES(0:MAX_CLASS_NUM))|
01467 msgmmsb6.for                   (for) 01555 |         ALLOCATE(CLASS_NAME(0:NUM_OF_ACTIVE_CLASSES))|
01468 msgmmsb6.for                   (for) 01556 |         ALLOCATE(CLASS_TYPE(0:NUM_OF_ACTIVE_CLASSES))|
01469 msgmmsb6.for                   (for) 01557 |         ALLOCATE(CLASS_PRIOR_INBALANCE(0:NUM_OF_ACTIVE_CLASSES))|
01470 msgmmsb6.for                   (for) 01558 |         ALLOCATE(LINKED_2_CLASS(0:NUM_OF_ACTIVE_CLASSES))|
01471 msgmmsb6.for                   (for) 01559 |         ALLOCATE(YEAR_CLASS_DEACTIVATED(0:NUM_OF_ACTIVE_CLASSES))|
01472 msgmmsb6.for                   (for) 01560 |         ALLOCATE(ACTIVATE_CLASS_IN_YEAR(0:NUM_OF_ACTIVE_CLASSES))|
01473 msgmmsb6.for                   (for) 01561 |         ALLOCATE(LINEC(2,0:NUM_OF_ACTIVE_CLASSES))|
01474 msgmmsb6.for                   (for) 01562 |         ALLOCATE(COMMON_STOCK(2,0:NUM_OF_ACTIVE_CLASSES))|
01475 msgmmsb6.for                   (for) 01563 |         ALLOCATE(COMMON_SHARES(2,0:NUM_OF_ACTIVE_CLASSES))|
01476 msgmmsb6.for                   (for) 01564 |         ALLOCATE(PREFERRED_STOCK(2,0:NUM_OF_ACTIVE_CLASSES))|
01477 msgmmsb6.for                   (for) 01565 |         ALLOCATE(LONG_TERM_DEBT(2,0:NUM_OF_ACTIVE_CLASSES))|
01478 msgmmsb6.for                   (for) 01566 |         ALLOCATE(CUSTOMER_DEPOSITS_BAL(2,0:NUM_OF_ACTIVE_CLASSES))|
01479 msgmmsb6.for                   (for) 01567 |         ALLOCATE(CIAC_BALANCE(2,0:NUM_OF_ACTIVE_CLASSES))|
01480 msgmmsb6.for                   (for) 01568 |         ALLOCATE(NET_DD_BALANCE(2,0:NUM_OF_ACTIVE_CLASSES))|
01481 msgmmsb6.for                   (for) 01569 |         ALLOCATE(OTHER_DD_BALANCE(2,0:NUM_OF_ACTIVE_CLASSES))|
01482 msgmmsb6.for                   (for) 01570 |         ALLOCATE(GOODWILL(2,0:NUM_OF_ACTIVE_CLASSES))|
01483 msgmmsb6.for                   (for) 01571 |         ALLOCATE(|
01484 msgmmsb6.for                   (for) 01573 |         ALLOCATE(|
01485 msgmmsb6.for                   (for) 01575 |         ALLOCATE(CUM_AMORT_ADJUSTMENT(0:NUM_OF_ACTIVE_CLASSES))|
01486 msgmmsb6.for                   (for) 01576 |         ALLOCATE(CUM_GOODWILL_AT(0:NUM_OF_ACTIVE_CLASSES))|
01487 msgmmsb6.for                   (for) 01577 |         ALLOCATE(CUM_REG_ASSET_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01488 msgmmsb6.for                   (for) 01578 |         ALLOCATE(CUM_FASB133_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01489 msgmmsb6.for                   (for) 01579 |         ALLOCATE(CUM_FASB109_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01490 msgmmsb6.for                   (for) 01580 |         ALLOCATE(DEBT_FILE_UNAMORT_INTR_BAL(0:NUM_OF_ACTIVE_CLASSES))|
01491 msgmmsb6.for                   (for) 01581 |         ALLOCATE(|
01492 msgmmsb6.for                   (for) 01583 |         ALLOCATE(|
01493 msgmmsb6.for                   (for) 01585 |         ALLOCATE(CUM_DD_UNAMORT_INTR_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01494 msgmmsb6.for                   (for) 01586 |         ALLOCATE(|
01495 msgmmsb6.for                   (for) 01588 |         ALLOCATE(|
01496 msgmmsb6.for                   (for) 01590 |         ALLOCATE(CAP_LEASES_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01497 msgmmsb6.for                   (for) 01591 |         ALLOCATE(ASSETS_NEC_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01498 msgmmsb6.for                   (for) 01592 |         ALLOCATE(PREFERRED_STOCK_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01499 msgmmsb6.for                   (for) 01593 |         ALLOCATE(LTD_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01500 msgmmsb6.for                   (for) 01594 |         ALLOCATE(STD_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01501 msgmmsb6.for                   (for) 01595 |         ALLOCATE(NUC_DECOM_LIAB_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01502 msgmmsb6.for                   (for) 01596 |         ALLOCATE(LIABS_NEC_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01503 msgmmsb6.for                   (for) 01597 |         ALLOCATE(OTHER_INVESTMENT_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01504 msgmmsb6.for                   (for) 01598 |         ALLOCATE(NOTES_RECEIVABLE_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01505 msgmmsb6.for                   (for) 01599 |         ALLOCATE(|
01506 msgmmsb6.for                   (for) 01601 |         ALLOCATE(DEFERRED_REVENUES_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01507 msgmmsb6.for                   (for) 01602 |         ALLOCATE(DEFERRED_FUEL_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01508 msgmmsb6.for                   (for) 01603 |         ALLOCATE(|
01509 msgmmsb6.for                   (for) 01605 |         ALLOCATE(LT_INVEST_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01510 msgmmsb6.for                   (for) 01606 |         ALLOCATE(ST_INVEST_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01511 msgmmsb6.for                   (for) 01607 |         ALLOCATE(FUEL_INVENTORY_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01512 msgmmsb6.for                   (for) 01608 |         ALLOCATE(GAS_IN_STORAGE_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01513 msgmmsb6.for                   (for) 01609 |         ALLOCATE(MATRIAL_SUPPLY_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01514 msgmmsb6.for                   (for) 01610 |         ALLOCATE(ACCOUNTS_RECV_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01515 msgmmsb6.for                   (for) 01611 |         ALLOCATE(UNBILLED_REV_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01516 msgmmsb6.for                   (for) 01612 |         ALLOCATE(TAXES_RECEIVABLE_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01517 msgmmsb6.for                   (for) 01613 |         ALLOCATE(CURRENT_LT_DEBT_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01518 msgmmsb6.for                   (for) 01614 |         ALLOCATE(NOTES_PAYABLE_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01519 msgmmsb6.for                   (for) 01615 |         ALLOCATE(CUSTOMER_DEPOSIT_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01520 msgmmsb6.for                   (for) 01616 |         ALLOCATE(CICA_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01521 msgmmsb6.for                   (for) 01617 |         ALLOCATE(|
01522 msgmmsb6.for                   (for) 01619 |         ALLOCATE(ACCRUED_PENSION_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01523 msgmmsb6.for                   (for) 01620 |         ALLOCATE(DEFERRED_GAINS_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01524 msgmmsb6.for                   (for) 01621 |         ALLOCATE(storm_RESERVE_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01525 msgmmsb6.for                   (for) 01622 |         ALLOCATE(ACCOUNTS_PAYABLE_BAL_ADJ_AT(0:NUM_OF_ACTIVE_CLASSES))|
01526 msgmmsb6.for                   (for) 01623 |         ALLOCATE(CUM_DD_RATEBASE_ADJUST(0:NUM_OF_ACTIVE_CLASSES))|
01527 msgmmsb6.for                   (for) 01624 |         ALLOCATE(|
01528 msgmmsb6.for                   (for) 01626 |         ALLOCATE(CUMULATIVE_CLASS_OTH_LIAB_SALE_ASSETS|
01529 msgmmsb6.for                   (for) 01628 |         ALLOCATE(CUMULATIVE_GPV_ADJUSTMENTS(0:NUM_OF_ACTIVE_CLASSES))|
01530 msgmmsb6.for                   (for) 01629 |         ALLOCATE(AI_CLASS_CWIP(0:NUM_OF_ACTIVE_CLASSES))|
01531 msgmmsb6.for                   (for) 01630 |         ALLOCATE(PREFERRED_STOCK_ADDENDUM(0:NUM_OF_ACTIVE_CLASSES))|
01532 msgmmsb6.for                   (for) 01631 |         ALLOCATE(LONG_TERM_DEBT_ADDENDUM(0:NUM_OF_ACTIVE_CLASSES))|
01533 msgmmsb6.for                   (for) 01632 |         ALLOCATE(WEIGHTED_CLASS_PS_COST(0:NUM_OF_ACTIVE_CLASSES))|
01534 msgmmsb6.for                   (for) 01633 |         ALLOCATE(WEIGHTED_CLASS_LTD_COST(0:NUM_OF_ACTIVE_CLASSES))|
01535 msgmmsb6.for                   (for) 01634 |         ALLOCATE(CURRENCY_DENOMINATION(0:NUM_OF_ACTIVE_CLASSES))|
01536 msgmmsb6.for                   (for) 01635 |         ALLOCATE(REPORT_CLASS(0:NUM_OF_ACTIVE_CLASSES))|
01537 msgmmsb6.for                   (for) 01636 |         ALLOCATE(REPORT_ZEROS_4_CLASS(0:NUM_OF_ACTIVE_CLASSES))|
01538 msgmmsb6.for                   (for) 01637 |         ALLOCATE(CONSOLIDATING_CLASS(0:NUM_OF_ACTIVE_CLASSES))|
01539 msgmmsb6.for                   (for) 01638 |         ALLOCATE(FED_TAX_EXEMPT(0:NUM_OF_ACTIVE_CLASSES))|
01540 msgmmsb6.for                   (for) 01639 |         ALLOCATE(STD_INTEREST_VECTOR(0:NUM_OF_ACTIVE_CLASSES))|
01541 msgmmsb6.for                   (for) 01640 |         ALLOCATE(LTD_INTEREST_VECTOR(0:NUM_OF_ACTIVE_CLASSES))|
01542 msgmmsb6.for                   (for) 01641 |         ALLOCATE(PS_DIVIDENDS_VECTOR(0:NUM_OF_ACTIVE_CLASSES))|
01543 msgmmsb6.for                   (for) 01642 |         ALLOCATE(STI_INTEREST_VECTOR(0:NUM_OF_ACTIVE_CLASSES))|
01544 msgmmsb6.for                   (for) 01643 |         ALLOCATE(LTI_INTEREST_VECTOR(0:NUM_OF_ACTIVE_CLASSES))|
01545 msgmmsb6.for                   (for) 01644 |         ALLOCATE(|
01546 msgmmsb6.for                   (for) 01646 |         ALLOCATE(DECOM_FUND_INTEREST_VECTOR(0:NUM_OF_ACTIVE_CLASSES))|
01547 msgmmsb6.for                   (for) 01647 |         ALLOCATE(|
01548 msgmmsb6.for                   (for) 01649 |         ALLOCATE(CURRENCY_CONVERSION_VECTOR(0:NUM_OF_ACTIVE_CLASSES))|
01549 msgmmsb6.for                   (for) 01699 |         ALLOCATE(ABANDS(2,0:NUM_OF_ACTIVE_CLASSES))|
01550 msgmmsb6.for                   (for) 01700 |         ALLOCATE(AFDC1C(2,0:NUM_OF_ACTIVE_CLASSES))|
01551 msgmmsb6.for                   (for) 01701 |         ALLOCATE(AFDC2C(2,0:NUM_OF_ACTIVE_CLASSES))|
01552 msgmmsb6.for                   (for) 01702 |         ALLOCATE(AFDCDP(2,0:NUM_OF_ACTIVE_CLASSES))|
01553 msgmmsb6.for                   (for) 01703 |         ALLOCATE(BKDEPC(2,0:NUM_OF_ACTIVE_CLASSES))|
01554 msgmmsb6.for                   (for) 01704 |         ALLOCATE(BOOK_VALUE_STOCK(2,0:NUM_OF_ACTIVE_CLASSES))|
01555 msgmmsb6.for                   (for) 01705 |         ALLOCATE(STOCK_PRICE_YEAR_END(2,0:NUM_OF_ACTIVE_CLASSES))|
01556 msgmmsb6.for                   (for) 01706 |         ALLOCATE(DEFERRED_PURCHASE_POWER(2,0:NUM_OF_ACTIVE_CLASSES))|
01557 msgmmsb6.for                   (for) 01707 |         ALLOCATE(PENSION_LIABILITY(2,0:NUM_OF_ACTIVE_CLASSES))|
01558 msgmmsb6.for                   (for) 01708 |         ALLOCATE(DEFERRED_GAIN_FROM_SALES(2,0:NUM_OF_ACTIVE_CLASSES))|
01559 msgmmsb6.for                   (for) 01709 |         ALLOCATE(STORM_RESERVE_BALANCE(2,0:NUM_OF_ACTIVE_CLASSES))|
01560 msgmmsb6.for                   (for) 01710 |         ALLOCATE(ExecBenefitsBalance(2,0:NUM_OF_ACTIVE_CLASSES))|
01561 msgmmsb6.for                   (for) 01711 |         ALLOCATE(IncentiveCompBalance(2,0:NUM_OF_ACTIVE_CLASSES))|
01562 msgmmsb6.for                   (for) 01712 |         ALLOCATE(ACCRUED_VACATION_PAY_BALANCE(2,|
01563 msgmmsb6.for                   (for) 01714 |         ALLOCATE(RB_INCLUDE_DEFERRED_TAX_DR(2,0:NUM_OF_ACTIVE_CLASSES))|
01564 msgmmsb6.for                   (for) 01715 |         ALLOCATE(RB_INCLUDE_DEFERRED_REVENUES(2,|
01565 msgmmsb6.for                   (for) 01717 |         ALLOCATE(RB_EXCLUDED_PENSION_LIABILITY(2,|
01566 msgmmsb6.for                   (for) 01719 |         ALLOCATE(RB_EXCLUDED_DEFERRED_ASSET_GAIN(2,|
01567 msgmmsb6.for                   (for) 01721 |         ALLOCATE(RB_EXCLUDED_STORM_RESERVE(2,0:NUM_OF_ACTIVE_CLASSES))|
01568 msgmmsb6.for                   (for) 01722 |         ALLOCATE(RB_EXCLUDE_ACCRUED_VACATION_PAY(2,|
01569 msgmmsb6.for                   (for) 01724 |         ALLOCATE(FASB143_NET_ASSET_VALUE(2,0:NUM_OF_ACTIVE_CLASSES))|
01570 msgmmsb6.for                   (for) 01725 |         ALLOCATE(FASB143_LT_LIABILITY_VALUE(2,0:NUM_OF_ACTIVE_CLASSES))|
01571 msgmmsb6.for                   (for) 01726 |         ALLOCATE(PROPERTY_TAX_BASIS(2,0:NUM_OF_ACTIVE_CLASSES))  ! 91 BASE YEA)|
01572 msgmmsb6.for                   (for) 01727 |         ALLOCATE(FASB_87_INTANGIBLE_ASSETS(0:NUM_OF_ACTIVE_CLASSES))|
01573 msgmmsb6.for                   (for) 01728 |         ALLOCATE(OTHER_COMPREHENSIVE_INCOME_BALANCE(|
01574 msgmmsb6.for                   (for) 01730 |         ALLOCATE(OPERATING_REVENUE_TAX_LAGGED_BASIS(|
01575 msgmmsb6.for                   (for) 01732 |         ALLOCATE(TOTAL_CAPITIAL(2,0:NUM_OF_ACTIVE_CLASSES))|
01576 msgmmsb6.for                   (for) 01733 |         ALLOCATE(CEPCUM(2,0:NUM_OF_ACTIVE_CLASSES))|
01577 msgmmsb6.for                   (for) 01734 |         ALLOCATE(COMMON_EQUITY(2,0:NUM_OF_ACTIVE_CLASSES))|
01578 msgmmsb6.for                   (for) 01735 |         ALLOCATE(DDAMTC(2,0:NUM_OF_ACTIVE_CLASSES))|
01579 msgmmsb6.for                   (for) 01736 |         ALLOCATE(DEPCUM(2,0:NUM_OF_ACTIVE_CLASSES))|
01580 msgmmsb6.for                   (for) 01737 |         ALLOCATE(DEF_ITC_BALANCE(2,0:NUM_OF_ACTIVE_CLASSES))|
01581 msgmmsb6.for                   (for) 01738 |         ALLOCATE(NAFC1C(2,0:NUM_OF_ACTIVE_CLASSES))|
01582 msgmmsb6.for                   (for) 01739 |         ALLOCATE(NAFC2C(2,0:NUM_OF_ACTIVE_CLASSES))|
01583 msgmmsb6.for                   (for) 01740 |         ALLOCATE(NF_IN_PROCESS(2,0:NUM_OF_ACTIVE_CLASSES))|
01584 msgmmsb6.for                   (for) 01741 |         ALLOCATE(NFIS(2,0:NUM_OF_ACTIVE_CLASSES))|
01585 msgmmsb6.for                   (for) 01742 |         ALLOCATE(NNFV(2,0:NUM_OF_ACTIVE_CLASSES))|
01586 msgmmsb6.for                   (for) 01743 |         ALLOCATE(NPV(2,0:NUM_OF_ACTIVE_CLASSES))|
01587 msgmmsb6.for                   (for) 01744 |         ALLOCATE(OTHER_ASSETS(2,0:NUM_OF_ACTIVE_CLASSES))|
01588 msgmmsb6.for                   (for) 01745 |         ALLOCATE(PLANT(2,0:NUM_OF_ACTIVE_CLASSES))|
01589 msgmmsb6.for                   (for) 01746 |         ALLOCATE(RBCWIP(2,0:NUM_OF_ACTIVE_CLASSES))|
01590 msgmmsb6.for                   (for) 01747 |         ALLOCATE(RBDD(2,0:NUM_OF_ACTIVE_CLASSES))|
01591 msgmmsb6.for                   (for) 01748 |         ALLOCATE(RBNF(2,0:NUM_OF_ACTIVE_CLASSES))|
01592 msgmmsb6.for                   (for) 01749 |         ALLOCATE(RB_NF_IN_PROCESS(2,0:NUM_OF_ACTIVE_CLASSES))|
01593 msgmmsb6.for                   (for) 01750 |         ALLOCATE(REARNC(2,0:NUM_OF_ACTIVE_CLASSES))|
01594 msgmmsb6.for                   (for) 01751 |         ALLOCATE(SALES(2,0:NUM_OF_ACTIVE_CLASSES))|
01595 msgmmsb6.for                   (for) 01752 |         ALLOCATE(SHORT_TERM_DEBT(2,0:NUM_OF_ACTIVE_CLASSES))|
01596 msgmmsb6.for                   (for) 01753 |         ALLOCATE(|
01597 msgmmsb6.for                   (for) 01755 |         ALLOCATE(DEF_TAX_CR_BALANCE(2,0:NUM_OF_ACTIVE_CLASSES))|
01598 msgmmsb6.for                   (for) 01756 |         ALLOCATE(WCCUM(2,0:NUM_OF_ACTIVE_CLASSES))|
01599 msgmmsb6.for                   (for) 01757 |         ALLOCATE(OPERATING_REVENUES(2,0:NUM_OF_ACTIVE_CLASSES))|
01600 msgmmsb6.for                   (for) 01758 |         ALLOCATE(STD_RATE(2,0:NUM_OF_ACTIVE_CLASSES))|
01601 msgmmsb6.for                   (for) 01759 |         ALLOCATE(LIQUID_INVESTMENTS(2,0:NUM_OF_ACTIVE_CLASSES))|
01602 msgmmsb6.for                   (for) 01760 |         ALLOCATE(NUC_DECOM_FUND_BAL(2,0:NUM_OF_ACTIVE_CLASSES))|
01603 msgmmsb6.for                   (for) 01761 |         ALLOCATE(CUMTXNORM(2,0:NUM_OF_ACTIVE_CLASSES))|
01604 msgmmsb6.for                   (for) 01762 |         ALLOCATE(BASE_REVENUE(2,0:NUM_OF_ACTIVE_CLASSES))|
01605 msgmmsb6.for                   (for) 01763 |         ALLOCATE(OTHER_REVENUE(2,0:NUM_OF_ACTIVE_CLASSES))|
01606 msgmmsb6.for                   (for) 01764 |         ALLOCATE(ADJ_CLAUSE_REVENUE(2,0:NUM_OF_ACTIVE_CLASSES))|
01607 msgmmsb6.for                   (for) 01765 |         ALLOCATE(OTHER_LT_LIABILITY(2,0:NUM_OF_ACTIVE_CLASSES))|
01608 msgmmsb6.for                   (for) 01766 |         ALLOCATE(CUM_DEF_REVENUES(2,0:NUM_OF_ACTIVE_CLASSES))|
01609 msgmmsb6.for                   (for) 01767 |         ALLOCATE(NPV_RATEBASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01610 msgmmsb6.for                   (for) 01768 |         ALLOCATE(CUM_ITC_DEF_RATEBASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01611 msgmmsb6.for                   (for) 01769 |         ALLOCATE(RBCWIP_AFDC_METH2(2,0:NUM_OF_ACTIVE_CLASSES))|
01612 msgmmsb6.for                   (for) 01770 |         ALLOCATE(CUM_DEF_TAX_RATEBASE(2,0:NUM_OF_ACTIVE_CLASSES))|
01613 msgmmsb6.for                   (for) 01771 |         ALLOCATE(RB_REG_DD(2,0:NUM_OF_ACTIVE_CLASSES))|
01614 msgmmsb6.for                   (for) 01772 |         ALLOCATE(DEFERRED_EXPENSE_BALANCE(2,0:NUM_OF_ACTIVE_CLASSES))|
01615 msgmmsb6.for                   (for) 01773 |         ALLOCATE(AMTBKPREF(2,0:NUM_OF_ACTIVE_CLASSES))|
01616 msgmmsb6.for                   (for) 01774 |         ALLOCATE(AMTNEGBKPREF(2,0:NUM_OF_ACTIVE_CLASSES))|
01617 msgmmsb6.for                   (for) 01775 |         ALLOCATE(AVAILCREDIT(2,0:NUM_OF_ACTIVE_CLASSES))|
01618 msgmmsb6.for                   (for) 01776 |         ALLOCATE(INVESTMENT_IN_SUBSIDIARIES(2,0:NUM_OF_ACTIVE_CLASSES))|
01619 msgmmsb6.for                   (for) 01777 |         ALLOCATE(CAPITIALZIED_LEASES(2,0:NUM_OF_ACTIVE_CLASSES))|
01620 msgmmsb6.for                   (for) 01778 |         ALLOCATE(INIT_PLANNING_PEAK(3,0:NUM_OF_ACTIVE_CLASSES))|
01621 msgmmsb6.for                   (for) 01786 |         ALLOCATE(ANNINT(0:MAX_FINANCIAL_SIMULATION_YEARS-1,|
01622 msgmmsb6.for                   (for) 01788 |         ALLOCATE(ANN_PS_DIV(0:MAX_FINANCIAL_SIMULATION_YEARS-1,|
01623 msgmmsb6.for                   (for) 01790 |         ALLOCATE(ANN_LTD_INT(0:MAX_FINANCIAL_SIMULATION_YEARS-1,|
01624 msgmmsb6.for                   (for) 01792 |         ALLOCATE(LTDINT(0:MAX_FINANCIAL_SIMULATION_YEARS-1,|
01625 msgmmsb6.for                   (for) 01794 |         ALLOCATE(LTDRET(0:MAX_FINANCIAL_SIMULATION_YEARS-1,|
01626 msgmmsb6.for                   (for) 01796 |         ALLOCATE(PSDIV(0:MAX_FINANCIAL_SIMULATION_YEARS-1,|
01627 msgmmsb6.for                   (for) 01798 |         ALLOCATE(PSRED(0:MAX_FINANCIAL_SIMULATION_YEARS-1,|
01628 msgmmsb6.for                   (for) 01800 |         ALLOCATE(RATE_REVENUE(0:MAX_FINANCIAL_SIMULATION_YEARS-1,|
01629 msgmmsb6.for                   (for) 01802 |         ALLOCATE(NET_DEFERRED_EXPENSE_CASH(|
01630 msgmmsb6.for                   (for) 01805 |         ALLOCATE(ANNUAL_DEFERRED_CASH_AMORT(|
01631 msgmmsb6.for                   (for) 01813 |         ALLOCATE(WEIGHTED_COST_OF_CAPITAL_BY(0:NUM_OF_ACTIVE_CLASSES))|
01632 msgmmsb6.for                   (for) 01814 |         ALLOCATE(PARENT_SUB_CIAC_BALANCE(0:NUM_OF_ACTIVE_CLASSES))|
01633 msgmmsb6.for                   (for) 01815 |         ALLOCATE(CIAC_CLASS_BALANCE(0:NUM_OF_ACTIVE_CLASSES))|
01634 msgmmsb6.for                   (for) 01819 |         ALLOCATE(|
01635 msgmmsb6.for                   (for) 01821 |         ALLOCATE(OPERATING_ASSETS_YEAR_END(0:NUM_OF_ACTIVE_CLASSES))|
01636 msgmmsb6.for                   (for) 02087 |         ALLOCATE(BALANCE_SHEET_ROLLUPS(2,0:LAST_TRANSFER_ITEM,|
01637 msgmmsb6.for                   (for) 02093 |         ALLOCATE(LEVEL_SUMATION(0:VARIABLE_NUMBER-1,|
01638 msgmmsb6.for                   (for) 02097 |         ALLOCATE(NUC_FUEL_RATE_BASE_AT(0:MAX_LINKED_LEVEL))|
01639 msgmmsb6.for                   (for) 02098 |         ALLOCATE(CWIP_IN_RATE_BASE_AT(0:MAX_LINKED_LEVEL))|
01640 msgmmsb6.for                   (for) 02099 |         ALLOCATE(OPENING_CASH_BALANCES(0:NUM_OF_ACTIVE_CLASSES))|
01641 msgmmsb6.for                   (for) 02106 |         ALLOCATE(SUB_INVESTMENT_IN_SUBS(0:MAX_LINKED_LEVEL))|
01642 msgmmsb6.for                   (for) 03526 |         ALLOCATE(SUB_TOTAL_SUB_INCOME(-1:DEFINED_CLASSES))|
01643 msgmmsb6.for                   (for) 03527 |         ALLOCATE(SUB_SBU_ACE_TAX_DEPRECIATION(-1:DEFINED_CLASSES))|
01644 msgmmsb6.for                   (for) 03528 |         ALLOCATE(SUB_SBU_TAX_PRE_DEPRECIATION(-1:DEFINED_CLASSES))|
01645 msgmmsb6.for                   (for) 03529 |         ALLOCATE(SBU_RETIREE_MEDICAL_PAYMENTS(-1:DEFINED_CLASSES))|
01646 msgmmsb6.for                   (for) 03530 |         ALLOCATE(SBU_CASH_OTH_ASSET(-1:DEFINED_CLASSES))|
01647 msgmmsb6.for                   (for) 03531 |         ALLOCATE(SBU_DSM_CAPITIAL(-1:DEFINED_CLASSES))|
01648 msgmmsb6.for                   (for) 03532 |         ALLOCATE(SBU_BTL_LEASE_PAYMENTS(-1:DEFINED_CLASSES))|
01649 msgmmsb6.for                   (for) 03533 |         ALLOCATE(SBU_ATL_LEASE_PAYMENTS(-1:DEFINED_CLASSES))|
01650 msgmmsb6.for                   (for) 03534 |         ALLOCATE(SBU_CHANGE_DECOMMISSIONING_LIAB(-1:DEFINED_CLASSES))|
01651 msgmmsb6.for                   (for) 03547 |         ALLOCATE(PROPERTY_VALUE_GPV_WITH_CWIP(0:MAX_LINKED_LEVEL))|
01652 msgmmsb6.for                   (for) 03548 |         ALLOCATE(PROPERTY_VALUE_GPV_NO_CWIP(0:MAX_LINKED_LEVEL))|
01653 msgmmsb6.for                   (for) 03549 |         ALLOCATE(PROPERTY_VALUE_NPV_WITH_CWIP(0:MAX_LINKED_LEVEL))|
01654 msgmmsb6.for                   (for) 03550 |         ALLOCATE(PROPERTY_VALUE_NPV_NO_CWIP(0:MAX_LINKED_LEVEL))|
01655 msgmmsb6.for                   (for) 03551 |         ALLOCATE(EXCLUDED_SBU_PROPERTY_TAX(0:MAX_LINKED_LEVEL))|
01656 msgmmsb6.for                   (for) 03552 |         ALLOCATE(SUB_EARNINGS_2_COMMON(0:MAX_LINKED_LEVEL))|
01657 msgmmsb6.for                   (for) 03553 |         ALLOCATE(SUB_DIVIDEND_PAYMENTS_2_PARENT(0:MAX_LINKED_LEVEL))|
01658 msgmmsb6.for                   (for) 03554 |         ALLOCATE(SUB_EQUITY_NEEDS_FROM_PARENT(0:MAX_LINKED_LEVEL))|
01659 msgmmsb6.for                   (for) 03555 |         ALLOCATE(PARENT_ACQUISITION_OF_SUB_COST(0:MAX_LINKED_LEVEL))|
01660 msgmmsb6.for                   (for) 03556 |         ALLOCATE(PARENT_ACQUISITION_GOODWILL(0:MAX_LINKED_LEVEL))|
01661 msgmmsb6.for                   (for) 03557 |         ALLOCATE(SUB_EARNINGS_2_EXCLUDE(0:MAX_LINKED_LEVEL))|
01662 msgmmsb6.for                   (for) 03558 |         ALLOCATE(NUC_FUEL_RATE_BASE_AT(0:MAX_LINKED_LEVEL))|
01663 msgmmsb6.for                   (for) 03559 |         ALLOCATE(CWIP_IN_RATE_BASE_AT(0:MAX_LINKED_LEVEL))|
01664 msgmmsb6.for                   (for) 03560 |         ALLOCATE(SBU_DEFERRED_DEBIT_RATEBASE(0:MAX_LINKED_LEVEL))|
01665 msgmmsb6.for                   (for) 03561 |         ALLOCATE(SBU_DEFERRED_CREDIT_RATEBASE(0:MAX_LINKED_LEVEL))|
01666 msgmmsb6.for                   (for) 03562 |         ALLOCATE(SBU_CLASS_OTH_LIAB_SALE_ASSETS(0:MAX_LINKED_LEVEL))|
01667 msgmmsb6.for                   (for) 03563 |         ALLOCATE(SBU_ATL_TAX_EXPENSE(0:MAX_LINKED_LEVEL))|
01668 msgmmsb6.for                   (for) 03564 |         ALLOCATE(SBU_BTL_TAX_EXPENSE(0:MAX_LINKED_LEVEL))|
01669 msgmmsb6.for                   (for) 03585 |         ALLOCATE(EXCLUDED_REVENUE_TAX_BASIS(0:MAX_LINKED_LEVEL))|
01670 msgmmsb6.for                   (for) 03586 |         ALLOCATE(EXCLUDED_REVENUE_TAX_BASIS_A(0:MAX_LINKED_LEVEL))|
01671 msgmmsb6.for                   (for) 03587 |         ALLOCATE(EXCLUDED_REVENUE_TAX_BASIS_B(0:MAX_LINKED_LEVEL))|
01672 msgmmsb6.for                   (for) 03588 |         ALLOCATE(EXCLUDED_REVENUE_TAX(0:MAX_LINKED_LEVEL))|
01673 msgmmsb6.for                   (for) 03589 |         ALLOCATE(EXCLUDED_OTHER_TAXES_REV(0:MAX_LINKED_LEVEL))|
01674 msgmmsb6.for                   (for) 03590 |         ALLOCATE(EXCLUDED_OTHER_TAXES_EXP(0:MAX_LINKED_LEVEL))|
01675 msgmmsb6.for                   (for) 03591 |         ALLOCATE(EXCLUDED_OTHER_TAXES(0:MAX_LINKED_LEVEL))|
01676 msgmmsb6.for                   (for) 03592 |         ALLOCATE(EXCLUDE_CAPITAL_TAXES_STATE(0:MAX_LINKED_LEVEL))|
01677 msgmmsb6.for                   (for) 03593 |         ALLOCATE(EXCLUDE_CAPITAL_TAXES_FED(0:MAX_LINKED_LEVEL))|
01678 msgmmsb6.for                   (for) 03594 |         ALLOCATE(EXCLUDE_CAPITAL_TAX_FED_BASIS(0:MAX_LINKED_LEVEL))|
01679 msgmmsb6.for                   (for) 03595 |         ALLOCATE(EXCLUDE_CAPITAL_TAX_STATE_BASIS(0:MAX_LINKED_LEVEL))|
01680 msgmmsb6.for                   (for) 13141 |         ALLOCATE(SBU_LINKED_LIST(-1:1024,0:MAX_LINKED_CLASSES))|
01681 msgmmsb6.for                   (for) 13142 |         ALLOCATE(LINKED_TO(0:MAX_INIT_CLASS_NUM))|
01682 msgmmsb6.for                   (for) 13144 |         ALLOCATE(LINKED_TO_COMPANY(0:MAX_INIT_CLASS_NUM))|
01683 msgmmsb6.for                   (for) 13149 |     +                          ALLOCATE(SUBs_LINKED_TO_PARENT(-1:1024))|
01684 msgmmsb6.for                   (for) 13542 |      ALLOCATE(DEFINED_CLASS_POINTER_LIST(-1:DEFINED_CLASSES))|
01685 msgmmsb6.for                   (for) 13543 |      ALLOCATE(CLASS_LINKED_LEVEL(DEFINED_CLASSES))|
01686 msgmmsb6.for                   (for) 13544 |      ALLOCATE(TEMP_LIST(DEFINED_CLASSES))|
01687 msgmmsb6.for                   (for) 13545 |      ALLOCATE(TEMP_LIST2(DEFINED_CLASSES))|
01688 msgmmsb6.for                   (for) 13546 |      ALLOCATE(TEMP_CLASS_LINKED_LEVEL(DEFINED_CLASSES))|
01689 msgmmsb6.for                   (for) 13547 |      ALLOCATE(CLASS_AT_LEVEL(-1:DEFINED_CLASSES))|
01690 msgmmsb6b.f90                  (f90) 00534 |         ALLOCATE(REPORTING_VARIABLES(0:NUM_ACTIVE_VARIABLES))|
01691 msgmmsb6b.f90                  (f90) 02202 |         ALLOCATE(TRANSFR_FUEL_COST_AMOUNT(0:12,NUMBER_OF_COST_CLASSES))|
01692 msgmmsb6b.f90                  (f90) 02203 |         ALLOCATE(TRANSFR_VAR_COST_AMOUNT(0:12,NUMBER_OF_COST_CLASSES))|
01693 msgmmsb6b.f90                  (f90) 02204 |         ALLOCATE(TRANSFR_COST_QUANTITIES(0:12,NUMBER_OF_COST_CLASSES))|
01694 msgmmsb6b.f90                  (f90) 02205 |         ALLOCATE(TRANSFR_COST_CLASS_POINTR(0:MAX_COST_CLASS_NUMBER+1))|
01695 msgmmsb6b.f90                  (f90) 02291 |         ALLOCATE(TRANSFR_REV_AMOUNT(0:12,NUMBER_OF_REV_CLASSES),TRANSFR_REV_QUANTITIES(0:12,NUMBER_OF_REV_CLASSES), &|
01696 msgmmtax.f90                   (f90) 00082 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01697 msgmmtax.f90                   (f90) 00136 |            ALLOCATE(BC_ASSET_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))|
01698 msgmmtax.f90                   (f90) 00175 |      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
01699 msgmmtax.f90                   (f90) 00217 |         ALLOCATE(OL_ASSET_CLASS_POINTER(MAX_OL_CLASS_ID_NUM))|
01700 msgmmtax.f90                   (f90) 00485 |      ALLOCATE(DATA_VALUES(0:AVAIL_DATA_YEARS),ANNUAL_TAX_VALUES(0:FINANCIAL_SIMULATION_YEARS), &|
01701 msgmmtax.f90                   (f90) 00784 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_CLASS_NUM))|
01702 msgmmtax.f90                   (f90) 00790 |         ALLOCATE(TAX_VALUES(FINANCIAL_SIMULATION_YEARS,0:12,-5:NUM_OF_CLASSES,LAST_TAX_ITEM,3))|
01703 msgmmtax.f90                   (f90) 00792 |         ALLOCATE(TAX_VALUES_NOLS(0:15,0:12,-5:NUM_OF_CLASSES,35:40))|
01704 msgmmtax.f90                   (f90) 00794 |         ALLOCATE(ACTUAL_MONTHLY_TAX_VALUES(0:12,FINANCIAL_SIMULATION_YEARS,-1:NUM_OF_CLASSES,LAST_ACTUAL_TAX_ITEM))|
01705 msgmmtax.f90                   (f90) 00797 |         ALLOCATE(ACTUAL_VALUE_FOUND(-1:NUM_OF_CLASSES,LAST_ACTUAL_TAX_ITEM))|
01706 msgmmtax.f90                   (f90) 01663 |            ALLOCATE(ASSET_CLASS_POINTER(BASE_CASE_MAX_CLASS_NUM))|
01707 msgmmtax.f90                   (f90) 01668 |         ALLOCATE(TAX_VALUES(FINANCIAL_SIMULATION_YEARS,0:12,-5:BASE_CASE_NUM_OF_CLASSES,LAST_TAX_ITEM,3))|
01708 msgmmtax.f90                   (f90) 01669 |         ALLOCATE(TAX_VALUES_NOLS(0:15,0:12,-5:BASE_CASE_NUM_OF_CLASSES,35:40))|
01709 msgmmtax.f90                   (f90) 01670 |         ALLOCATE(ACTUAL_MONTHLY_TAX_VALUES(0:12,FINANCIAL_SIMULATION_YEARS,-1:BASE_CASE_NUM_OF_CLASSES,LAST_ACTUAL_TAX_ITEM))|
01710 msgmmtax.f90                   (f90) 01671 |         ALLOCATE(ACTUAL_VALUE_FOUND(-1:BASE_CASE_NUM_OF_CLASSES,LAST_ACTUAL_TAX_ITEM))|
01711 msgmmtax.f90                   (f90) 01974 |         ALLOCATE(FEDERAL_BOOK_LOSSES(-1:YRS,-1:MAX_CLASS_NUM),SEC_42_CREDITS(-1:YRS,-1:MAX_CLASS_NUM), &|
01712 msgoutpt.for                   (for) 00326 |     +                       ALLOCATE(VARIABLE_VALUE(0:LAST_VARIABLE()))|
01713 msgoutpt.for                   (for) 01315 |            ALLOCATE(OUTPUT_VARIABLE(SHFRM_VARIABLES_USED),STAT=ERROR)|
01714 msgoutpt.for                   (for) 01331 |         ALLOCATE(OUTPUT_VARIABLE(AN_DECOMP_VARIABLES_USED),STAT=ERROR)|
01715 msgoutpt.for                   (for) 01369 |         ALLOCATE(VARIABLE_LIST(0:ACTIVE_VARIABLES-1))|
01716 msgoutpt.for                   (for) 01413 |         ALLOCATE(VARIABLE_ACTIVE(0:LAST_VARIABLE()))|
01717 msgoutpt.for                   (for) 01447 |     +                     ALLOCATE(VARIABLE_LIST(0:ACTIVE_VARIABLES-1))|
01718 msgoutpt.for                   (for) 01743 |            ALLOCATE(PA_EXISTING_PLANNING_CAPACITY(|
01719 msgoutpt.for                   (for) 01745 |            ALLOCATE(PA_NEW_PLANNING_CAPACITY(0:10,MAX_ACTIVE_GROUPS))|
01720 msgoutpt.for                   (for) 01746 |            ALLOCATE(PA_NEWGEN_CAPACITY(10,MAX_ACTIVE_GROUPS))|
01721 msgoutpt.for                   (for) 01747 |            ALLOCATE(PA_AFTER_PEAK_CAPACITY(MAX_ACTIVE_GROUPS))|
01722 msgoutpt.for                   (for) 01748 |            ALLOCATE(PA_RETIREMENT_CAPACITY(MAX_ACTIVE_GROUPS))|
01723 msgoutpt.for                   (for) 01749 |            ALLOCATE(PA_INTERRUPTIBLE_LOAD(MAX_ACTIVE_GROUPS))|
01724 msgoutpt.for                   (for) 01750 |            ALLOCATE(PA_TRANSFER_CAPACITY(MAX_ACTIVE_GROUPS))|
01725 msgoutpt.for                   (for) 01751 |            ALLOCATE(PA_CAPACITY_PLANNING_PEAK(MAX_ACTIVE_GROUPS))|
01726 msgoutpt.for                   (for) 01752 |            ALLOCATE(TG_2_PLANNING_AREA(MAX_ACTIVE_GROUPS))|
01727 msgprosymrevexp.f90            (f90) 00137 |            ALLOCATE(ProSymRevAndExps(0:12,BaseStartYr:BaseEndYr, &|
01728 msgprosymrevexp.f90            (f90) 00164 |      ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
01729 msgprosymrevexp.f90            (f90) 00166 |      ALLOCATE(ProSymRevAndExps(0:12,StartYr:EndYr, &|
01730 msgprosymrevexp.f90            (f90) 00173 |      ALLOCATE(VALUES(DataVariables,StartYr:EndYr))|
01731 msgprosymrevexp.f90            (f90) 00291 |         ALLOCATE(ResourceList(ResourcesInFile))     |
01732 msgprosymrevexp.f90            (f90) 00293 |         ALLOCATE(ResourceListIDs(ResourcesInFile))|
01733 newfred2.for                   (for) 00289 |      ALLOCATE(LODA(12,31,24))|
01734 newfred2.for                   (for) 00689 |            ALLOCATE(ENERGY_CLASS_SPLIT(12,MAX_LOAD_CLASSES),|
01735 newfred2.for                   (for) 01345 |      ALLOCATE(CLASS_COINCIDENT_PEAK(MAX_LOAD_CLASSES),|
01736 newfred2.for                   (for) 01388 |            ALLOCATE(SPLIT_DAY_TYPE_ENERGY(2,12,MAX_LOAD_CLASSES),|
01737 newfred2.for                   (for) 01391 |         ALLOCATE(OVERLAY_TYPE(MAX_LOAD_CLASSES),|
01738 newfred2.for                   (for) 01865 |            ALLOCATE(ASSET_CLASS_POINTER(MAX_RATE_CLASS_ID_NUM))|
01739 newfred2.for                   (for) 01869 |         ALLOCATE(RATE_CLASS_ENRG_REVENUES(0:NUMBER_OF_RATE_CLASSES),|
01740 newfred2.for                   (for) 01884 |         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS),|
01741 newfred2.for                   (for) 02193 |         ALLOCATE(FORECAST_ENERGY(2,12,MAX_LOAD_CLASSES),|
01742 objtfunc.f90                   (f90) 00099 |         ALLOCATE(OPERATION_TYPE(QUE_STACK_POINTR), &|
01743 objtfunc.f90                   (f90) 00129 |         ALLOCATE(PARTIAL_OBJFUNC_DES(NUM_OF_VARIABLES), &|
01744 pf_objt.f90                    (f90) 00248 |         ALLOCATE(BC_TABLE_POSITION_FOR(0:MAX_BC_CLASS_NUMBER))|
01745 pf_objt.f90                    (f90) 00407 |      ALLOCATE(OL_TABLE_POSITION_FOR(0:MAX_OL_CLASS_NUMBER))|
01746 picadisp.f90                   (f90) 00140 |         ALLOCATE(MAX_BLOCKS_PER_TRANS(0:UPPER_TRANS_GROUP)) ! 0 IS THE MAX OF ALL GROUPS|
01747 picadisp.f90                   (f90) 00152 |      ALLOCATE(BLOCK_DISP_COST(NUNITS,2),SECOND_DISP_COST(NUNITS,2))|
01748 picadisp.f90                   (f90) 00154 |      ALLOCATE(SHADOW_DISPATCH_ADDER(NBLOK))|
01749 picadisp.f90                   (f90) 00306 |            ALLOCATE(LAMDA_BLK1(BLK1_DISPATCH),BLKNO_BLK1(BLK1_DISPATCH),INHEAT_BLK1(BLK1_DISPATCH),MWBLOK_BLK1(BLK1_DISPATCH), &|
01750 picadisp.f90                   (f90) 00312 |            ALLOCATE(LAMDA_BLK2(BLK2_DISPATCH),BLKNO_BLK2(BLK2_DISPATCH),INHEAT_BLK2(BLK2_DISPATCH),MWBLOK_BLK2(BLK2_DISPATCH), &|
01751 picadisp.f90                   (f90) 00317 |            ALLOCATE(LAMDA_MR(MUST_RUN_BLOCKS),BLKNO_MR(MUST_RUN_BLOCKS),INHEAT_MR(MUST_RUN_BLOCKS),MWBLOK_MR(MUST_RUN_BLOCKS), &|
01752 picadisp.f90                   (f90) 00356 |         allocate(first_blk_used(MAX_CL_UNITS))|
01753 picadisp.f90                   (f90) 00364 |            ALLOCATE(PosBLK0(BLK0))|
01754 picadisp.f90                   (f90) 00391 |            ALLOCATE(PosBLK1(BLK1))|
01755 picadisp.f90                   (f90) 00392 |            ALLOCATE(LAMDA_TEMP(BLK1_DISPATCH))|
01756 picadisp.f90                   (f90) 00405 |            ALLOCATE(LAMDA_TEMP(BLK2_DISPATCH))|
01757 picadisp.f90                   (f90) 00408 |            ALLOCATE(PosBLK2(BLK2))|
01758 picadisp.f90                   (f90) 00414 |            ALLOCATE(SECOND_BLK_PASSED(BLK2))|
01759 picadisp.f90                   (f90) 00702 |      ALLOCATE(BLOCK_DISP_COST(NUNITS,2),SECOND_DISP_COST(NUNITS,2))|
01760 picadisp.f90                   (f90) 00704 |      ALLOCATE(SHADOW_DISPATCH_ADDER(NBLOK))|
01761 postfix.f90                    (f90) 00040 |      ALLOCATE(PARTIAL_OBJFUNC_DES(50),GROUP_STRING(50),   &|
01762 postfix.f90                    (f90) 00226 |         ALLOCATE(OPERATOR_STACK(0:256),   &|
01763 pro_cost.f90                   (f90) 00669 |         ALLOCATE(ENERGY(MAX_SIMULATION_YEARS), &|
01764 pro_cost.f90                   (f90) 00716 |            ALLOCATE(NOX_SEASON_REGION(MAX_TRANS_GROUP_NUMBER), &|
01765 pro_cost.f90                   (f90) 01028 |         ALLOCATE(BLENDED_BTU_COST(MAX(NUNITS,INT(1,2))))|
01766 pro_cost.f90                   (f90) 01035 |         ALLOCATE(ANNUAL_BLENDED_BTU_COST(MAX(NUNITS,INT(1,2))))|
01767 pro_cost.f90                   (f90) 01036 |         ALLOCATE(ANNUAL_FUEL_BTU_COST(MAX(NUNITS,INT(1,2))))|
01768 pro_cost.f90                   (f90) 01037 |         ALLOCATE(ANNUAL_DISP_BTU_COST(MAX(NUNITS,INT(1,2))))|
01769 pro_cost.f90                   (f90) 02778 |            ALLOCATE(FIRST_HYDRO_LOAD_NO(0:NUMBER_OF_HYDRO_LOAD_GROUPS), &|
01770 rdi_objt.f90                   (f90) 00143 |      ALLOCATE (RDI_VARIABLE_NAME(NUMBER_OF_RDI_VARIABLES))|
01771 rdi_objt.f90                   (f90) 00562 |         ALLOCATE(MONTHS_PER_PLANT(MAX_RDI_PLANTS), &|
01772 rdi_objt.f90                   (f90) 01387 |      ALLOCATE (RBC_VARIABLE_NAME(NUMBER_OF_RBC_VARIABLES))|
01773 rdi_objt.f90                   (f90) 01843 |         ALLOCATE(MONTHS_PER_PLANT(MAX_RBC_PLANTS))|
01774 reaenrg.for                    (for) 00468 |         ALLOCATE(SUM_ANNUAL_REPORT_ITEMS(13,NUMBER_OF_AREAS+2))|
01775 reaenrg.for                    (for) 02129 |            ALLOCATE(ANNUAL_CL_DISPATCHED_ENRG(CL_UNITS))|
01776 reaenrg.for                    (for) 02130 |            ALLOCATE(ANNUAL_CL_DISPATCHED_CAP(CL_UNITS))|
01777 reaenrg.for                    (for) 02131 |            ALLOCATE(ANNUAL_CL_LOSSES(CL_UNITS))|
01778 reaenrg.for                    (for) 02132 |            ALLOCATE(ANNUAL_CL_PURCHASED_ENRG(CL_UNITS))|
01779 reaenrg.for                    (for) 02133 |            ALLOCATE(ANNUAL_CL_PURCHASED_CAP(CL_UNITS))|
01780 reaenrg.for                    (for) 02134 |            ALLOCATE(ANNUAL_CL_FIXED_CHARGE(CL_UNITS))|
01781 reaenrg.for                    (for) 02135 |            ALLOCATE(ANNUAL_CL_FUEL_CHARGE(CL_UNITS))|
01782 reaenrg.for                    (for) 02136 |            ALLOCATE(ANNUAL_CL_VARIABLE_CHARGE(CL_UNITS))|
01783 reaenrg.for                    (for) 02137 |            ALLOCATE(ANNUAL_CL_EMISSION_COST(CL_UNITS))|
01784 reaenrg.for                    (for) 02138 |            ALLOCATE(ANNUAL_CL_MAX_DISPATCHED_CAP(CL_UNITS))|
01785 reaenrg.for                    (for) 02139 |            ALLOCATE(ANNUAL_CL_MAX_PURCHASED_CAP(CL_UNITS))|
01786 reaenrg.for                    (for) 02176 |            ALLOCATE(ANNUAL_CT_DISPATCHED_ENRG(CONTRACTS))|
01787 reaenrg.for                    (for) 02177 |            ALLOCATE(ANNUAL_CT_DISPATCHED_CAP(CONTRACTS))|
01788 reaenrg.for                    (for) 02178 |            ALLOCATE(ANNUAL_CT_LOSSES(CONTRACTS))|
01789 reaenrg.for                    (for) 02179 |            ALLOCATE(ANNUAL_CT_PURCHASED_ENRG(CONTRACTS))|
01790 reaenrg.for                    (for) 02180 |            ALLOCATE(ANNUAL_CT_PURCHASED_CAP(CONTRACTS))|
01791 reaenrg.for                    (for) 02181 |            ALLOCATE(ANNUAL_CT_FIXED_CHARGE(CONTRACTS))|
01792 reaenrg.for                    (for) 02182 |            ALLOCATE(ANNUAL_CT_FUEL_CHARGE(CONTRACTS))|
01793 reaenrg.for                    (for) 02183 |            ALLOCATE(ANNUAL_CT_VARIABLE_CHARGE(CONTRACTS))|
01794 reaenrg.for                    (for) 02184 |            ALLOCATE(ANNUAL_CT_MAX_DISPATCHED_CAP(CONTRACTS))|
01795 reaenrg.for                    (for) 02185 |            ALLOCATE(ANNUAL_CT_MAX_PURCHASED_CAP(CONTRACTS))|
01796 reaenrg.for                    (for) 02209 |            ALLOCATE(ANNUAL_EL_DISPATCHED_ENRG(EL_UNITS))|
01797 reaenrg.for                    (for) 02210 |            ALLOCATE(ANNUAL_EL_DISPATCHED_CAP(EL_UNITS))|
01798 reaenrg.for                    (for) 02211 |            ALLOCATE(ANNUAL_EL_LOSSES(EL_UNITS))|
01799 reaenrg.for                    (for) 02212 |            ALLOCATE(ANNUAL_EL_PURCHASED_ENRG(EL_UNITS))|
01800 reaenrg.for                    (for) 02213 |            ALLOCATE(ANNUAL_EL_PURCHASED_CAP(EL_UNITS))|
01801 reaenrg.for                    (for) 02214 |            ALLOCATE(ANNUAL_EL_FIXED_CHARGE(EL_UNITS))|
01802 reaenrg.for                    (for) 02215 |            ALLOCATE(ANNUAL_EL_FUEL_CHARGE(EL_UNITS))|
01803 reaenrg.for                    (for) 02216 |            ALLOCATE(ANNUAL_EL_VARIABLE_CHARGE(EL_UNITS))|
01804 reaenrg.for                    (for) 02217 |            ALLOCATE(ANNUAL_EL_MAX_DISPATCHED_CAP(EL_UNITS))|
01805 reaenrg.for                    (for) 02218 |            ALLOCATE(ANNUAL_EL_MAX_PURCHASED_CAP(EL_UNITS))|
01806 servicac.f90                   (f90) 00725 |      ALLOCATE(SERVICE_ASSET_CLASS_POINTER(1024), &|
01807 servicac.f90                   (f90) 00738 |      ALLOCATE(SERVICE_NAME(NUM_SERVICE_TRANS), &|
01808 servicac.f90                   (f90) 00869 |      ALLOCATE(CAPACITY_CHARGE(SERVICE_TRANS), &|
01809 servicac.f90                   (f90) 01658 |            ALLOCATE(ANNUAL_ST_ENERGY(NUMBER_OF_SERVICES), &|
01810 servicac.f90                   (f90) 01684 |            ALLOCATE(MONTHLY_ST_ENERGY(0:12,NUMBER_OF_SERVICES), &|
01811 servicac.f90                   (f90) 01745 |            ALLOCATE(ANNUAL_ST_ENERGY(NUMBER_OF_SERVICES), &|
01812 servicac.f90                   (f90) 01771 |            ALLOCATE(MONTHLY_ST_ENERGY(0:12,NUMBER_OF_SERVICES), &|
01813 servicac.f90                   (f90) 02547 |         ALLOCATE(ST_ANN_CLASS_ATL_EXPENSE(0:12, &|
01814 servicac.f90                   (f90) 02569 |         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &|
01815 servicac.f90                   (f90) 02998 |               ALLOCATE(ANNUAL_COSTS(6,TOTAL_ANNUAL_RECORDS))|
01816 servicac.f90                   (f90) 03005 |               ALLOCATE(TRANS_NAME(NO_OF_TRANS), &|
01817 snszmds.f90                    (f90) 00728 |         ALLOCATE (TEMP_CHRONO_LOAD(TEMP_HOURS))|
01818 startovl.f90                   (f90) 00257 |      ALLOCATE (DATA_RECORD(MIN(HELP,OUT_REC_LENGTH-int(4,4))))|
01819 tf_objt.f90                    (f90) 00228 |               ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023), &|
01820 tf_objt.f90                    (f90) 00810 |            ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023), &|
01821 tf_objt.f90                    (f90) 02281 |             ALLOCATE( &|
01822 tf_objt.f90                    (f90) 02510 |            ALLOCATE(CUSTOMER_GROUP(MAX_TRANS_LOAD_TABLES)) ! INT2|
01823 tf_objt.f90                    (f90) 02511 |            ALLOCATE(CUSTOMER_GROUP_2(MAX_TRANS_LOAD_TABLES))|
01824 tf_objt.f90                    (f90) 02512 |            ALLOCATE( &|
01825 tf_objt.f90                    (f90) 02527 |            ALLOCATE(SCENARIO_INDEX(MAX_TRANS_LOAD_TABLES))|
01826 tf_objt.f90                    (f90) 02528 |            ALLOCATE(TRANSACTION_GROUP(MAX_TRANS_LOAD_TABLES)) ! INT2|
01827 tf_objt.f90                    (f90) 02529 |            ALLOCATE(REFERENCE_LOAD_NUMBER(MAX_TRANS_LOAD_TABLES)) ! INT2|
01828 tf_objt.f90                    (f90) 02530 |            ALLOCATE(MARKET_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01829 tf_objt.f90                    (f90) 02531 |            ALLOCATE(MONTHLY_ENERGY_PRICE_PATTERN( &|
01830 tf_objt.f90                    (f90) 02533 |            ALLOCATE(MARKET_DEMAND_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01831 tf_objt.f90                    (f90) 02534 |            ALLOCATE(MONTHLY_DEMAND_PRICE_PATTERN( &|
01832 tf_objt.f90                    (f90) 02536 |            ALLOCATE(MARKET_CUSTOMER_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01833 tf_objt.f90                    (f90) 02537 |            ALLOCATE(ASSET_CLASS_ID(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01834 tf_objt.f90                    (f90) 02538 |            ALLOCATE(ASSET_CLASS_REV_ALLOC_VECTOR( &|
01835 tf_objt.f90                    (f90) 02540 |            ALLOCATE(ANNUAL_ENERGY(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01836 tf_objt.f90                    (f90) 02541 |            ALLOCATE(ANNUAL_PEAK(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01837 tf_objt.f90                    (f90) 02542 |            ALLOCATE(ANNUAL_CUSTOMERS(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01838 tf_objt.f90                    (f90) 02543 |            ALLOCATE(ANNUAL_MULTIPLIER(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01839 tf_objt.f90                    (f90) 02544 |            ALLOCATE(MONTHLY_ENERGY(12,MAX_TRANS_LOAD_TABLES)) ! (12) REAL4|
01840 tf_objt.f90                    (f90) 02545 |            ALLOCATE(MONTHLY_PEAK(12,MAX_TRANS_LOAD_TABLES)) ! (12) REAL4|
01841 tf_objt.f90                    (f90) 02546 |            ALLOCATE(MONTHLY_CUSTOMERS(12,MAX_TRANS_LOAD_TABLES)) ! (12) REAL4|
01842 tf_objt.f90                    (f90) 02547 |            ALLOCATE(DIST_ENERGY_LOSS_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01843 tf_objt.f90                    (f90) 02548 |            ALLOCATE(TRANS_ENERGY_LOSS_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01844 tf_objt.f90                    (f90) 02549 |            ALLOCATE(PEAK_LOSS_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01845 tf_objt.f90                    (f90) 02550 |            ALLOCATE(PEAK_COIN_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01846 tf_objt.f90                    (f90) 02551 |            ALLOCATE(DISTRIBUTION_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4|
01847 tf_objt.f90                    (f90) 02552 |            ALLOCATE(TRANSMISSION_PRICE(MAX_TRANS_LOAD_TABLES)) !REAL4!|
01848 tf_objt.f90                    (f90) 02553 |            ALLOCATE(CUSTOMER_CLASS_NAME(MAX_TRANS_LOAD_TABLES)) ! CHAR*30|
01849 tf_objt.f90                    (f90) 02554 |            ALLOCATE(CALCULATION_MODE(MAX_TRANS_LOAD_TABLES)) ! CHAR*1|
01850 tf_objt.f90                    (f90) 02555 |            ALLOCATE(REFERENCE_LOAD_NAME(MAX_TRANS_LOAD_TABLES)) ! CHAR*5|
01851 tf_objt.f90                    (f90) 02556 |            ALLOCATE(TRANS_MONTHLY_ENERGY(12,MAX_TRANS_LOAD_TABLES))|
01852 tf_objt.f90                    (f90) 02557 |            ALLOCATE(WH_TRANS_MONTHLY_ENERGY(12, &|
01853 tf_objt.f90                    (f90) 02559 |            ALLOCATE(WH_TRANS_MONTHLY_CAPACITY(12, &|
01854 tf_objt.f90                    (f90) 02561 |            ALLOCATE(TRANS_MONTHLY_PEAK(12,MAX_TRANS_LOAD_TABLES))|
01855 tf_objt.f90                    (f90) 02562 |            ALLOCATE(TRANS_MONTHLY_CUSTOMERS(12,MAX_TRANS_LOAD_TABLES))|
01856 tf_objt.f90                    (f90) 02563 |            ALLOCATE(TABLE_ACTIVE(MAX_TRANS_LOAD_TABLES))|
01857 tf_objt.f90                    (f90) 02564 |            ALLOCATE(BASECASE_MARKET_AREA_ID(MAX_TRANS_LOAD_TABLES))|
01858 tf_objt.f90                    (f90) 02565 |            ALLOCATE(WD_INDEX(MAX_TRANS_LOAD_TABLES))|
01859 tf_objt.f90                    (f90) 02566 |            ALLOCATE(BASECASE_TRANS_AREA_ID(MAX_TRANS_LOAD_TABLES))|
01860 tf_objt.f90                    (f90) 02567 |            ALLOCATE(BASECASE_NERC_SUB_ID(MAX_TRANS_LOAD_TABLES))|
01861 tf_objt.f90                    (f90) 02568 |            ALLOCATE(MONTHLY_UNITS(MAX_TRANS_LOAD_TABLES))|
01862 tf_objt.f90                    (f90) 02569 |            ALLOCATE(PRICE_INDEX_ACTIVE(MAX_TRANS_LOAD_TABLES))|
01863 tf_objt.f90                    (f90) 02570 |            ALLOCATE(THREE_FACTOR_TRANSFORM(MAX_TRANS_LOAD_TABLES))|
01864 tf_objt.f90                    (f90) 02571 |            ALLOCATE(JURISDICTIONAL_CUSTOMER(MAX_TRANS_LOAD_TABLES))|
01865 tf_objt.f90                    (f90) 02572 |            ALLOCATE(FUEL_COST_RECOVERY_THROUGH_FAC( &|
01866 tf_objt.f90                    (f90) 02574 |            ALLOCATE(BASE_COST_OF_FAC_FUEL(MAX_TRANS_LOAD_TABLES))|
01867 tf_objt.f90                    (f90) 02575 |            ALLOCATE(MINIMUM_MARKET_PRICE(MAX_TRANS_LOAD_TABLES))|
01868 tf_objt.f90                    (f90) 02576 |            ALLOCATE(MAXIMUM_MARKET_PRICE(MAX_TRANS_LOAD_TABLES))|
01869 tf_objt.f90                    (f90) 02577 |            ALLOCATE(INDEXED_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES))|
01870 tf_objt.f90                    (f90) 02578 |            ALLOCATE(TG_COUNTER(MAX_TRANS_GROUPS))|
01871 tf_objt.f90                    (f90) 02579 |            ALLOCATE(LOAD_DISPATCH_POSITION(MAX_TRANS_LOAD_TABLES, &|
01872 tf_objt.f90                    (f90) 02581 |            ALLOCATE(LOAD_DISPATCH_INDEX(MAX_TRANS_LOAD_TABLES, &|
01873 tf_objt.f90                    (f90) 02584 |            ALLOCATE(TRANS_LOAD_GROUPS_INDEX(MAX_TRANS_GROUPS))|
01874 tf_objt.f90                    (f90) 02585 |            ALLOCATE(CUST_CLASS_GROUPS_INDEX(0:MAX_CLASS_GROUPS))|
01875 tf_objt.f90                    (f90) 02586 |            ALLOCATE(CUST2_CLASS_GROUPS_INDEX(0:MAX_CLASS2_GROUPS))|
01876 tf_objt.f90                    (f90) 02587 |            ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))|
01877 tf_objt.f90                    (f90) 02588 |            ALLOCATE(ASSET_2_TRANS_INDEX(0:MAX_ASSET_GROUPS, &|
01878 tf_objt.f90                    (f90) 02590 |            ALLOCATE(NUMBER_ASSET_2_TRANS(MAX_TRANS_GROUPS))|
01879 tf_objt.f90                    (f90) 02591 |            ALLOCATE(ASSET_TRANSACTION_CROSS_INDEX(0:MAX_ASSET_GROUPS, &|
01880 tf_objt.f90                    (f90) 02593 |            ALLOCATE(NUMBER_TRANS_PER_AC_TG(0:MAX_ASSET_GROUPS, &|
01881 tf_objt.f90                    (f90) 02595 |            ALLOCATE(TRANS_WITHIN_AC_TG(0:MAX_ASSET_GROUPS, &|
01882 tf_objt.f90                    (f90) 02597 |            ALLOCATE(FIRST_AC_TG(0:MAX_ASSET_GROUPS))|
01883 tf_objt.f90                    (f90) 02598 |            ALLOCATE(FIRST_TABLE_TG(0:MAX_TRANS_GROUPS))|
01884 tf_objt.f90                    (f90) 02599 |            ALLOCATE(TRANS_LOAD_2_TRANS_GROUPS(MAX_TRANS_GROUPS))|
01885 tf_objt.f90                    (f90) 02600 |            ALLOCATE(TRANS_LOAD_GROUP_2_TG(MAX_TRANS_GROUPS))|
01886 tf_objt.f90                    (f90) 02601 |            ALLOCATE(CUST_CLASS_GROUP_2_CG(MAX_CLASS_GROUPS+1))|
01887 tf_objt.f90                    (f90) 02602 |            ALLOCATE(CUST2_CLASS_GROUP_2_CG(MAX_CLASS_GROUPS+1))|
01888 tf_objt.f90                    (f90) 02603 |            ALLOCATE(ASSET_CLASS_GROUP_2_AC(0:MAX_ASSET_GROUPS+1))|
01889 tf_objt.f90                    (f90) 02604 |            ALLOCATE(LAST_TABLE_FOR_TG(MAX_TRANS_GROUPS))|
01890 tf_objt.f90                    (f90) 02605 |            ALLOCATE(LAST_TABLE_FOR_CG(MAX_CLASS_GROUPS+1))|
01891 tf_objt.f90                    (f90) 02606 |            ALLOCATE(LAST_TABLE_FOR_CG2(MAX_CLASS_GROUPS+1))|
01892 tf_objt.f90                    (f90) 02607 |            ALLOCATE(TG_CG_DATABASE(0:12,0:MAX_TRANS_GROUPS, &|
01893 tf_objt.f90                    (f90) 02609 |            ALLOCATE(TG_CG2_DATABASE(0:12,0:MAX_TRANS_GROUPS, &|
01894 tf_objt.f90                    (f90) 02611 |            ALLOCATE(TABLE_DAY_SHIFT(MAX_TRANS_LOAD_TABLES))|
01895 tf_objt.f90                    (f90) 02612 |            ALLOCATE(TF_PLANNING_PEAK(0:MAX_TRANS_GROUPS,12))|
01896 tf_objt.f90                    (f90) 02613 |            ALLOCATE(PA_PLANNING_PEAK(0:NUMBER_OF_ACTIVE_GROUPS,12))|
01897 tf_objt.f90                    (f90) 02614 |            ALLOCATE(CM_PLANNING_PEAK(0:NUMBER_OF_ACTIVE_GROUPS,12))|
01898 tf_objt.f90                    (f90) 02615 |            ALLOCATE(GLOBAL_PA_PEAK(0:NUMBER_OF_ACTIVE_GROUPS))|
01899 tf_objt.f90                    (f90) 02616 |            ALLOCATE(GLOBAL_CM_PEAK(0:NUMBER_OF_ACTIVE_GROUPS))|
01900 tf_objt.f90                    (f90) 02617 |            ALLOCATE(GLOBAL_PA_PEAK_MONTH(0:NUMBER_OF_ACTIVE_GROUPS))|
01901 tf_objt.f90                    (f90) 02618 |            ALLOCATE(GLOBAL_CM_PEAK_MONTH(0:NUMBER_OF_ACTIVE_GROUPS))|
01902 tf_objt.f90                    (f90) 02619 |            ALLOCATE(TG_2_PLANNING_AREA(MAX_TRANS_GROUPS_FROM_TG))|
01903 tf_objt.f90                    (f90) 02620 |            ALLOCATE(TG_2_CAPACITY_MARKET(MAX_TRANS_GROUPS_FROM_TG))|
01904 tf_objt.f90                    (f90) 02621 |            ALLOCATE(REV_CLASS_INDEX(0:MAX_TRANS_LOAD_TABLES,4))|
01905 tf_objt.f90                    (f90) 02622 |            ALLOCATE(ENERGY_LOSS_MULT(MAX_TRANS_LOAD_TABLES))|
01906 tf_objt.f90                    (f90) 02623 |            ALLOCATE(REF_LEAP_YEAR_DAY_SHIFT(MAX_TRANS_LOAD_TABLES))|
01907 tf_objt.f90                    (f90) 02624 |            ALLOCATE(DEMAND_PRICING_METHOD(MAX_TRANS_LOAD_TABLES))|
01908 tf_objt.f90                    (f90) 02625 |           ALLOCATE(MONTHLY_TABLE_PEAK_SALES(MAX_TRANS_LOAD_TABLES,12))|
01909 tf_objt.f90                    (f90) 02626 |            ALLOCATE(INTRA_COMPANY_TRANSACTION(MAX_TRANS_LOAD_TABLES))|
01910 tf_objt.f90                    (f90) 02627 |            ALLOCATE(INTRA_ASSET_CLASS_ID(MAX_TRANS_LOAD_TABLES))|
01911 tf_objt.f90                    (f90) 02628 |            ALLOCATE(INTRA_ASSET_CLASS_ALLOCATION( &|
01912 tf_objt.f90                    (f90) 02630 |            ALLOCATE(INTRA_ACCOUNT_CLASSIFICATION( &|
01913 tf_objt.f90                    (f90) 02632 |            ALLOCATE(INTRA_EXPENSE_COLLECTION(MAX_TRANS_LOAD_TABLES))|
01914 tf_objt.f90                    (f90) 02695 |         ALLOCATE(MONTHLY_INTERRUPTIBLE_REVENUE( &|
01915 tf_objt.f90                    (f90) 02735 |            ALLOCATE(LAST_THIS_YR_ENERGY(2,12,0:MAX_TRANS_LOAD_TABLES)) ! 1=LAST_YEAR, 2=THIS YEAR|
01916 tf_objt.f90                    (f90) 02736 |            ALLOCATE(LAST_THIS_YR_PEAK(2,12,0:MAX_TRANS_LOAD_TABLES))|
01917 tf_objt.f90                    (f90) 03485 |         ALLOCATE(MONTHLY_AC_COST_AT_MARKET( &|
01918 tf_objt.f90                    (f90) 03488 |         ALLOCATE(MONTHLY_AC_CONTRACT_REVENUE( &|
01919 tf_objt.f90                    (f90) 03492 |         ALLOCATE(MONTHLY_AC_CONTRACT_EXPENSE( &|
01920 tf_objt.f90                    (f90) 03545 |         ALLOCATE(DAY_OF_WEEK(31,0:MAX_TRANS_LOAD_TABLES))|
01921 tf_objt.f90                    (f90) 03558 |            ALLOCATE( &|
01922 tf_objt.f90                    (f90) 03560 |            ALLOCATE( &|
01923 tf_objt.f90                    (f90) 03562 |            ALLOCATE( &|
01924 tf_objt.f90                    (f90) 03564 |            ALLOCATE( &|
01925 tf_objt.f90                    (f90) 03566 |            ALLOCATE( &|
01926 tf_objt.f90                    (f90) 03568 |            ALLOCATE( &|
01927 tf_objt.f90                    (f90) 03570 |            ALLOCATE( &|
01928 tf_objt.f90                    (f90) 03572 |            ALLOCATE( &|
01929 tf_objt.f90                    (f90) 03621 |         ALLOCATE (WH_TRANS_ALLOC(0:MAX_TRANS_LOAD_TABLES, &|
01930 tf_objt.f90                    (f90) 03623 |         ALLOCATE(TRANS_HOURLY_LOAD( &|
01931 tf_objt.f90                    (f90) 03625 |         ALLOCATE(WH_LOADS_PER_HOUR( &|
01932 tf_objt.f90                    (f90) 03627 |         ALLOCATE(HYDRO_HOURLY_LOAD( & !  NEED TO COUNT HYDRO GROUPS|
01933 tf_objt.f90                    (f90) 03629 |         ALLOCATE(TABLE_HOURLY_LOAD( &|
01934 tf_objt.f90                    (f90) 03631 |         ALLOCATE(MONTHLY_TRANS_ENERGY(0:MAX_TRANS_LOAD_GROUPS))|
01935 tf_objt.f90                    (f90) 03632 |         ALLOCATE(MONTHLY_HYDRO_ENERGY(0:NUMBER_OF_HYDRO_GROUPS))|
01936 tf_objt.f90                    (f90) 03633 |         ALLOCATE(MONTHLY_TRANS_PEAK(0:MAX_TRANS_LOAD_GROUPS))|
01937 tf_objt.f90                    (f90) 03634 |         ALLOCATE(MONTHLY_TRANS_BASE(0:MAX_TRANS_LOAD_GROUPS))|
01938 tf_objt.f90                    (f90) 03636 |         ALLOCATE(MONTHLY_HYDRO_PEAK(0:NUMBER_OF_HYDRO_GROUPS))|
01939 tf_objt.f90                    (f90) 03637 |         ALLOCATE(MONTHLY_HYDRO_BASE(0:NUMBER_OF_HYDRO_GROUPS))|
01940 tf_objt.f90                    (f90) 03639 |         ALLOCATE(MONTHLY_TABLE_ENERGY(0:MAX_TRANS_LOAD_TABLES))|
01941 tf_objt.f90                    (f90) 03640 |         ALLOCATE(MONTHLY_TABLE_SALES_ENERGY( &|
01942 tf_objt.f90                    (f90) 03642 |         ALLOCATE(MONTHLY_TABLE_PEAK(0:MAX_TRANS_LOAD_TABLES))|
01943 tf_objt.f90                    (f90) 03643 |         ALLOCATE(TABLE_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES))|
01944 tf_objt.f90                    (f90) 03644 |         ALLOCATE(TABLE_ENERGY_REVENUE(0:MAX_TRANS_LOAD_TABLES))|
01945 tf_objt.f90                    (f90) 03645 |         ALLOCATE(TABLE_DEMAND_REVENUE(0:MAX_TRANS_LOAD_TABLES))|
01946 tf_objt.f90                    (f90) 03646 |         ALLOCATE(TABLE_CUSTOMER_REVENUE(0:MAX_TRANS_LOAD_TABLES))|
01947 tf_objt.f90                    (f90) 03648 |         ALLOCATE(TRANS_ENERGY_REVENUE(0:MAX_TRANS_LOAD_GROUPS))|
01948 tf_objt.f90                    (f90) 03649 |         ALLOCATE(TRANS_DEMAND_REVENUE(0:MAX_TRANS_LOAD_GROUPS))|
01949 tf_objt.f90                    (f90) 03650 |         ALLOCATE(TRANS_CUSTOMER_REVENUE(0:MAX_TRANS_LOAD_GROUPS))|
01950 tf_objt.f90                    (f90) 03652 |         ALLOCATE(CLASS_ENERGY_REVENUE(0:MAX_CUST_CLASS_GROUPS))|
01951 tf_objt.f90                    (f90) 03653 |         ALLOCATE(TRANS_INDEXED_REVENUE(0:MAX_CUST_CLASS_GROUPS))|
01952 tf_objt.f90                    (f90) 03655 |         ALLOCATE(CLASS_PEAK_REVENUE(0:MAX_CUST_CLASS_GROUPS))|
01953 tf_objt.f90                    (f90) 03656 |         ALLOCATE(CLASS_CUSTOMER_REVENUE(0:MAX_CUST_CLASS_GROUPS))|
01954 tf_objt.f90                    (f90) 03657 |         ALLOCATE(CAP_CUSTOMER_EXPENSE(0:MAX_CUST_CLASS_GROUPS))|
01955 tf_objt.f90                    (f90) 03658 |         ALLOCATE(TF_TG_CAP_MARKET_MW(0:MAX_TRANS_GROUPS))|
01956 tf_objt.f90                    (f90) 03659 |         ALLOCATE(TF_TG_CAP_MARKET_COST(0:MAX_TRANS_GROUPS))|
01957 tf_objt.f90                    (f90) 03660 |         ALLOCATE(MONTHLY_CLASS_ENERGY(0:MAX_CUST_CLASS_GROUPS))|
01958 tf_objt.f90                    (f90) 03661 |         ALLOCATE(MONTHLY_CLASS_PEAK(0:MAX_CUST_CLASS_GROUPS))|
01959 tf_objt.f90                    (f90) 03662 |         ALLOCATE(MONTHLY_CLASS_CUSTOMERS(0:MAX_CUST_CLASS_GROUPS))|
01960 tf_objt.f90                    (f90) 03663 |         ALLOCATE(LOCAL_CUSTOMER_NAME(0:MAX_CUST_CLASS_GROUPS))|
01961 tf_objt.f90                    (f90) 03664 |         ALLOCATE(ASSET_CLASS_HOURLY_LOAD(MAX_HOURS_IN_MONTH, &|
01962 tf_objt.f90                    (f90) 05001 |         ALLOCATE(ANN_LOAD_DISPATCH_COST_BY_UNIT(NUNITS+2, &|
01963 tf_objt.f90                    (f90) 05006 |         ALLOCATE(ANN_LOAD_DISPATCH_REV_BY_UNIT(NUNITS+2, &|
01964 tf_objt.f90                    (f90) 05011 |         ALLOCATE(ANN_LOAD_DISPATCH_BY_UNIT(NUNITS+2, &|
01965 tf_objt.f90                    (f90) 05174 |            ALLOCATE(HOURLY_CUST_MARGIN_DB(4,HOURLY_CUST_MARGIN_VAR))|
01966 tf_objt.f90                    (f90) 05186 |            ALLOCATE(LOAD_DISPATCH_BY_BLOCK( &|
01967 tf_objt.f90                    (f90) 05188 |            ALLOCATE(LOAD_DISPATCH_COST( &|
01968 tf_objt.f90                    (f90) 05190 |            ALLOCATE(LOCAL_MUST_RUN_OPTIONS(R_TOTAL_DISPATCH_BLOCKS))|
01969 tf_objt.f90                    (f90) 05191 |            ALLOCATE(LOAD_DISPATCH_COST_BY_UNIT( &|
01970 tf_objt.f90                    (f90) 05193 |            ALLOCATE(LOAD_DISPATCH_REV( &|
01971 tf_objt.f90                    (f90) 05195 |           ALLOCATE(LOAD_DISPATCH_REV_BY_UNIT(NUNITS+2,LOAD_BLOCK_VAR))|
01972 tf_objt.f90                    (f90) 05196 |            ALLOCATE(LOAD_DISPATCH_BY_UNIT(NUNITS+2,LOAD_BLOCK_VAR))|
01973 tf_objt.f90                    (f90) 05597 |         ALLOCATE(CUM_BLOCK_GENERATION(LAST_BLOCK,3))|
01974 tf_objt.f90                    (f90) 07119 |         ALLOCATE(TEMP_CHRONO_LOAD(SAVE_HOURS))|
01975 tf_objt.f90                    (f90) 07162 |         ALLOCATE(TEMP_CHRONO_LOAD(SAVE_HOURS))|
01976 tf_objt.f90                    (f90) 08506 |         ALLOCATE(TRANS_GROUP_INDEX(0:MAX_TRANS_GROUP_INDEX))|
01977 tf_objt.f90                    (f90) 08507 |         ALLOCATE(TG_SCENARIO_VARIABLE_INDEX(0:MAX_TRANS_GROUP_INDEX))|
01978 tf_objt.f90                    (f90) 08508 |         ALLOCATE(HYDRO_GROUP_2_TG(0:MAX_TRANS_GROUP_INDEX))|
01979 tf_objt.f90                    (f90) 08510 |         ALLOCATE(TRANS_GROUP_POSITION(0:MAX_TRANS_GROUP_INDEX))|
01980 tf_objt.f90                    (f90) 08511 |         ALLOCATE(REPORT_THIS_GROUP(0:MAX_TRANS_GROUP_INDEX))|
01981 tf_objt.f90                    (f90) 08512 |         ALLOCATE(TG_2_HYDRO_GROUP(0:MAX_TRANS_GROUP_INDEX))|
01982 tf_objt.f90                    (f90) 08513 |         ALLOCATE(TG_2_PLANNING_AREA(0:MAX_TRANS_GROUP_INDEX))|
01983 tf_objt.f90                    (f90) 08514 |         ALLOCATE(TG_2_CAPACITY_MARKET(0:MAX_TRANS_GROUP_INDEX))|
01984 tf_objt.f90                    (f90) 08515 |         ALLOCATE(HYDRO_AGGREGATION_POSITION(0:MAX_TRANS_GROUP_INDEX))|
01985 tf_objt.f90                    (f90) 08516 |         ALLOCATE(HYDRO_AGGREGATION_INDEX(0:MAX_TRANS_GROUP_INDEX))|
01986 tf_objt.f90                    (f90) 08517 |         ALLOCATE(PLANNING_AREA_POSITION(0:MAX_TRANS_GROUP_INDEX))|
01987 tf_objt.f90                    (f90) 08518 |         ALLOCATE(CAPACITY_MARKET_POSITION(0:MAX_TRANS_GROUP_INDEX))|
01988 tf_objt.f90                    (f90) 08519 |         ALLOCATE(PLANNING_AREA_INDEX(0:MAX_TRANS_GROUP_INDEX))|
01989 tf_objt.f90                    (f90) 08520 |         ALLOCATE(CAPACITY_MARKET_INDEX(0:MAX_TRANS_GROUP_INDEX))|
01990 tf_objt.f90                    (f90) 08521 |         ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))|
01991 tf_objt.f90                    (f90) 08522 |         ALLOCATE(ASSET_CLASS_2_TG(0:MAX_ASSET_GROUPS))|
01992 tf_objt.f90                    (f90) 08523 |         ALLOCATE(ASSET_CLASS_GROUP_2_AC(0:MAX_TRANS_GROUP_INDEX))|
01993 tf_objt.f90                    (f90) 08598 |         ALLOCATE(GROUP_NAME(0:TRANS_GROUPS_RECORDS))|
01994 tf_objt.f90                    (f90) 08600 |         ALLOCATE(GROUP_ACTIVE(TRANS_GROUPS_RECORDS))|
01995 tf_objt.f90                    (f90) 08601 |         ALLOCATE(TRANSACTION_GROUP(TRANS_GROUPS_RECORDS))|
01996 tf_objt.f90                    (f90) 08602 |         ALLOCATE(TRANS_GROUP_FULL_NAME(TRANS_GROUPS_RECORDS))|
01997 tf_objt.f90                    (f90) 08604 |         ALLOCATE(TG_BASECASE_MARKET_AREA_ID(TRANS_GROUPS_RECORDS))|
01998 tf_objt.f90                    (f90) 08605 |         ALLOCATE(BASECASE_MARKET_AREA_ID(TRANS_GROUPS_RECORDS))|
01999 tf_objt.f90                    (f90) 08606 |         ALLOCATE(BASE_CASE_TRANS_AREA_ID(TRANS_GROUPS_RECORDS))|
02000 tf_objt.f90                    (f90) 08607 |         ALLOCATE(BASECASE_SUBREGION_ID(TRANS_GROUPS_RECORDS))|
02001 tf_objt.f90                    (f90) 08608 |         ALLOCATE(SPINNING_UNITS(TRANS_GROUPS_RECORDS))|
02002 tf_objt.f90                    (f90) 08609 |         ALLOCATE(OFF_PEAK_SPINNING_UNITS(TRANS_GROUPS_RECORDS))|
02003 tf_objt.f90                    (f90) 08610 |         ALLOCATE(SPINNING_RESERVE(TRANS_GROUPS_RECORDS))|
02004 tf_objt.f90                    (f90) 08611 |         ALLOCATE(OFF_PEAK_SPINNING_RESERVE(TRANS_GROUPS_RECORDS))|
02005 tf_objt.f90                    (f90) 08612 |         ALLOCATE(MAX_HOURLY_RAMP_UP(TRANS_GROUPS_RECORDS))|
02006 tf_objt.f90                    (f90) 08613 |         ALLOCATE(MAX_HOURLY_RAMP_DOWN(TRANS_GROUPS_RECORDS))|
02007 tf_objt.f90                    (f90) 08614 |         ALLOCATE(FIRST_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS))|
02008 tf_objt.f90                    (f90) 08615 |         ALLOCATE(FIRST_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS))|
02009 tf_objt.f90                    (f90) 08616 |         ALLOCATE(SECOND_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS))|
02010 tf_objt.f90                    (f90) 08617 |         ALLOCATE(SECOND_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS))|
02011 tf_objt.f90                    (f90) 08618 |         ALLOCATE(THIRD_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS))|
02012 tf_objt.f90                    (f90) 08619 |         ALLOCATE(THIRD_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS))|
02013 tf_objt.f90                    (f90) 08620 |         ALLOCATE(ADDITIONAL_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS,7))|
02014 tf_objt.f90                    (f90) 08621 |        ALLOCATE(ADDITIONAL_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS,7))|
02015 tf_objt.f90                    (f90) 08622 |         ALLOCATE(RTO_GROUP(TRANS_GROUPS_RECORDS))|
02016 tf_objt.f90                    (f90) 08623 |         ALLOCATE(NOX_YEAR(TRANS_GROUPS_RECORDS))|
02017 tf_objt.f90                    (f90) 08624 |         ALLOCATE(END_NOX_YEAR(TRANS_GROUPS_RECORDS))|
02018 tf_objt.f90                    (f90) 08625 |         ALLOCATE(ST_LHS_FOR_PRICES(TRANS_GROUPS_RECORDS))|
02019 tf_objt.f90                    (f90) 08626 |         ALLOCATE(MRX_VOLATILITY_MULT(TRANS_GROUPS_RECORDS))|
02020 tf_objt.f90                    (f90) 08627 |         ALLOCATE(NIGHT_SCARCITY_MULT(TRANS_GROUPS_RECORDS))|
02021 tf_objt.f90                    (f90) 08628 |         ALLOCATE(WEEKEND_SCARCITY_MULT(TRANS_GROUPS_RECORDS))|
02022 tf_objt.f90                    (f90) 08629 |         ALLOCATE(PRICE_CAP(TRANS_GROUPS_RECORDS))|
02023 tf_objt.f90                    (f90) 08630 |         ALLOCATE(PRICE_MINIMUM(TRANS_GROUPS_RECORDS))|
02024 tf_objt.f90                    (f90) 08631 |         ALLOCATE(REGIONAL_CAPACITY_MARKET(TRANS_GROUPS_RECORDS))|
02025 tf_objt.f90                    (f90) 08632 |         ALLOCATE(MAX_HOURLY_TG_IMPORT(TRANS_GROUPS_RECORDS))|
02026 tf_objt.f90                    (f90) 08633 |         ALLOCATE(MAX_HOURLY_TG_EXPORT(TRANS_GROUPS_RECORDS))|
02027 tf_objt.f90                    (f90) 08634 |         ALLOCATE(HYDRO_LOAD_AGGREGATION(TRANS_GROUPS_RECORDS))|
02028 tf_objt.f90                    (f90) 08635 |         ALLOCATE(REGIONAL_PLANNING_AREA(TRANS_GROUPS_RECORDS))|
02029 tf_objt.f90                    (f90) 08636 |         ALLOCATE(REPORT_CL_CAPACITY(TRANS_GROUPS_RECORDS))|
02030 tf_objt.f90                    (f90) 08637 |         ALLOCATE(ASSET_CLASS_ID(TRANS_GROUPS_RECORDS))|
02031 tf_objt.f90                    (f90) 08638 |         ALLOCATE(ASSET_CLASS_REV_ALLOC_VECTOR(TRANS_GROUPS_RECORDS))|
02032 tf_objt.f90                    (f90) 08639 |         ALLOCATE(TIME_ZONE(TRANS_GROUPS_RECORDS))|
02033 tf_objt.f90                    (f90) 08640 |         ALLOCATE(CAPACITY_ADDER(TRANS_GROUPS_RECORDS))|
02034 tf_objt.f90                    (f90) 08641 |         ALLOCATE(NOX_SEASON(TRANS_GROUPS_RECORDS))|
02035 tf_objt.f90                    (f90) 08642 |         ALLOCATE(PURCHASE_POWER_ASSIGN(TRANS_GROUPS_RECORDS))|
02036 tf_objt.f90                    (f90) 08643 |         ALLOCATE(CREATE_HOURLY_PRICE(TRANS_GROUPS_RECORDS))|
02037 tf_objt.f90                    (f90) 08644 |         ALLOCATE(CREATE_HOURLY_PRICE_INDEX(TRANS_GROUPS_RECORDS))|
02038 tf_objt.f90                    (f90) 08645 |         ALLOCATE(CREATE_HOURLY_PRICE_POS(TRANS_GROUPS_RECORDS))|
02039 tf_objt.f90                    (f90) 08646 |         ALLOCATE(HOURLY_PRICE_NAME(TRANS_GROUPS_RECORDS))|
02040 tf_objt.f90                    (f90) 08647 |         ALLOCATE(PURCHASE_ASSET_CLASS_ID(TRANS_GROUPS_RECORDS))|
02041 tf_objt.f90                    (f90) 08648 |         ALLOCATE(PURCHASE_ASSET_ALLOC_VECTOR(TRANS_GROUPS_RECORDS))|
02042 tf_objt.f90                    (f90) 08657 |         ALLOCATE(PRICE_ESCALATION_RATE(TRANS_GROUPS_RECORDS), &|
02043 tf_objt.f90                    (f90) 10503 |         ALLOCATE(DEMAND(R_MAX_TRANS_LOAD_GROUPS))|
02044 tf_objt.f90                    (f90) 10504 |         ALLOCATE(DX(R_MAX_TRANS_LOAD_GROUPS))|
02045 tf_objt.f90                    (f90) 10505 |         ALLOCATE(PEAK(R_MAX_TRANS_LOAD_GROUPS))|
02046 tf_objt.f90                    (f90) 10506 |         ALLOCATE(LODDUR(LOAD_POINTS,R_MAX_TRANS_LOAD_GROUPS))|
02047 tf_objt.f90                    (f90) 10507 |         ALLOCATE(LPROB(LOAD_POINTS,R_MAX_TRANS_LOAD_GROUPS))|
02048 tf_objt.f90                    (f90) 10847 |         ALLOCATE(DEMAND(R_MAX_HYDRO_LOAD_GROUPS))|
02049 tf_objt.f90                    (f90) 10848 |         ALLOCATE(DX(R_MAX_HYDRO_LOAD_GROUPS))|
02050 tf_objt.f90                    (f90) 10849 |         ALLOCATE(PEAK(R_MAX_HYDRO_LOAD_GROUPS))|
02051 tf_objt.f90                    (f90) 10850 |         ALLOCATE(LODDUR(1000,R_MAX_HYDRO_LOAD_GROUPS))|
02052 tf_objt.f90                    (f90) 10851 |         ALLOCATE(LPROB(1000,R_MAX_HYDRO_LOAD_GROUPS))|
02053 tf_objt.f90                    (f90) 11054 |      ALLOCATE(HOURLY_HYDRO(LOAD_HOURS_IN_PERIOD), &|
02054 tf_objt.f90                    (f90) 11080 |      ALLOCATE(SORT_POS(LOAD_HOURS_IN_PERIOD))|
02055 tf_objt.f90                    (f90) 11356 |      ALLOCATE(SORT_POS(LOAD_HOURS_IN_PERIOD))|
02056 tf_objt.f90                    (f90) 11624 |         ALLOCATE(TRANS_LOAD_AFTER_EL( &|
02057 tf_objt.f90                    (f90) 11626 |         ALLOCATE(TRANS_HOURLY_HYDRO( &|
02058 tf_objt.f90                    (f90) 11628 |         ALLOCATE(MONTHLY_TRANS_LOAD_MWH(0:MAX_TRANS_LOAD_GROUPS))|
02059 tf_objt.f90                    (f90) 11629 |        ALLOCATE(MONTHLY_TRANS_LOAD_HYDRO_MWH(0:MAX_TRANS_LOAD_GROUPS))|
02060 tf_objt.f90                    (f90) 11630 |         ALLOCATE(MONTHLY_TRANS_LOAD_HYDRO_MW(0:MAX_TRANS_LOAD_GROUPS))|
02061 tf_objt.f90                    (f90) 11631 |         ALLOCATE(MONTHLY_TRANS_PEAK(0:MAX_TRANS_LOAD_GROUPS))|
02062 tf_objt.f90                    (f90) 11632 |         ALLOCATE(MONTHLY_TRANS_BASE(0:MAX_TRANS_LOAD_GROUPS))|
02063 tf_objt.f90                    (f90) 12332 |            ALLOCATE(SCENARIO_VARIABLE_ACTIVE(NUM_SCEN_VAR))|
02064 tf_objt.f90                    (f90) 12333 |            ALLOCATE(SCENARIO_VARIABLE_NAME(NUM_SCEN_VAR))|
02065 tf_objt.f90                    (f90) 12344 |            ALLOCATE(TIME_FRAME(NUM_SCEN_VAR,AVAIL_DATA_YEARS))|
02066 tf_objt.f90                    (f90) 12345 |            ALLOCATE(HOURLY_REFERENCE_NAME(NUM_SCEN_VAR, &|
02067 tf_objt.f90                    (f90) 12347 |            ALLOCATE(HOURLY_REFERENCE_NUMBER(NUM_SCEN_VAR, &|
02068 tf_objt.f90                    (f90) 12349 |            ALLOCATE(SCEN_MAKER_VARIABLE(NUM_SCEN_VAR,LOCAL_MONTHS, &|
02069 tf_objt2.f90                   (f90) 00735 |         ALLOCATE(unit_names(MAX_OUTAGES), &|
02070 tf_objt2.f90                   (f90) 00900 |         ALLOCATE( BEGIN_MONTH_DAY(MAX_EVENTS_IN_MONTH), &|
02071 tf_objt2.f90                   (f90) 01055 |         ALLOCATE( MONTH_EVENT_COUNTER(MAX_TRANS_GROUPS) )|
02072 tf_objt2.f90                   (f90) 01065 |         ALLOCATE(EVENTS_RECORD_INDEX( &|
02073 tf_objt2.f90                   (f90) 01089 |         ALLOCATE(BEGIN_IN_HOUR_ALAN(MAX_FO_PER_MONTH+1), &|
02074 tf_objt2.f90                   (f90) 01449 |            ALLOCATE(CLM_MAINT_INDEX(TOTAL_INDEXED_OUTAGES))|
02075 tf_objt2.f90                   (f90) 03257 |            ALLOCATE( &|
02076 tf_objt2.f90                   (f90) 03277 |            ALLOCATE( &|
02077 tf_objt2.f90                   (f90) 03430 |         ALLOCATE(MAX_BLOCKS_PER_TRANS(0:UPPER_TRANS_GROUP), &|
02078 tf_objt2.f90                   (f90) 03473 |         ALLOCATE(PRODUCT_GEN_BY_S_U(0:MAX_START_UP_UNITS_BY_TG,3, &|
02079 tf_objt2.f90                   (f90) 03477 |         ALLOCATE(PRODUCT_FUEL_BY_S_U(0:MAX_START_UP_UNITS_BY_TG,3, &|
02080 tf_objt2.f90                   (f90) 03480 |         ALLOCATE ( &|
02081 tf_objt2.f90                   (f90) 03500 |         ALLOCATE ( &|
02082 tf_objt2.f90                   (f90) 03506 |         ALLOCATE ( &|
02083 tf_objt2.f90                   (f90) 03515 |         ALLOCATE ( &|
02084 tf_objt2.f90                   (f90) 03520 |         ALLOCATE ( &|
02085 tf_objt2.f90                   (f90) 03525 |         ALLOCATE (TRANC_DATABASE(1,1,1,1))|
02086 tf_objt2.f90                   (f90) 03754 |       ALLOCATE (GENERATION_BY_SEGMENT(MAX_OUTAGE_BLOCKS,800))|
02087 tf_objt2.f90                   (f90) 03755 |       allocate(DAILY_CUM_DISPATCH_MW(MAX_OUTAGE_BLOCKS,24))|
02088 tf_objt2.f90                   (f90) 03757 |      allocate(DEPTH_HOURLY_DISPATCH_MW(MAX_OUTAGE_BLOCKS))|
02089 tf_objt2.f90                   (f90) 03758 |      allocate(DAILY_CUM_DISPATCH_COST(MAX_OUTAGE_BLOCKS,24))|
02090 tf_objt2.f90                   (f90) 03759 |      allocate(DAILY_GEN_BY_S_U(0:NO_START_UP_UNITS,24))|
02091 tf_objt2.f90                   (f90) 03760 |      allocate(DAILY_FUEL_BY_S_U( &|
02092 tf_objt2.f90                   (f90) 03763 |      allocate(DAILY_SYSTEM_COST_AND_REV(0:2,24))|
02093 tf_objt2.f90                   (f90) 03764 |      allocate(DAILY_COST_BY_S_U(0:NO_START_UP_UNITS,24))|
02094 tf_objt2.f90                   (f90) 03765 |      allocate(DAILY_SPIN_BY_S_U(0:NO_START_UP_UNITS,24))|
02095 tf_objt2.f90                   (f90) 03766 |      allocate(MONTH_MUST_BY_BLOCK(MAX_OUTAGE_BLOCKS,800))|
02096 tf_objt2.f90                   (f90) 03767 |      allocate(DAILY_MUST_RUN_CAPACITY(24))|
02097 tf_objt2.f90                   (f90) 03768 |      allocate(DAILY_EMERGENCY_CAPACITY(24))|
02098 tf_objt2.f90                   (f90) 03769 |      allocate(DAILY_HARD_WIRED_BY_BLOCK(24,MAX_OUTAGE_BLOCKS))|
02099 tf_objt2.f90                   (f90) 03770 |      allocate(INCREMENTAL_FUEL_COST(MAX_OUTAGE_BLOCKS))|
02100 tf_objt2.f90                   (f90) 03771 |      allocate(OPTION_NAME(0:NO_START_UP_UNITS))|
02101 tf_objt2.f90                   (f90) 03772 |      allocate(CONTRIBUTES_TO_SPIN(NO_START_UP_UNITS))|
02102 tf_objt2.f90                   (f90) 03773 |      allocate(MONTH_UNIT_STARTS(NO_START_UP_UNITS))|
02103 tf_objt2.f90                   (f90) 03774 |      allocate(MONTH_UNIT_DOWNS(NO_START_UP_UNITS))|
02104 tf_objt2.f90                   (f90) 03775 |      allocate(IN_MIN_DOWN_TIME_STATE(NO_START_UP_UNITS,800))|
02105 tf_objt2.f90                   (f90) 03776 |      allocate(IN_MIN_UP_TIME_STATE(NO_START_UP_UNITS,800))|
02106 tf_objt2.f90                   (f90) 03777 |      allocate(START_UP_COSTS(NO_START_UP_UNITS))|
02107 tf_objt2.f90                   (f90) 03778 |      allocate(EMERGENCY_CAPACITY(NO_START_UP_UNITS))|
02108 tf_objt2.f90                   (f90) 03779 |      allocate(MIN_SPIN_CAP(NO_START_UP_UNITS))|
02109 tf_objt2.f90                   (f90) 03780 |      allocate(MAX_SPIN_CAP(NO_START_UP_UNITS))|
02110 tf_objt2.f90                   (f90) 03781 |      allocate(HOURLY_EMERGENCY_CAPACITY(NO_START_UP_UNITS))|
02111 tf_objt2.f90                   (f90) 03782 |      allocate(MAX_HOURLY_RAMP_UP(0:MAX_OUTAGE_BLOCKS))|
02112 tf_objt2.f90                   (f90) 03783 |      allocate(MAX_HOURLY_RAMP_DOWN(0:MAX_OUTAGE_BLOCKS))|
02113 tf_objt2.f90                   (f90) 03784 |      allocate(EMERGENCY_HEATRATE(NO_START_UP_UNITS))|
02114 tf_objt2.f90                   (f90) 03785 |      allocate(MONTH_UNIT_START_COST(NO_START_UP_UNITS))|
02115 tf_objt2.f90                   (f90) 03786 |      allocate(MONTH_OPERATING_HOURS(NO_START_UP_UNITS))|
02116 tf_objt2.f90                   (f90) 03787 |      allocate(MUST_RUN_BLOCK(MAX_OUTAGE_BLOCKS))|
02117 tf_objt2.f90                   (f90) 03788 |      allocate(UNIT_IS_UP(NO_START_UP_UNITS))|
02118 tf_objt2.f90                   (f90) 03789 |      allocate(ORIGINAL_VALUE_OF_U(NO_START_UP_UNITS))|
02119 tf_objt2.f90                   (f90) 03790 |      allocate(INCREASING_VALUE_OF_U(NO_START_UP_UNITS))|
02120 tf_objt2.f90                   (f90) 03791 |      allocate(GLOBAL_BLOCK_INDEX(MAX_OUTAGE_BLOCKS))|
02121 tf_objt2.f90                   (f90) 03792 |      allocate(MIN_UP_TIME(NO_START_UP_UNITS))|
02122 tf_objt2.f90                   (f90) 03793 |      allocate(MIN_DOWN_TIME(NO_START_UP_UNITS))|
02123 tf_objt2.f90                   (f90) 03794 |      allocate(RAMP_RATE(NO_START_UP_UNITS))|
02124 tf_objt2.f90                   (f90) 03795 |      allocate(RAMP_DOWN_RATE(NO_START_UP_UNITS))|
02125 tf_objt2.f90                   (f90) 03796 |      allocate(START_UP_COST_PER_MWH(NO_START_UP_UNITS))|
02126 tf_objt2.f90                   (f90) 03797 |      allocate(EMERGENCY_COST_PER_MWH(NO_START_UP_UNITS))|
02127 tf_objt2.f90                   (f90) 03798 |      allocate(MINIMUM_UP_INDEX(NO_START_UP_UNITS))|
02128 tf_objt2.f90                   (f90) 03799 |      allocate(SORTED_OPTIONS(MAX_OUTAGE_BLOCKS))|
02129 tf_objt2.f90                   (f90) 03800 |      allocate(FLOOR_SORTED_OPTIONS(MAX_OUTAGE_BLOCKS))|
02130 tf_objt2.f90                   (f90) 03801 |      allocate(CEILING_SORTED_OPTIONS(MAX_OUTAGE_BLOCKS))|
02131 tf_objt2.f90                   (f90) 03802 |      allocate(MUST_RUN_OPTIONS(MAX_OUTAGE_BLOCKS))|
02132 tf_objt2.f90                   (f90) 03803 |      allocate(UNIT_FOR_OUTAGE_BLOCK(MAX_OUTAGE_BLOCKS))|
02133 tf_objt2.f90                   (f90) 03804 |      allocate(S_U_FOR_OUTAGE_BLOCK(MAX_OUTAGE_BLOCKS))|
02134 tf_objt2.f90                   (f90) 03805 |      allocate(BLOCK_FOR_OUTAGE_BLOCK(MAX_OUTAGE_BLOCKS))|
02135 tf_objt2.f90                   (f90) 03806 |      allocate(DISPATCH_COST_FOR_BLOCK(MAX_OUTAGE_BLOCKS))|
02136 tf_objt2.f90                   (f90) 03807 |      allocate(ALT_DISPATCH_COST_FOR_BLOCK(MAX_OUTAGE_BLOCKS))|
02137 tf_objt2.f90                   (f90) 03808 |      allocate(FLOOR_PRICE_FOR_BLOCK(MAX_OUTAGE_BLOCKS))|
02138 tf_objt2.f90                   (f90) 03809 |      allocate(CEILING_PRICE_FOR_BLOCK(MAX_OUTAGE_BLOCKS))|
02139 tf_objt2.f90                   (f90) 03810 |      allocate(MARKET_FLOOR_UNIT(NO_START_UP_UNITS))|
02140 tf_objt2.f90                   (f90) 03811 |      allocate(MARKET_CEILING_UNIT(NO_START_UP_UNITS))|
02141 tf_objt2.f90                   (f90) 03812 |      allocate(RETAIL_REVENUE_FOR_BLOCK(MAX_OUTAGE_BLOCKS))|
02142 tf_objt2.f90                   (f90) 03813 |      allocate(UNIT_GEN_IN_SYSTEM(2,0:NO_START_UP_UNITS))|
02143 tf_objt2.f90                   (f90) 03814 |      allocate(UNIT_HEAT_PARAM(2,0:NO_START_UP_UNITS))|
02144 tf_objt2.f90                   (f90) 03815 |      allocate(UNIT_SPIN_IN_SYSTEM(2,0:NO_START_UP_UNITS))|
02145 tf_objt2.f90                   (f90) 03816 |      allocate(OUTAGE_BLOCK_BY_SEGMENT(2,NO_START_UP_UNITS))|
02146 tf_objt2.f90                   (f90) 03817 |      allocate(MARKET_RESOURCE_COUNTER(TOTAL_START_UP_UNITS))|
02147 tf_objt2.f90                   (f90) 03818 |      allocate(DAILY_OPTION_STATUS(0:MAX_DAYS,NO_START_UP_UNITS))|
02148 tf_objt2.f90                   (f90) 03819 |      allocate(PRICING_GROUP_SELL_PRICE( &|
02149 tf_objt2.f90                   (f90) 03821 |      allocate(PRICING_GROUP_SELL_NAME(LOCAL_PRICING_GROUPS))|
02150 tf_objt2.f90                   (f90) 03822 |      allocate(PRICING_GROUP_SELL_QUANT(0:LOCAL_PRICING_GROUPS))|
02151 tf_objt2.f90                   (f90) 03824 |      allocate(PRICING_GROUP_SELL_QUANT_AVAIL( &|
02152 tf_objt2.f90                   (f90) 03827 |      allocate(PRICING_GROUP_BUY_QUANT_AVAIL( &|
02153 tf_objt2.f90                   (f90) 03830 |      allocate(PRICING_GROUP_SELL_GEN(0:LOCAL_PRICING_GROUPS,24))|
02154 tf_objt2.f90                   (f90) 03831 |      allocate(PRICING_GROUP_SELL_INDEX(LOCAL_PRICING_GROUPS))|
02155 tf_objt2.f90                   (f90) 03834 |      allocate(PRICING_GROUP_BUY_PRICE( &|
02156 tf_objt2.f90                   (f90) 03836 |      allocate(PRICING_GROUP_BUY_NAME( &|
02157 tf_objt2.f90                   (f90) 03838 |      allocate(PRICING_GROUP_BUY_QUANT(0: &|
02158 tf_objt2.f90                   (f90) 03841 |      allocate(PRICING_GROUP_BUY_INDEX(LOCAL_BUY_PRICING_GROUPS))|
02159 tf_objt2.f90                   (f90) 03842 |      allocate(ALL_RESOURCE_TYPE(ALL_RESOURCE_NUMBER))|
02160 tf_objt2.f90                   (f90) 03843 |      allocate(ALL_RESOURCE_INDEX(ALL_RESOURCE_NUMBER))|
02161 tf_objt2.f90                   (f90) 03844 |      allocate(ALL_DEMAND_TYPE(ALL_DEMAND_NUMBER))|
02162 tf_objt2.f90                   (f90) 03845 |      allocate(ALL_DEMAND_INDEX(ALL_DEMAND_NUMBER))|
02163 tf_objt2.f90                   (f90) 03846 |      allocate(ALL_RESOURCE_PRICE(ALL_RESOURCE_NUMBER))|
02164 tf_objt2.f90                   (f90) 03847 |      allocate(ALL_RESOURCE_QUANTITY(ALL_RESOURCE_NUMBER))|
02165 tf_objt2.f90                   (f90) 03848 |      allocate(ALL_RESOURCE_QUANTITY_USED(ALL_RESOURCE_NUMBER))|
02166 tf_objt2.f90                   (f90) 03849 |      allocate(ALL_DEMAND_SERVED(0:LOCAL_PRICING_GROUPS, &|
02167 tf_objt2.f90                   (f90) 03852 |      allocate(ALL_DEMAND_PRICE(ALL_DEMAND_NUMBER))|
02168 tf_objt2.f90                   (f90) 03853 |      allocate(ALL_DEMAND_QUANTITY(ALL_DEMAND_NUMBER))|
02169 tf_objt2.f90                   (f90) 03854 |      allocate(ALL_DEMAND_NAME(ALL_DEMAND_NUMBER))|
02170 tf_objt2.f90                   (f90) 03855 |      allocate(ALL_DEMAND_BUY_GEN(0:ALL_DEMAND_NUMBER,24))|
02171 tf_objt2.f90                   (f90) 03856 |      allocate(HOURLY_RESOURCES_AND_MARKETS( &|
02172 tf_objt2.f90                   (f90) 13525 |         ALLOCATE(JOINT_DISPATCH_ORDER(MAX_NBLOK,4))|
02173 tf_objt2.f90                   (f90) 13532 |         ALLOCATE(COMBINED_GEN_IN_SYSTEM(2,LOCAL_NUNITS), &|
02174 transobj.f90                   (f90) 01343 |      ALLOCATE(MULTI_AREA_NAME(0:MAX_TRANS_GROUPS))|
02175 transobj.f90                   (f90) 01344 |      ALLOCATE(TRANS_GROUP_LOAD_ACTIVE(MAX_TRANS_GROUP_NUMBER))|
02176 transobj.f90                   (f90) 01345 |      ALLOCATE(REPORT_AREA_ACTIVE(MAX_TRANS_GROUPS))|
02177 transobj.f90                   (f90) 01466 |      ALLOCATE(MONTHLY_MARKET_PRICES(HOURS_IN_MONTH))|
02178 transobj.f90                   (f90) 01651 |         ALLOCATE(ANNUAL_MARKET_PRICES(ANNUAL_HOURS)) |
02179 transobj.f90                   (f90) 01791 |            ALLOCATE(ANNUAL_MARKET_PRICES(ANNUAL_HOURS)) |
02180 transobj.f90                   (f90) 01991 |               ALLOCATE( &|
02181 transobj.f90                   (f90) 01993 |               ALLOCATE( &|
02182 transobj.f90                   (f90) 01995 |               ALLOCATE(MAX_USER_W_PRICE(UPPER_TRANS_GROUP))|
02183 transobj.f90                   (f90) 01996 |               ALLOCATE(MIN_USER_W_PRICE(UPPER_TRANS_GROUP))|
02184 transobj.f90                   (f90) 01997 |               ALLOCATE(MONTH_MAX_USER_W_PRICE(12,UPPER_TRANS_GROUP))|
02185 transobj.f90                   (f90) 01998 |               ALLOCATE(MONTH_MIN_USER_W_PRICE(12,UPPER_TRANS_GROUP))|
02186 transobj.f90                   (f90) 01999 |               ALLOCATE(SUM_USER_W_PRICES(UPPER_TRANS_GROUP))|
02187 transobj.f90                   (f90) 02000 |               ALLOCATE(MONTH_SUM_USER_W_PRICES(12,UPPER_TRANS_GROUP))|
02188 transobj.f90                   (f90) 02482 |               ALLOCATE( &|
02189 transobj.f90                   (f90) 02484 |               ALLOCATE(MAX_USER_W_PRICE(UPPER_TRANS_GROUP))|
02190 transobj.f90                   (f90) 02485 |               ALLOCATE(MIN_USER_W_PRICE(UPPER_TRANS_GROUP))|
02191 transobj.f90                   (f90) 02486 |               ALLOCATE(MONTH_MAX_USER_W_PRICE(12,UPPER_TRANS_GROUP))|
02192 transobj.f90                   (f90) 02487 |               ALLOCATE(MONTH_MIN_USER_W_PRICE(12,UPPER_TRANS_GROUP))|
02193 transobj.f90                   (f90) 02488 |               ALLOCATE(SUM_USER_W_PRICES(UPPER_TRANS_GROUP))|
02194 transobj.f90                   (f90) 02489 |               ALLOCATE(MONTH_SUM_USER_W_PRICES(12,UPPER_TRANS_GROUP))|
02195 transobj.f90                   (f90) 02866 |            ALLOCATE(M_ANNUAL_PURCHASE_ENERGY(UPPER_TRANS_GROUP))|
02196 transobj.f90                   (f90) 02867 |            ALLOCATE(M_ANNUAL_PURCHASE_COSTS(UPPER_TRANS_GROUP))|
02197 transobj.f90                   (f90) 02868 |            ALLOCATE(M_ANNUAL_SALES_ENERGY(UPPER_TRANS_GROUP))|
02198 transobj.f90                   (f90) 02869 |            ALLOCATE(M_ANNUAL_SALES_REVENUES(UPPER_TRANS_GROUP))|
02199 transobj.f90                   (f90) 02870 |            ALLOCATE(M_ANNUAL_NATIVE_COST(UPPER_TRANS_GROUP))|
02200 transobj.f90                   (f90) 02871 |            ALLOCATE(M_ANNUAL_LOAD_B4_SALES(UPPER_TRANS_GROUP))|
02201 transobj.f90                   (f90) 02872 |            ALLOCATE(M_ANNUAL_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))|
02202 transobj.f90                   (f90) 02873 |            ALLOCATE(M_ANNUAL_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))|
02203 transobj.f90                   (f90) 02874 |            ALLOCATE(M_ANNUAL_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))|
02204 transobj.f90                   (f90) 02875 |            ALLOCATE(ANNUAL_TL_MWH(UPPER_TRANS_GROUP))|
02205 transobj.f90                   (f90) 02876 |            ALLOCATE(ANNUAL_TL_PEAK(UPPER_TRANS_GROUP))|
02206 transobj.f90                   (f90) 02877 |            ALLOCATE(ANNUAL_COIN_PEAK(0:UPPER_TRANS_GROUP))|
02207 transobj.f90                   (f90) 02878 |            ALLOCATE(ANNUAL_TL_BASE(UPPER_TRANS_GROUP))|
02208 transobj.f90                   (f90) 02879 |            ALLOCATE(ANNUAL_TL_HYDRO_MWH(UPPER_TRANS_GROUP))|
02209 transobj.f90                   (f90) 02880 |            ALLOCATE(ANNUAL_TL_HYDRO_MW(UPPER_TRANS_GROUP))|
02210 transobj.f90                   (f90) 02881 |            ALLOCATE(ANNUAL_TL_HYDRO_ROR(UPPER_TRANS_GROUP))|
02211 transobj.f90                   (f90) 02882 |            ALLOCATE(ANNUAL_SPINNING_MWH(UPPER_TRANS_GROUP))|
02212 transobj.f90                   (f90) 02883 |            ALLOCATE(ANNUAL_EFFECTIVE_CAPACITY(UPPER_TRANS_GROUP))|
02213 transobj.f90                   (f90) 02884 |            ALLOCATE(ANNUAL_UNSERVED_ENERGY(UPPER_TRANS_GROUP,0:12))|
02214 transobj.f90                   (f90) 02885 |            ALLOCATE(ANNUAL_ABOVE_RESOURCES(UPPER_TRANS_GROUP,0:12))|
02215 transobj.f90                   (f90) 02886 |            ALLOCATE(ANNUAL_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))|
02216 transobj.f90                   (f90) 02887 |            ALLOCATE(ANNUAL_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP, &|
02217 transobj.f90                   (f90) 02890 |            ALLOCATE(ANNUAL_PRODUCT_PRICE(0:UPPER_TRANS_GROUP, &|
02218 transobj.f90                   (f90) 02892 |            ALLOCATE(ANNUAL_PRODUCT_QUANTITY(0:UPPER_TRANS_GROUP, &|
02219 transobj.f90                   (f90) 02894 |            ALLOCATE(ANNUAL_PRODUCT_HEATRATE(0:UPPER_TRANS_GROUP, &|
02220 transobj.f90                   (f90) 02896 |            ALLOCATE(ANNUAL_PRODUCT_MARGINAL_FUEL(0:UPPER_TRANS_GROUP, &|
02221 transobj.f90                   (f90) 02898 |            ALLOCATE(ANNUAL_PRODUCT_FUEL_PRICE(0:UPPER_TRANS_GROUP, &|
02222 transobj.f90                   (f90) 02900 |            ALLOCATE(ANNUAL_PRODUCT_HOURS(NUM_PRODUCTS))|
02223 transobj.f90                   (f90) 02918 |            ALLOCATE(SYSTEM_HOURLY_LOADS( &|
02224 transobj.f90                   (f90) 03041 |         ALLOCATE(M_MONTHLY_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))|
02225 transobj.f90                   (f90) 03042 |         ALLOCATE(M_MONTHLY_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))|
02226 transobj.f90                   (f90) 03043 |         ALLOCATE(M_MONTHLY_LOAD_B4_SALES(UPPER_TRANS_GROUP))|
02227 transobj.f90                   (f90) 03044 |         ALLOCATE(M_MONTHLY_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))|
02228 transobj.f90                   (f90) 03045 |         ALLOCATE(M_PURCHASE_ENERGY(UPPER_TRANS_GROUP))|
02229 transobj.f90                   (f90) 03046 |         ALLOCATE(M_PURCHASE_COSTS(UPPER_TRANS_GROUP))|
02230 transobj.f90                   (f90) 03047 |         ALLOCATE(MARKET_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP))|
02231 transobj.f90                   (f90) 03048 |         ALLOCATE(M_SALES_ENERGY(UPPER_TRANS_GROUP))|
02232 transobj.f90                   (f90) 03049 |         ALLOCATE(M_SALES_REVENUES(UPPER_TRANS_GROUP))|
02233 transobj.f90                   (f90) 03050 |         ALLOCATE(M_NATIVE_COST(UPPER_TRANS_GROUP))|
02234 transobj.f90                   (f90) 03051 |         ALLOCATE(M_SPINNING_MWH(UPPER_TRANS_GROUP))|
02235 transobj.f90                   (f90) 03052 |         ALLOCATE(M_UNSERVED_ENERGY(UPPER_TRANS_GROUP))|
02236 transobj.f90                   (f90) 03053 |         ALLOCATE(M_ABOVE_RESOURCES(UPPER_TRANS_GROUP))|
02237 transobj.f90                   (f90) 03054 |         ALLOCATE(M_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))|
02238 transobj.f90                   (f90) 03055 |         ALLOCATE(TRANS_ROR_CAPACITY(0:UPPER_TRANS_GROUP))|
02239 transobj.f90                   (f90) 03056 |         ALLOCATE(HOURLY_DUMP_CAPACITY(UPPER_TRANS_GROUP))|
02240 transobj.f90                   (f90) 03057 |         ALLOCATE(TRANS_MUST_CAPACITY(UPPER_TRANS_GROUP))|
02241 transobj.f90                   (f90) 03058 |         ALLOCATE(TRANS_SPINNING_CAPACITY(UPPER_TRANS_GROUP))|
02242 transobj.f90                   (f90) 03059 |         ALLOCATE(AREA_PRICE_MULT(UPPER_TRANS_GROUP))|
02243 transobj.f90                   (f90) 03060 |         ALLOCATE(TG_USING_PRICE_DISTN(UPPER_TRANS_GROUP))|
02244 transobj.f90                   (f90) 03061 |         ALLOCATE(HOURLY_SPINNING_CAPACITY(UPPER_TRANS_GROUP))|
02245 transobj.f90                   (f90) 03062 |         ALLOCATE(DAILY_PEAK(UPPER_TRANS_GROUP))|
02246 transobj.f90                   (f90) 03063 |         ALLOCATE(OFF_PEAK_SPINNING_CAPACITY(UPPER_TRANS_GROUP))|
02247 transobj.f90                   (f90) 03064 |         ALLOCATE(TRANS_RAMP_UP(UPPER_TRANS_GROUP))|
02248 transobj.f90                   (f90) 03065 |         ALLOCATE(TRANS_RAMP_DOWN(UPPER_TRANS_GROUP))|
02249 transobj.f90                   (f90) 03066 |         ALLOCATE(TRANS_MAX_IMPORT(UPPER_TRANS_GROUP))|
02250 transobj.f90                   (f90) 03067 |         ALLOCATE(TRANS_MAX_EXPORT(UPPER_TRANS_GROUP))|
02251 transobj.f90                   (f90) 03068 |         ALLOCATE(LAST_HOUR_SELL(UPPER_TRANS_GROUP))|
02252 transobj.f90                   (f90) 03069 |         ALLOCATE(MONTH_COIN_PEAK(0:UPPER_TRANS_GROUP))|
02253 transobj.f90                   (f90) 03070 |         ALLOCATE(MONTH_NON_COIN_PEAK(0:UPPER_TRANS_GROUP))|
02254 transobj.f90                   (f90) 03071 |         ALLOCATE(HOURLY_TRANSACTION(UPPER_TRANS_GROUP))|
02255 transobj.f90                   (f90) 03072 |         ALLOCATE(MAX_HOURLY_IMPORT(UPPER_TRANS_GROUP))|
02256 transobj.f90                   (f90) 03073 |         ALLOCATE(MAX_HOURLY_EXPORT(UPPER_TRANS_GROUP))|
02257 transobj.f90                   (f90) 03074 |         ALLOCATE(SCARCITY_MULT(0:UPPER_TRANS_GROUP))|
02258 transobj.f90                   (f90) 03075 |         ALLOCATE(SYSTEM_STORAGE(0:800,0:UPPER_TRANS_GROUP))|
02259 transobj.f90                   (f90) 03076 |         ALLOCATE(SYSTEM_DERIVATIVES(0:800,UPPER_TRANS_GROUP))|
02260 transobj.f90                   (f90) 03208 |            ALLOCATE(FISCAL_PURCHASE_ENERGY(UPPER_TRANS_GROUP))|
02261 transobj.f90                   (f90) 03209 |            ALLOCATE(FISCAL_PURCHASE_COSTS(UPPER_TRANS_GROUP))|
02262 transobj.f90                   (f90) 03210 |            ALLOCATE(FISCAL_SALES_ENERGY(UPPER_TRANS_GROUP))|
02263 transobj.f90                   (f90) 03211 |            ALLOCATE(FISCAL_SALES_REVENUES(UPPER_TRANS_GROUP))|
02264 transobj.f90                   (f90) 03212 |            ALLOCATE(FISCAL_NATIVE_COST(UPPER_TRANS_GROUP))|
02265 transobj.f90                   (f90) 03213 |            ALLOCATE(FISCAL_LOAD_B4_SALES(UPPER_TRANS_GROUP))|
02266 transobj.f90                   (f90) 03214 |            ALLOCATE(FISCAL_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))|
02267 transobj.f90                   (f90) 03215 |            ALLOCATE(FISCAL_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))|
02268 transobj.f90                   (f90) 03216 |            ALLOCATE(FISCAL_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))|
02269 transobj.f90                   (f90) 03217 |            ALLOCATE(FISCAL_TL_MWH(UPPER_TRANS_GROUP))|
02270 transobj.f90                   (f90) 03218 |            ALLOCATE(FISCAL_TL_PEAK(UPPER_TRANS_GROUP))|
02271 transobj.f90                   (f90) 03219 |            ALLOCATE(FISCAL_COIN_PEAK(0:UPPER_TRANS_GROUP))|
02272 transobj.f90                   (f90) 03220 |            ALLOCATE(FISCAL_TL_BASE(UPPER_TRANS_GROUP))|
02273 transobj.f90                   (f90) 03221 |            ALLOCATE(FISCAL_TL_HYDRO_MWH(UPPER_TRANS_GROUP))|
02274 transobj.f90                   (f90) 03222 |            ALLOCATE(FISCAL_TL_HYDRO_MW(UPPER_TRANS_GROUP))|
02275 transobj.f90                   (f90) 03223 |            ALLOCATE(FISCAL_TL_HYDRO_ROR(UPPER_TRANS_GROUP))|
02276 transobj.f90                   (f90) 03224 |            ALLOCATE(FISCAL_SPINNING_MWH(UPPER_TRANS_GROUP))|
02277 transobj.f90                   (f90) 03225 |            ALLOCATE(FISCAL_EFFECTIVE_CAPACITY(UPPER_TRANS_GROUP))|
02278 transobj.f90                   (f90) 03226 |            ALLOCATE(FISCAL_UNSERVED_ENERGY(UPPER_TRANS_GROUP))|
02279 transobj.f90                   (f90) 03227 |            ALLOCATE(FISCAL_ABOVE_RESOURCES(UPPER_TRANS_GROUP))|
02280 transobj.f90                   (f90) 03228 |            ALLOCATE(FISCAL_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))|
02281 transobj.f90                   (f90) 03229 |            ALLOCATE(FISCAL_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP))|
02282 transobj.f90                   (f90) 03265 |         ALLOCATE(ALL_MONTH_PRICES(R_HOURS_IN_MONTH, &|
02283 transobj.f90                   (f90) 03274 |         ALLOCATE(HOURLY_MARGINAL_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02284 transobj.f90                   (f90) 03275 |         ALLOCATE(HOURLY_LAST_PRICE(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02285 transobj.f90                   (f90) 03276 |         ALLOCATE(DAILY_PRODUCTS_CAPACITY( &|
02286 transobj.f90                   (f90) 03278 |         ALLOCATE(HOURLY_IN_PRICE(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02287 transobj.f90                   (f90) 03279 |         ALLOCATE(HOURLY_MC_AFTER(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02288 transobj.f90                   (f90) 03280 |         ALLOCATE(SYSTEM_OUTPUT(DAILY_HOURS))|
02289 transobj.f90                   (f90) 03281 |         ALLOCATE(TIE_FLOW(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02290 transobj.f90                   (f90) 03282 |         ALLOCATE(HOURLY_LOAD_B4_SALES(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02291 transobj.f90                   (f90) 03283 |         ALLOCATE(HOURLY_TRANSFER_MWH(UPPER_TRANS_GROUP,DAILY_HOURS))|
02292 transobj.f90                   (f90) 03284 |         ALLOCATE(HOURLY_FORWARDS_4_MONTH(UPPER_TRANS_GROUP, &|
02293 transobj.f90                   (f90) 03286 |         ALLOCATE(DAILY_MARKET_PRICE(DAILY_HOURS))|
02294 transobj.f90                   (f90) 03287 |         ALLOCATE(HOURLY_LOADS(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02295 transobj.f90                   (f90) 03288 |         ALLOCATE(TEST_HOURLY_REVENUE(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02296 transobj.f90                   (f90) 03289 |         ALLOCATE(MARGINAL_COST_DELTA(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02297 transobj.f90                   (f90) 03290 |         ALLOCATE(HOURLY_PRO_COST_AFTER_SALES(0:UPPER_TRANS_GROUP, &|
02298 transobj.f90                   (f90) 03292 |         ALLOCATE(HOURLY_CAPACITY(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02299 transobj.f90                   (f90) 03293 |         ALLOCATE(HOURLY_EUE(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02300 transobj.f90                   (f90) 03294 |         ALLOCATE(HOURLY_DERIVATIVES(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02301 transobj.f90                   (f90) 03295 |         ALLOCATE(HOURLY_PRO_COST_B4_SALES(0:UPPER_TRANS_GROUP, &|
02302 transobj.f90                   (f90) 03297 |         ALLOCATE(HOURLY_INCREMENTAL_COST(0:UPPER_TRANS_GROUP, &|
02303 transobj.f90                   (f90) 03299 |         ALLOCATE(HOURLY_LAMDA(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02304 transobj.f90                   (f90) 03300 |         ALLOCATE(PRODUCT_PRICE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02305 transobj.f90                   (f90) 03301 |         ALLOCATE(PRODUCT_VOLATILITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02306 transobj.f90                   (f90) 03302 |         ALLOCATE(PRODUCT_QUANTITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02307 transobj.f90                   (f90) 03303 |         ALLOCATE(PRODUCT_HEATRATE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02308 transobj.f90                   (f90) 03304 |         ALLOCATE(PRODUCT_MARGINAL_FUEL(0:UPPER_TRANS_GROUP, &|
02309 transobj.f90                   (f90) 03306 |         ALLOCATE(PRODUCT_FUEL_PRICE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02310 transobj.f90                   (f90) 03307 |         ALLOCATE(PRODUCT_HOURS(NUM_PRODUCTS))|
02311 transobj.f90                   (f90) 03308 |         ALLOCATE(PRODUCT_MEAN_RETURN(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02312 transobj.f90                   (f90) 03309 |         ALLOCATE(SUM_SQUARED_DEVIATIONS( &|
02313 transobj.f90                   (f90) 03311 |         ALLOCATE(PRODUCT_DAILY_RETURN( &|
02314 transobj.f90                   (f90) 03313 |         ALLOCATE(SCARCITY_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02315 transobj.f90                   (f90) 03314 |         ALLOCATE(ENERGY_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02316 transobj.f90                   (f90) 03334 |            ALLOCATE(PRODUCT_LAST_PRICE( &|
02317 transobj.f90                   (f90) 06315 |         ALLOCATE(HOURLY_MARGINAL_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02318 transobj.f90                   (f90) 06316 |         ALLOCATE(LAST_TRANS_MC(0:UPPER_TRANS_GROUP))|
02319 transobj.f90                   (f90) 06317 |         ALLOCATE(HOURLY_LAST_PRICE(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02320 transobj.f90                   (f90) 06318 |         ALLOCATE(M_HOURLY_MC_B4_SALES(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02321 transobj.f90                   (f90) 06319 |         ALLOCATE(LAST_BUYER(UPPER_TRANS_GROUP))|
02322 transobj.f90                   (f90) 06320 |         ALLOCATE(LAST_SELLER(UPPER_TRANS_GROUP))|
02323 transobj.f90                   (f90) 06321 |         ALLOCATE(PRICE_ANCHORED(UPPER_TRANS_GROUP))|
02324 transobj.f90                   (f90) 06322 |         ALLOCATE(HOURLY_LAMDA(UPPER_TRANS_GROUP,DAILY_HOURS))|
02325 transobj.f90                   (f90) 06323 |         ALLOCATE(TRANSACTIONS_WITHIN_HOUR(DAILY_HOURS))|
02326 transobj.f90                   (f90) 06324 |         ALLOCATE(BUY_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))|
02327 transobj.f90                   (f90) 06325 |         ALLOCATE(SELL_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))|
02328 transobj.f90                   (f90) 06326 |         ALLOCATE(CUM_REDUNDANT_TRANSACTION(MAX_TRANS_WITHIN_HOUR))|
02329 transobj.f90                   (f90) 06327 |         ALLOCATE(LAST_BUY_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))|
02330 transobj.f90                   (f90) 06328 |         ALLOCATE(LAST_SELL_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))|
02331 transobj.f90                   (f90) 06329 |         ALLOCATE(LONG_PATH_TRANSACTION(MAX_TRANS_WITHIN_HOUR))|
02332 transobj.f90                   (f90) 06330 |         ALLOCATE(MARGIN_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))|
02333 transobj.f90                   (f90) 06331 |         ALLOCATE(MW_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))|
02334 transobj.f90                   (f90) 06332 |         ALLOCATE(LAST_MW_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))|
02335 transobj.f90                   (f90) 06333 |         ALLOCATE(TIE_FLOW(UPPER_TRANS_GROUP,DAILY_HOURS))|
02336 transobj.f90                   (f90) 06334 |         ALLOCATE(MARGINAL_COST_DELTA(UPPER_TRANS_GROUP,DAILY_HOURS))|
02337 transobj.f90                   (f90) 06335 |         ALLOCATE(M_HOURLY_LOADS(UPPER_TRANS_GROUP,DAILY_HOURS))|
02338 transobj.f90                   (f90) 06336 |         ALLOCATE(TEST_HOUR_TIE_LIMIT(UPPER_TRANS_GROUP, &|
02339 transobj.f90                   (f90) 06338 |         ALLOCATE(M_HOURLY_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP, &|
02340 transobj.f90                   (f90) 06340 |         ALLOCATE(HOURLY_CAPACITY(UPPER_TRANS_GROUP,DAILY_HOURS))|
02341 transobj.f90                   (f90) 06341 |         ALLOCATE(HOURLY_DERIVATIVES(UPPER_TRANS_GROUP,DAILY_HOURS))|
02342 transobj.f90                   (f90) 06342 |         ALLOCATE(HOURLY_EUE(UPPER_TRANS_GROUP,DAILY_HOURS))|
02343 transobj.f90                   (f90) 06343 |         ALLOCATE(M_HOURLY_PRO_COST_B4_SALES(UPPER_TRANS_GROUP, &|
02344 transobj.f90                   (f90) 06345 |         ALLOCATE(M_HOURLY_INCREMENTAL_COST(UPPER_TRANS_GROUP, &|
02345 transobj.f90                   (f90) 06347 |         ALLOCATE(HOURLY_SELL_MWH(TOTAL_BUYER_INDEX,TOTAL_BUYER_INDEX))|
02346 transobj.f90                   (f90) 06348 |         ALLOCATE(MONTHLY_TRANSACTION_MWH(UPPER_TRANS_GROUP, &|
02347 transobj.f90                   (f90) 06350 |         ALLOCATE(TOTAL_DELIVERED_COST(UPPER_TRANS_GROUP, &|
02348 transobj.f90                   (f90) 06352 |         ALLOCATE(SELLERS_LOCAL_CAPACITY(UPPER_TRANS_GROUP, &|
02349 transobj.f90                   (f90) 06354 |         ALLOCATE(M_MONTHLY_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))|
02350 transobj.f90                   (f90) 06355 |         ALLOCATE(M_MONTHLY_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))|
02351 transobj.f90                   (f90) 06356 |         ALLOCATE(M_MONTHLY_LOAD_B4_SALES(UPPER_TRANS_GROUP))|
02352 transobj.f90                   (f90) 06357 |         ALLOCATE(M_MONTHLY_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))|
02353 transobj.f90                   (f90) 06358 |         ALLOCATE(M_PURCHASE_ENERGY(UPPER_TRANS_GROUP))|
02354 transobj.f90                   (f90) 06359 |         ALLOCATE(M_PURCHASE_COSTS(UPPER_TRANS_GROUP))|
02355 transobj.f90                   (f90) 06360 |         ALLOCATE(MARKET_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP))|
02356 transobj.f90                   (f90) 06361 |         ALLOCATE(M_SALES_ENERGY(UPPER_TRANS_GROUP))|
02357 transobj.f90                   (f90) 06362 |         ALLOCATE(M_SALES_REVENUES(UPPER_TRANS_GROUP))|
02358 transobj.f90                   (f90) 06363 |         ALLOCATE(M_NATIVE_COST(UPPER_TRANS_GROUP))|
02359 transobj.f90                   (f90) 06364 |         ALLOCATE(M_SPINNING_MWH(UPPER_TRANS_GROUP))|
02360 transobj.f90                   (f90) 06365 |         ALLOCATE(ALLOWED_TRANSACTION_PAIR(UPPER_TRANS_GROUP, &|
02361 transobj.f90                   (f90) 06367 |         ALLOCATE(TRANSACTIONS_PER_HOUR(UPPER_TRANS_GROUP, &|
02362 transobj.f90                   (f90) 06369 |         ALLOCATE(LONG_PATH_TRANSACTION_PAIR(UPPER_TRANS_GROUP, &|
02363 transobj.f90                   (f90) 06371 |         ALLOCATE(M_UNSERVED_ENERGY(UPPER_TRANS_GROUP))|
02364 transobj.f90                   (f90) 06372 |         ALLOCATE(M_ABOVE_RESOURCES(UPPER_TRANS_GROUP))|
02365 transobj.f90                   (f90) 06374 |         ALLOCATE(CAPPED_PRICE(UPPER_TRANS_GROUP))|
02366 transobj.f90                   (f90) 06375 |         ALLOCATE(PRICE_MINIMUM(UPPER_TRANS_GROUP))|
02367 transobj.f90                   (f90) 06376 |         ALLOCATE(M_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))|
02368 transobj.f90                   (f90) 06377 |         ALLOCATE(NET_MARGIN(UPPER_TRANS_GROUP))|
02369 transobj.f90                   (f90) 06378 |         ALLOCATE(BEST_PRICE_TO_BUYER(UPPER_TRANS_GROUP,2))|
02370 transobj.f90                   (f90) 06379 |         ALLOCATE(M_HOURLY_LOAD_B4_SALES( &|
02371 transobj.f90                   (f90) 06381 |         ALLOCATE(TRANS_ROR_CAPACITY(0:UPPER_TRANS_GROUP))|
02372 transobj.f90                   (f90) 06382 |         ALLOCATE(HOURLY_DUMP_CAPACITY(UPPER_TRANS_GROUP))|
02373 transobj.f90                   (f90) 06383 |         ALLOCATE(HOURLY_DUMP_BEFORE(UPPER_TRANS_GROUP))|
02374 transobj.f90                   (f90) 06384 |         ALLOCATE(TRANS_MUST_CAPACITY(UPPER_TRANS_GROUP))|
02375 transobj.f90                   (f90) 06385 |         ALLOCATE(TRANS_SPINNING_CAPACITY(UPPER_TRANS_GROUP))|
02376 transobj.f90                   (f90) 06386 |         ALLOCATE(HOURLY_SPINNING_CAPACITY(UPPER_TRANS_GROUP))|
02377 transobj.f90                   (f90) 06387 |         ALLOCATE(DAILY_PEAK(UPPER_TRANS_GROUP))|
02378 transobj.f90                   (f90) 06388 |         ALLOCATE(OFF_PEAK_SPINNING_CAPACITY(UPPER_TRANS_GROUP))|
02379 transobj.f90                   (f90) 06389 |         ALLOCATE(TRANS_RAMP_UP(UPPER_TRANS_GROUP))|
02380 transobj.f90                   (f90) 06390 |         ALLOCATE(TRANS_RAMP_DOWN(UPPER_TRANS_GROUP))|
02381 transobj.f90                   (f90) 06391 |         ALLOCATE(TRANS_MAX_IMPORT(UPPER_TRANS_GROUP))|
02382 transobj.f90                   (f90) 06392 |         ALLOCATE(TRANS_MAX_EXPORT(UPPER_TRANS_GROUP))|
02383 transobj.f90                   (f90) 06393 |         ALLOCATE(LAST_HOUR_SELL(UPPER_TRANS_GROUP))|
02384 transobj.f90                   (f90) 06394 |         ALLOCATE(MONTH_COIN_PEAK(0:UPPER_TRANS_GROUP))|
02385 transobj.f90                   (f90) 06395 |         ALLOCATE(MONTH_NON_COIN_PEAK(0:UPPER_TRANS_GROUP))|
02386 transobj.f90                   (f90) 06396 |         ALLOCATE(HOURLY_TRANSACTION(UPPER_TRANS_GROUP))|
02387 transobj.f90                   (f90) 06397 |         ALLOCATE(MAX_HOURLY_IMPORT(UPPER_TRANS_GROUP))|
02388 transobj.f90                   (f90) 06398 |         ALLOCATE(MAX_HOURLY_EXPORT(UPPER_TRANS_GROUP))|
02389 transobj.f90                   (f90) 06399 |         ALLOCATE(SCARCITY_MULT(0:UPPER_TRANS_GROUP))|
02390 transobj.f90                   (f90) 06400 |         ALLOCATE(HOURLY_TRANS_GROUP_LOAD_ACTIVE(UPPER_TRANS_GROUP))|
02391 transobj.f90                   (f90) 06401 |         ALLOCATE(HOURLY_TRANS_GROUP_GEN_ACTIVE(UPPER_TRANS_GROUP))|
02392 transobj.f90                   (f90) 06403 |         ALLOCATE(PRODUCT_PRICE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02393 transobj.f90                   (f90) 06404 |         ALLOCATE(PRODUCT_VOLATILITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02394 transobj.f90                   (f90) 06405 |         ALLOCATE(PRODUCT_QUANTITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02395 transobj.f90                   (f90) 06406 |         ALLOCATE(PRODUCT_HEATRATE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02396 transobj.f90                   (f90) 06407 |         ALLOCATE(PRODUCT_MARGINAL_FUEL( &|
02397 transobj.f90                   (f90) 06409 |         ALLOCATE(PRODUCT_FUEL_PRICE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02398 transobj.f90                   (f90) 06410 |         ALLOCATE(PRODUCT_HOURS(NUM_PRODUCTS))|
02399 transobj.f90                   (f90) 06411 |         ALLOCATE(PRODUCT_MEAN_RETURN(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02400 transobj.f90                   (f90) 06412 |         ALLOCATE(PRODUCT_SCARCITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))|
02401 transobj.f90                   (f90) 06413 |         ALLOCATE(SUM_SQUARED_DEVIATIONS( &|
02402 transobj.f90                   (f90) 06415 |         ALLOCATE(SCARCITY_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02403 transobj.f90                   (f90) 06416 |         ALLOCATE(ENERGY_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))|
02404 transobj.f90                   (f90) 06417 |         ALLOCATE(PRODUCT_DAILY_RETURN(0:UPPER_TRANS_GROUP, &|
02405 transobj.f90                   (f90) 06510 |            ALLOCATE( &|
02406 transobj.f90                   (f90) 06512 |            ALLOCATE(M_ANNUAL_PURCHASE_ENERGY(UPPER_TRANS_GROUP))|
02407 transobj.f90                   (f90) 06513 |            ALLOCATE(M_ANNUAL_PURCHASE_COSTS(UPPER_TRANS_GROUP))|
02408 transobj.f90                   (f90) 06514 |            ALLOCATE(M_ANNUAL_SALES_ENERGY(UPPER_TRANS_GROUP))|
02409 transobj.f90                   (f90) 06515 |            ALLOCATE(M_ANNUAL_SALES_REVENUES(UPPER_TRANS_GROUP))|
02410 transobj.f90                   (f90) 06516 |            ALLOCATE(M_ANNUAL_NATIVE_COST(UPPER_TRANS_GROUP))|
02411 transobj.f90                   (f90) 06517 |            ALLOCATE(M_ANNUAL_LOAD_B4_SALES(UPPER_TRANS_GROUP))|
02412 transobj.f90                   (f90) 06518 |            ALLOCATE(M_ANNUAL_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))|
02413 transobj.f90                   (f90) 06519 |            ALLOCATE(M_ANNUAL_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))|
02414 transobj.f90                   (f90) 06520 |            ALLOCATE(M_ANNUAL_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))|
02415 transobj.f90                   (f90) 06521 |            ALLOCATE(ANNUAL_TL_MWH(UPPER_TRANS_GROUP))|
02416 transobj.f90                   (f90) 06522 |            ALLOCATE(ANNUAL_TL_PEAK(UPPER_TRANS_GROUP))|
02417 transobj.f90                   (f90) 06523 |            ALLOCATE(ANNUAL_COIN_PEAK(0:UPPER_TRANS_GROUP))|
02418 transobj.f90                   (f90) 06524 |            ALLOCATE(ANNUAL_TL_BASE(UPPER_TRANS_GROUP))|
02419 transobj.f90                   (f90) 06525 |            ALLOCATE(ANNUAL_TL_HYDRO_MWH(UPPER_TRANS_GROUP))|
02420 transobj.f90                   (f90) 06526 |            ALLOCATE(ANNUAL_TL_HYDRO_MW(UPPER_TRANS_GROUP))|
02421 transobj.f90                   (f90) 06527 |            ALLOCATE(ANNUAL_TL_HYDRO_ROR(UPPER_TRANS_GROUP))|
02422 transobj.f90                   (f90) 06528 |            ALLOCATE(ANNUAL_SPINNING_MWH(UPPER_TRANS_GROUP))|
02423 transobj.f90                   (f90) 06529 |            ALLOCATE(ANNUAL_EFFECTIVE_CAPACITY(UPPER_TRANS_GROUP))|
02424 transobj.f90                   (f90) 06530 |            ALLOCATE(ANNUAL_UNSERVED_ENERGY(UPPER_TRANS_GROUP,0:12))|
02425 transobj.f90                   (f90) 06531 |            ALLOCATE(ANNUAL_ABOVE_RESOURCES(UPPER_TRANS_GROUP,0:12))|
02426 transobj.f90                   (f90) 06532 |            ALLOCATE(ANNUAL_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))|
02427 transobj.f90                   (f90) 06533 |            ALLOCATE(ANNUAL_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP, &|
02428 transobj.f90                   (f90) 06535 |            ALLOCATE(ANNUAL_TRANSACTION_MWH(UPPER_TRANS_GROUP, &|
02429 transobj.f90                   (f90) 06561 |            ALLOCATE(ANNUAL_PRODUCT_PRICE(0:UPPER_TRANS_GROUP, &|
02430 transobj.f90                   (f90) 06563 |            ALLOCATE(ANNUAL_PRODUCT_SCARCITY(0:UPPER_TRANS_GROUP, &|
02431 transobj.f90                   (f90) 06565 |            ALLOCATE(ANNUAL_PRODUCT_QUANTITY(0:UPPER_TRANS_GROUP, &|
02432 transobj.f90                   (f90) 06567 |            ALLOCATE(ANNUAL_PRODUCT_HEATRATE(0:UPPER_TRANS_GROUP, &|
02433 transobj.f90                   (f90) 06569 |            ALLOCATE(ANNUAL_PRODUCT_MARGINAL_FUEL(0:UPPER_TRANS_GROUP, &|
02434 transobj.f90                   (f90) 06571 |            ALLOCATE(ANNUAL_PRODUCT_FUEL_PRICE(0:UPPER_TRANS_GROUP, &|
02435 transobj.f90                   (f90) 06573 |            ALLOCATE(ANNUAL_PRODUCT_HOURS(NUM_PRODUCTS))|
02436 transobj2.f90                  (f90) 00372 |         ALLOCATE(HOURLY_TRANS_REVENUE(MAX_TIE_GROUPS,DAILY_HOURS))|
02437 transobj2.f90                  (f90) 01043 |      ALLOCATE(TRANS_CLASS_POINTER(0:1023), &|
02438 transobj2.f90                  (f90) 03256 |            ALLOCATE(TRANSACTION_NAME(NUM_TRANSACTIONS))|
02439 transobj2.f90                  (f90) 03257 |            ALLOCATE(MONTHLY_USER_CF(NUM_TRANSACTIONS))|
02440 transobj2.f90                  (f90) 03258 |            ALLOCATE(MONTHLY_USER_CF_COUNTER(NUM_TRANSACTIONS))|
02441 transobj2.f90                  (f90) 03259 |            ALLOCATE(MONTHLY_CF_TRANS(NUM_TRANSACTIONS))|
02442 transobj2.f90                  (f90) 03260 |            ALLOCATE(USER_CF(NUM_TRANSACTIONS))|
02443 transobj2.f90                  (f90) 03261 |            ALLOCATE(COUNTERPARTY_NAME(NUM_TRANSACTIONS))|
02444 transobj2.f90                  (f90) 03262 |            ALLOCATE(TRANSACTION_ID(NUM_TRANSACTIONS))|
02445 transobj2.f90                  (f90) 03263 |            ALLOCATE(TRANSACTION_GROUP(NUM_TRANSACTIONS))|
02446 transobj2.f90                  (f90) 03264 |            ALLOCATE(TRANSACTION_TYPE(NUM_TRANSACTIONS))|
02447 transobj2.f90                  (f90) 03265 |            ALLOCATE(DERIVATIVE_TYPE(NUM_TRANSACTIONS))|
02448 transobj2.f90                  (f90) 03266 |            ALLOCATE(PRODUCT_INDEX(NUM_TRANSACTIONS))|
02449 transobj2.f90                  (f90) 03267 |            ALLOCATE(TRANSACTION_CLASS(NUM_TRANSACTIONS))|
02450 transobj2.f90                  (f90) 03268 |            ALLOCATE(PRODUCT_TYPE(NUM_TRANSACTIONS))|
02451 transobj2.f90                  (f90) 03269 |            ALLOCATE(OPTION_POSITION(NUM_TRANSACTIONS))|
02452 transobj2.f90                  (f90) 03270 |            ALLOCATE(PRODUCT_ACTIVE(NUM_TRANSACTIONS))|
02453 transobj2.f90                  (f90) 03271 |            ALLOCATE(STRIKE_FREQUENCY(NUM_TRANSACTIONS))|
02454 transobj2.f90                  (f90) 03272 |            ALLOCATE(REPORT_PRODUCT(NUM_TRANSACTIONS))|
02455 transobj2.f90                  (f90) 03273 |            ALLOCATE(UNIT_CONTINGENCY(NUM_TRANSACTIONS))|
02456 transobj2.f90                  (f90) 03274 |            ALLOCATE(UNIT_CONTINGENT_LINK(NUM_TRANSACTIONS))|
02457 transobj2.f90                  (f90) 03275 |            ALLOCATE(ENERGY_MULTIPLIER(NUM_TRANSACTIONS))|
02458 transobj2.f90                  (f90) 03276 |            ALLOCATE(TRANS_ACTIVE_IN_ENDPOINT(NUM_TRANSACTIONS))|
02459 transobj2.f90                  (f90) 03277 |            ALLOCATE(HEAT_RATE_FOR_SPREAD(NUM_TRANSACTIONS))|
02460 transobj2.f90                  (f90) 03278 |            ALLOCATE(USER_DAY_TYPES_ID(NUM_TRANSACTIONS))|
02461 transobj2.f90                  (f90) 03279 |            ALLOCATE(PUMPING_CAPACITY(NUM_TRANSACTIONS))|
02462 transobj2.f90                  (f90) 03280 |            ALLOCATE(PUMPING_STORAGE_EFFICIENCY(NUM_TRANSACTIONS))|
02463 transobj2.f90                  (f90) 03281 |            ALLOCATE(DAILY_PUMPING_MULT(NUM_TRANSACTIONS))|
02464 transobj2.f90                  (f90) 03282 |            ALLOCATE(FUEL_TYPE(NUM_TRANSACTIONS))|
02465 transobj2.f90                  (f90) 03283 |            ALLOCATE(FUEL_PRICE_TYPE(NUM_TRANSACTIONS))|
02466 transobj2.f90                  (f90) 03284 |            ALLOCATE(FUEL_PRICE(NUM_TRANSACTIONS))|
02467 transobj2.f90                  (f90) 03285 |            ALLOCATE(TRANSPORTATION_BASIS_TYPE(NUM_TRANSACTIONS))|
02468 transobj2.f90                  (f90) 03286 |            ALLOCATE(TRANSPORTATION_BASIS(NUM_TRANSACTIONS))|
02469 transobj2.f90                  (f90) 03287 |            ALLOCATE(DELIVERY_ADDER(NUM_TRANSACTIONS))|
02470 transobj2.f90                  (f90) 03288 |            ALLOCATE(COUNTERPARTY_BOND_RATING(NUM_TRANSACTIONS))|
02471 transobj2.f90                  (f90) 03289 |            ALLOCATE(DOLLAR_MWH(NUM_TRANSACTIONS))|
02472 transobj2.f90                  (f90) 03290 |            ALLOCATE(WVPA_RATE_TRACKER(NUM_TRANSACTIONS))|
02473 transobj2.f90                  (f90) 03291 |            ALLOCATE(WVPA_MEM_TRACKER(NUM_TRANSACTIONS), &|
02474 transobj2.f90                  (f90) 03297 |            ALLOCATE(Tax_Credit_Begin_Date(NUM_TRANSACTIONS), &|
02475 transobj2.f90                  (f90) 03313 |            ALLOCATE(GRX_RPS_CAPACITY(NUM_TRANSACTIONS,30))|
02476 transobj2.f90                  (f90) 03314 |            ALLOCATE(SAVE_GRX_RPS_CAPACITY(NUM_TRANSACTIONS,30))|
02477 transobj2.f90                  (f90) 03315 |            ALLOCATE(GRX_STORAGE_PATTERN(NUM_OF_TRANS_CLASSES,8760))|
02478 transobj2.f90                  (f90) 03316 |            ALLOCATE(GRX_INDEP_PATTERN(NUM_OF_TRANS_CLASSES,8760))|
02479 transobj2.f90                  (f90) 03317 |            ALLOCATE(SAVE_GRX_STORAGE_PATTERN &|
02480 transobj2.f90                  (f90) 03319 |            ALLOCATE(ENERGY_PRICE_MULTIPLIER(NUM_TRANSACTIONS))|
02481 transobj2.f90                  (f90) 03320 |            ALLOCATE(MAX_QUANTITY_OF_PRODUCT(NUM_TRANSACTIONS))|
02482 transobj2.f90                  (f90) 03321 |            ALLOCATE(MONTHLY_ENERGY_MULT(NUM_TRANSACTIONS))|
02483 transobj2.f90                  (f90) 03322 |            ALLOCATE(CONTRACT_DATE(NUM_TRANSACTIONS))|
02484 transobj2.f90                  (f90) 03323 |            ALLOCATE(DISTINGUISHING(NUM_TRANSACTIONS))|
02485 transobj2.f90                  (f90) 03324 |            ALLOCATE(REPORTING_GENERATION_GROUP(NUM_TRANSACTIONS))|
02486 transobj2.f90                  (f90) 03325 |            ALLOCATE(EXPENSE_ASSIGNMENT(NUM_TRANSACTIONS))|
02487 transobj2.f90                  (f90) 03326 |            ALLOCATE(EXPENSE_COLLECTION(NUM_TRANSACTIONS))|
02488 transobj2.f90                  (f90) 03327 |            ALLOCATE(ASSET_CLASS_ID(NUM_TRANSACTIONS))|
02489 transobj2.f90                  (f90) 03328 |            ALLOCATE(ASSET_ALLOCATION_VECTOR(NUM_TRANSACTIONS))|
02490 transobj2.f90                  (f90) 03329 |            ALLOCATE(QUANTITY_OF_PRODUCT(NUM_TRANSACTIONS))|
02491 transobj2.f90                  (f90) 03330 |            ALLOCATE(PROPOSED_QUANT_OF_PRODUCT(NUM_TRANSACTIONS))|
02492 transobj2.f90                  (f90) 03331 |            ALLOCATE(SAVED_QUANT_OF_PRODUCT(NUM_TRANSACTIONS))|
02493 transobj2.f90                  (f90) 03332 |            ALLOCATE(HOURLY_QUANTITY(NUM_TRANSACTIONS))|
02494 transobj2.f90                  (f90) 03333 |            ALLOCATE(ENERGY_PRICE(NUM_TRANSACTIONS))|
02495 transobj2.f90                  (f90) 03334 |            ALLOCATE(MONTHLY_ENERGY_PRICE(NUM_TRANSACTIONS))|
02496 transobj2.f90                  (f90) 03335 |            ALLOCATE(CONTINGENT_CAPACITY(24,NUM_TRANSACTIONS))|
02497 transobj2.f90                  (f90) 03336 |            ALLOCATE(MONTHLY_2ND_ENERGY_PRICE(NUM_TRANSACTIONS))|
02498 transobj2.f90                  (f90) 03337 |            ALLOCATE(PRICE_TYPE(NUM_TRANSACTIONS))|
02499 transobj2.f90                  (f90) 03338 |            ALLOCATE(SECOND_ENERGY_PRICE(NUM_TRANSACTIONS))|
02500 transobj2.f90                  (f90) 03339 |            ALLOCATE(MAXIMUM_STRIKES(NUM_TRANSACTIONS))|
02501 transobj2.f90                  (f90) 03340 |            ALLOCATE(BILLING_LAG(NUM_TRANSACTIONS))|
02502 transobj2.f90                  (f90) 03341 |            ALLOCATE(MINIMUM_STRIKES(NUM_TRANSACTIONS))|
02503 transobj2.f90                  (f90) 03342 |            ALLOCATE(DOLLAR_KW_DAY(NUM_TRANSACTIONS))|
02504 transobj2.f90                  (f90) 03343 |            ALLOCATE(DOLLAR_KW_DAY_ESC(NUM_TRANSACTIONS))|
02505 transobj2.f90                  (f90) 03344 |            ALLOCATE(DOLLAR_KW_MONTH(NUM_TRANSACTIONS))|
02506 transobj2.f90                  (f90) 03345 |            ALLOCATE(DOLLAR_KW_MONTH_ESC(NUM_TRANSACTIONS))|
02507 transobj2.f90                  (f90) 03346 |            ALLOCATE(DOLLAR_KW_YEAR(NUM_TRANSACTIONS))|
02508 transobj2.f90                  (f90) 03347 |            ALLOCATE(GRX_DOLLAR_KW_YEAR(NUM_TRANSACTIONS))|
02509 transobj2.f90                  (f90) 03348 |            ALLOCATE(GRX_DOLLAR_KW_MONTH(NUM_TRANSACTIONS))|
02510 transobj2.f90                  (f90) 03349 |            ALLOCATE(DOLLAR_KW_YEAR_ESC(NUM_TRANSACTIONS))|
02511 transobj2.f90                  (f90) 03350 |            ALLOCATE(DOLLAR_MONTH(NUM_TRANSACTIONS))|
02512 transobj2.f90                  (f90) 03351 |            ALLOCATE(DOLLAR_MONTH_ESC(NUM_TRANSACTIONS))|
02513 transobj2.f90                  (f90) 03352 |            ALLOCATE(DOLLAR_DEAL(NUM_TRANSACTIONS))|
02514 transobj2.f90                  (f90) 03353 |            ALLOCATE(DOLLAR_DEAL_ESC(NUM_TRANSACTIONS))|
02515 transobj2.f90                  (f90) 03354 |            ALLOCATE(BEGIN_DAY(NUM_TRANSACTIONS))|
02516 transobj2.f90                  (f90) 03355 |            ALLOCATE(BEGIN_DAY_IN_MONTH(NUM_TRANSACTIONS))|
02517 transobj2.f90                  (f90) 03356 |            ALLOCATE(BEGIN_EP(NUM_TRANSACTIONS))|
02518 transobj2.f90                  (f90) 03357 |            ALLOCATE(END_DAY(NUM_TRANSACTIONS))|
02519 transobj2.f90                  (f90) 03358 |            ALLOCATE(END_DAY_IN_MONTH(NUM_TRANSACTIONS))|
02520 transobj2.f90                  (f90) 03359 |            ALLOCATE(END_EP(NUM_TRANSACTIONS))|
02521 transobj2.f90                  (f90) 03360 |            ALLOCATE(ACTIVE_IN_MONTH(NUM_TRANSACTIONS))|
02522 transobj2.f90                  (f90) 03361 |            ALLOCATE(TRANS_TO_ACTIVE_MONTH(NUM_TRANSACTIONS))|
02523 transobj2.f90                  (f90) 03362 |            ALLOCATE(ACTIVE_IN_YEAR(NUM_TRANSACTIONS))|
02524 transobj2.f90                  (f90) 03363 |            ALLOCATE(ACTIVE_IN_YEAR_INDEX(NUM_TRANSACTIONS))|
02525 transobj2.f90                  (f90) 03364 |            ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))|
02526 transobj2.f90                  (f90) 03365 |            ALLOCATE(ASSET_2_TRANS_INDEX(0:MAX_ASSET_GROUPS, &|
02527 transobj2.f90                  (f90) 03367 |            ALLOCATE(NUMBER_ASSET_2_TRANS(MAX_TRANS_GROUPS))|
02528 transobj2.f90                  (f90) 03368 |            ALLOCATE(FIRST_AC_TG(0:MAX_ASSET_GROUPS))|
02529 transobj2.f90                  (f90) 03369 |            ALLOCATE(NUM_FORWARDS(NUM_OF_TRANS_CLASSES))|
02530 transobj2.f90                  (f90) 03370 |            ALLOCATE(MONTHLY_CONTINGENT_CAPACITY(NUM_OF_TRANS_CLASSES))|
02531 transobj2.f90                  (f90) 03371 |            ALLOCATE(ANNUAL_INTERRUPTIBLE_CAPACITY( &|
02532 transobj2.f90                  (f90) 03373 |            ALLOCATE(ANNU_PLANNING_INTRPT_CAPACITY( &|
02533 transobj2.f90                  (f90) 03375 |            ALLOCATE(ANNUAL_STORAGE_CAPACITY(NUM_OF_TRANS_CLASSES))|
02534 transobj2.f90                  (f90) 03379 |            ALLOCATE(ANNUAL_CAPACITY(744,12,NUM_OF_TRANS_CLASSES))|
02535 transobj2.f90                  (f90) 03380 |            ALLOCATE(NUM_PUTS(NUM_OF_TRANS_CLASSES))|
02536 transobj2.f90                  (f90) 03381 |            ALLOCATE(NUM_STORAGE(NUM_OF_TRANS_CLASSES))|
02537 transobj2.f90                  (f90) 03382 |            ALLOCATE(ANN_NUM_STOR(NUM_OF_TRANS_CLASSES))|
02538 transobj2.f90                  (f90) 03383 |            ALLOCATE(SCEN_NUM_STOR(NUM_OF_TRANS_CLASSES))|
02539 transobj2.f90                  (f90) 03384 |            ALLOCATE(ANN_STOR_POS(NUM_TRANSACTIONS, &|
02540 transobj2.f90                  (f90) 03386 |            ALLOCATE(SCEN_STOR_POS(NUM_TRANSACTIONS, &|
02541 transobj2.f90                  (f90) 03389 |            ALLOCATE(NUM_LF_PUTS(NUM_OF_TRANS_CLASSES))|
02542 transobj2.f90                  (f90) 03390 |            ALLOCATE(NUM_MONTH_PUTS(NUM_OF_TRANS_CLASSES))|
02543 transobj2.f90                  (f90) 03391 |            ALLOCATE(NUM_ANNUAL_PUTS(NUM_OF_TRANS_CLASSES))|
02544 transobj2.f90                  (f90) 03392 |            ALLOCATE(NUM_INTER_PUTS(NUM_OF_TRANS_CLASSES))|
02545 transobj2.f90                  (f90) 03394 |            ALLOCATE(FORWARD_POSITION(NUM_TRANSACTIONS, &|
02546 transobj2.f90                  (f90) 03402 |            ALLOCATE(PUT_POSITION(NUM_TRANSACTIONS, &|
02547 transobj2.f90                  (f90) 03404 |            ALLOCATE(MONTH_PUT_POSITION(NUM_TRANSACTIONS, &|
02548 transobj2.f90                  (f90) 03406 |            ALLOCATE(ANNUAL_PUT_POSITION(NUM_TRANSACTIONS, &|
02549 transobj2.f90                  (f90) 03408 |            ALLOCATE(STORAGE_POSITION(NUM_TRANSACTIONS, &|
02550 transobj2.f90                  (f90) 03410 |            ALLOCATE(BATTERY_POSITION(NUM_TRANSACTIONS, &|
02551 transobj2.f90                  (f90) 03414 |            ALLOCATE(LF_PUT_POSITION(NUM_TRANSACTIONS, &|
02552 transobj2.f90                  (f90) 03416 |            ALLOCATE(INTER_PUT_POSITION(NUM_TRANSACTIONS, &|
02553 transobj2.f90                  (f90) 03420 |            ALLOCATE(TRANS_E_P_INDEX(MAX_TRANS_GROUP_NUM))|
02554 transobj2.f90                  (f90) 03421 |            ALLOCATE(TRANS_EP_2_TRANS_GROUPS(MAX_TRANS_GROUP_NUM))|
02555 transobj2.f90                  (f90) 03422 |            ALLOCATE(TRANS_EP_2_TG(MAX_TRANS_GROUP_NUM))|
02556 transobj2.f90                  (f90) 03423 |            ALLOCATE(YEARS_BEG_DAY_IN_MO(NUM_TRANSACTIONS,0:12))|
02557 transobj2.f90                  (f90) 03424 |            ALLOCATE(YEARS_END_DAY_IN_MO(NUM_TRANSACTIONS,0:12))|
02558 transobj2.f90                  (f90) 03439 |            ALLOCATE(RPS_PROGRAM_NUMBER(NUM_TRANSACTIONS), &|
02559 transobj2.f90                  (f90) 03912 |            ALLOCATE(FISCAL_ENERGY_COST(NUM_TRANSACTIONS))|
02560 transobj2.f90                  (f90) 03913 |            ALLOCATE(FISCAL_ENERGY(NUM_TRANSACTIONS))|
02561 transobj2.f90                  (f90) 03914 |            ALLOCATE(FISCAL_CAPACITY(NUM_TRANSACTIONS))|
02562 transobj2.f90                  (f90) 03915 |            ALLOCATE(FISCAL_ENERGY_REVENUE(NUM_TRANSACTIONS))|
02563 transobj2.f90                  (f90) 03916 |            ALLOCATE(FISCAL_TRANS_HOURS(NUM_TRANSACTIONS))|
02564 transobj2.f90                  (f90) 03917 |            ALLOCATE(FISCAL_PRODUCT_HOURS(NUM_TRANSACTIONS))|
02565 transobj2.f90                  (f90) 03918 |            ALLOCATE(FISCAL_PRODUCT_DAYS(NUM_TRANSACTIONS))|
02566 transobj2.f90                  (f90) 03919 |            ALLOCATE(FISCAL_PRODUCT_MONTHS(NUM_TRANSACTIONS))|
02567 transobj2.f90                  (f90) 03920 |            ALLOCATE(FISCAL_STRIKES(NUM_TRANSACTIONS))|
02568 transobj2.f90                  (f90) 03921 |            ALLOCATE(FISCAL_ACTIVE_IN_YEAR_INDEX(NUM_TRANSACTIONS))|
02569 transobj2.f90                  (f90) 03922 |            ALLOCATE(FISCAL_ACTIVE_IN_YEAR(NUM_TRANSACTIONS))|
02570 transobj2.f90                  (f90) 03923 |            ALLOCATE(FISCAL_TRANSACTION_COST(NUM_TRANSACTIONS))|
02571 transobj2.f90                  (f90) 03924 |            ALLOCATE(FISCAL_TRANSACTION_REVENUE(NUM_TRANSACTIONS))|
02572 transobj2.f90                  (f90) 04677 |         ALLOCATE(MONTHLY_ENERGY_COST(NUM_TRANSACTIONS,0:12))|
02573 transobj2.f90                  (f90) 04678 |         ALLOCATE(MONTHLY_ENERGY(NUM_TRANSACTIONS,0:12))|
02574 transobj2.f90                  (f90) 04679 |         ALLOCATE(MONTHLY_CHARGE(NUM_TRANSACTIONS,0:12))|
02575 transobj2.f90                  (f90) 04680 |         ALLOCATE(MONTHLY_ENR_FOR_REV(NUM_TRANSACTIONS,0:12))|
02576 transobj2.f90                  (f90) 04681 |         ALLOCATE(MONTHLY_ENR_FOR_EXP(NUM_TRANSACTIONS,0:12))|
02577 transobj2.f90                  (f90) 04682 |         ALLOCATE(MONTHLY_CAPACITY(NUM_TRANSACTIONS,0:12))|
02578 transobj2.f90                  (f90) 04683 |         ALLOCATE(MONTHLY_ENERGY_REVENUE(NUM_TRANSACTIONS,0:12))|
02579 transobj2.f90                  (f90) 04684 |         ALLOCATE(MONTHLY_AC_REVENUE(0:MAX_ASSET_CLASS_GROUPS,4,0:12))|
02580 transobj2.f90                  (f90) 04685 |         ALLOCATE(MONTHLY_AC_REVENUE_ENERGY( &|
02581 transobj2.f90                  (f90) 04687 |         ALLOCATE(MONTHLY_AC_EXPENSE(0:MAX_ASSET_CLASS_GROUPS,4,0:12))|
02582 transobj2.f90                  (f90) 04688 |         ALLOCATE(MONTHLY_AC_EXPENSE_ENERGY( &|
02583 transobj2.f90                  (f90) 04690 |         ALLOCATE(MONTHLY_TRANS_HOURS(NUM_TRANSACTIONS,0:12))|
02584 transobj2.f90                  (f90) 04691 |         ALLOCATE(MONTHLY_PRODUCT_HOURS(NUM_TRANSACTIONS,0:12))|
02585 transobj2.f90                  (f90) 04692 |         ALLOCATE(MONTHLY_PRODUCT_DAYS(NUM_TRANSACTIONS,0:12))|
02586 transobj2.f90                  (f90) 04693 |         ALLOCATE(MONTHLY_PRODUCT_MONTHS(NUM_TRANSACTIONS))|
02587 transobj2.f90                  (f90) 04694 |         ALLOCATE(MONTHLY_STRIKES(NUM_TRANSACTIONS,0:12))|
02588 transobj2.f90                  (f90) 04695 |         ALLOCATE(MONTHLY_TRANSACTION_COST(NUM_TRANSACTIONS,0:12))|
02589 transobj2.f90                  (f90) 04696 |         ALLOCATE(MONTHLY_TRANSACTION_REVENUE(NUM_TRANSACTIONS,0:12))|
02590 transobj2.f90                  (f90) 05073 |         ALLOCATE(STRIKES_AVAILABLE(NUM_TRANSACTIONS))|
02591 transobj2.f90                  (f90) 05074 |         ALLOCATE(STRIKES_REQUIRED(NUM_TRANSACTIONS))|
02592 transobj2.f90                  (f90) 05307 |            ALLOCATE(MONTHLY_CT_GROUP_REPORT(0:12,0:MAX_MONTHLY_GROUPS, &|
02593 transobj2.f90                  (f90) 12154 |         ALLOCATE(HRLY_TG_SCEN_ELECT_MULT(744,12,R_TG))|
02594 transobj2.f90                  (f90) 13234 |         ALLOCATE(OBS(PRICE_POINTS,R_MAX_TRANS_GROUPS)) ! DON'T NEED MONTHLY|
02595 transobj2.f90                  (f90) 13235 |         ALLOCATE(LPROB(PRICE_POINTS,R_MAX_TRANS_GROUPS)) ! DON'T NEED MONTHLY|
02596 transobj2.f90                  (f90) 13236 |         ALLOCATE(LODDUR(PRICE_POINTS,R_MAX_TRANS_GROUPS,13)) !  DONE.|
02597 transobj2.f90                  (f90) 13237 |         ALLOCATE(CUM_HOURS(PRICE_POINTS,R_MAX_TRANS_GROUPS,13)) ! DONE.|
02598 transobj2.f90                  (f90) 13238 |         ALLOCATE(CUM_REVENUE(PRICE_POINTS,R_MAX_TRANS_GROUPS,13)) ! NEED MONTHLY|
02599 transobj2.f90                  (f90) 13239 |         ALLOCATE(ENERGY(PRICE_POINTS,R_MAX_TRANS_GROUPS)) ! DON'T NEED MONTHLY|
02600 transobj2.f90                  (f90) 14553 |         ALLOCATE(TRANS_GROUP_POSITION(-1:MAX_TRANS_GROUP_NUMBER))|
02601 transobj2.f90                  (f90) 14613 |         ALLOCATE(SELLER_TRANSACTION_GROUP(MAX_PATHS))|
02602 transobj2.f90                  (f90) 14614 |         ALLOCATE(LONG_PATH_PATH(MAX_PATHS))|
02603 transobj2.f90                  (f90) 14615 |         ALLOCATE(ACTIVE_PATH_FOR_LONG_PATH(MAX_PATHS))|
02604 transobj2.f90                  (f90) 14616 |         ALLOCATE(BELONGS_TO_A_LONG_PATH(MAX_PATHS))|
02605 transobj2.f90                  (f90) 14617 |         ALLOCATE(BUYER_TRANSACTION_GROUP(MAX_PATHS))|
02606 transobj2.f90                  (f90) 14618 |         ALLOCATE(PATH_PERCENT(MAX_PATHS))|
02607 transobj2.f90                  (f90) 14619 |         ALLOCATE(WHEEL_PATH(MAX_PATHS,MAX_WHEELS))|
02608 transobj2.f90                  (f90) 14620 |         ALLOCATE(PATHS_PER_PAIR(-1:NUM_TRANS_GROUPS, &|
02609 transobj2.f90                  (f90) 14622 |         ALLOCATE(LONG_PATH_FOR_PAIR(-1:NUM_TRANS_GROUPS, &|
02610 transobj2.f90                  (f90) 14624 |         ALLOCATE(HOUR_LONG_PATH_FOR_PAIR(-1:NUM_TRANS_GROUPS, &|
02611 transobj2.f90                  (f90) 14626 |         ALLOCATE(HOUR_PATHS_PER_PAIR(-1:NUM_TRANS_GROUPS, &|
02612 transobj2.f90                  (f90) 14628 |         ALLOCATE(HOUR_PATH_FOR_LONG_PATH(MAX_PATHS))|
02613 transobj2.f90                  (f90) 14629 |         ALLOCATE(HOUR_WHEEL_PATH(MAX(INT(1,2),MAX_HOURLY_LONG_PATHS), &|
02614 transobj2.f90                  (f90) 14631 |         ALLOCATE(PATHS_INDEX(-1:NUM_TRANS_GROUPS,-1:NUM_TRANS_GROUPS, &|
02615 transobj2.f90                  (f90) 14633 |         ALLOCATE(HOUR_PATHS_INDEX( &|
02616 transobj2.f90                  (f90) 14636 |         ALLOCATE(TIES_PER_PAIR(-1:NUM_TRANS_GROUPS, &|
02617 transobj2.f90                  (f90) 14638 |         ALLOCATE(TIE_INDEX(-1:NUM_TRANS_GROUPS,-1:NUM_TRANS_GROUPS, &|
02618 transobj2.f90                  (f90) 14640 |         ALLOCATE(TIE_WHEEL_INDEX(-1:NUM_TRANS_GROUPS, &|
02619 transobj2.f90                  (f90) 14643 |         ALLOCATE(SEASON_PATH_LIMIT(-1:NUM_TRANS_GROUPS, &|
02620 transobj2.f90                  (f90) 14645 |         ALLOCATE(HOUR_PATH_LIMIT(-1:NUM_TRANS_GROUPS, &|
02621 transobj2.f90                  (f90) 14647 |         ALLOCATE(HOUR_PATH_MW(-1:NUM_TRANS_GROUPS, &|
02622 transobj2.f90                  (f90) 14649 |         ALLOCATE(SAVE_HOUR_PATH_LIMIT(-1:NUM_TRANS_GROUPS, &|
02623 transobj2.f90                  (f90) 14651 |         ALLOCATE(HOUR_TIE_LOADING(-1:NUM_TRANS_GROUPS, &|
02624 transobj2.f90                  (f90) 14653 |         ALLOCATE(DAILY_TIE_LOADING(MAX_PATHS,24))|
02625 transobj2.f90                  (f90) 14654 |         ALLOCATE(DAILY_TIE_CONSTRAINT(MAX_PATHS,24))|
02626 transobj2.f90                  (f90) 14655 |         ALLOCATE(PATH_NAME(MAX_PATHS))|
02627 transobj2.f90                  (f90) 14656 |         ALLOCATE(LONG_PATH_NAME(MAX_PATHS))|
02628 transobj2.f90                  (f90) 14657 |         ALLOCATE(PATH_WHEEL_RATE(MAX_PATHS))|
02629 transobj2.f90                  (f90) 14658 |         ALLOCATE(PATH_SPREAD(MAX_PATHS))|
02630 transobj2.f90                  (f90) 14659 |         ALLOCATE(PATH_OWNER(MAX_PATHS))|
02631 transobj2.f90                  (f90) 14660 |         ALLOCATE(PATH_KV_RATING(MAX_PATHS))|
02632 transobj2.f90                  (f90) 14661 |         ALLOCATE(PATH_INDUCTANCE(MAX_PATHS))|
02633 transobj2.f90                  (f90) 14662 |         ALLOCATE(PATH_WHEEL_MULT(MAX_PATHS))|
02634 transobj2.f90                  (f90) 14663 |         ALLOCATE(PATH_SPREAD_MULT(MAX_PATHS))|
02635 transobj2.f90                  (f90) 14664 |         ALLOCATE(PATH_SPREAD_OFF_MULT(MAX_PATHS))|
02636 transobj2.f90                  (f90) 14665 |         ALLOCATE(PATH_WHEEL_OFF_MULT(MAX_PATHS))|
02637 transobj2.f90                  (f90) 14666 |         ALLOCATE(MARKET_PRICE_ID(MAX_PATHS))|
02638 transobj2.f90                  (f90) 14667 |         ALLOCATE(SCENARIO_NUM(MAX_PATHS))|
02639 transobj2.f90                  (f90) 14668 |         ALLOCATE(MARKET_PRICE_DELTA(MAX_PATHS))|
02640 transobj2.f90                  (f90) 14669 |         ALLOCATE(TRANS_LINE_CONSTRAINT(MAX_PATHS))|
02641 transobj2.f90                  (f90) 14670 |         ALLOCATE(PEAK_PRICE_DELTA_MULT(MAX_PATHS))|
02642 transobj2.f90                  (f90) 14671 |         ALLOCATE(OFF_PEAK_PRICE_DELTA_MULT(MAX_PATHS))|
02643 transobj2.f90                  (f90) 14672 |         ALLOCATE(TRANS_LINE_INDEX(MAX_PATHS))|
02644 transobj2.f90                  (f90) 14673 |         ALLOCATE(GRX_ID(MAX_PATHS))|
02645 transobj2.f90                  (f90) 14674 |         ALLOCATE(TG_USED_IN_PATH(-1:MAX_TRANS_GROUP_NUMBER))|
02646 transobj2.f90                  (f90) 14675 |         ALLOCATE(REVERSE_PATH(-1:NUM_TRANS_GROUPS, &|
02647 transobj2.f90                  (f90) 15027 |         ALLOCATE(MULTI_AREA_MONTH_PRICE(R_HOURS_IN_MONTH,TEMP_I2))|
02648 transobj2.f90                  (f90) 15028 |         ALLOCATE(SELL_BEG_FO_HR_TL(ACTIVE_PATH_NUMBER))|
02649 transobj2.f90                  (f90) 15029 |         ALLOCATE(SELL_END_FO_HR_TL(ACTIVE_PATH_NUMBER))|
02650 transobj2.f90                  (f90) 15159 |         ALLOCATE(MULTI_AREA_BUY_MONTH_PRICE(R_HOURS_IN_MONTH,TEMP_I2))|
02651 transobj2.f90                  (f90) 15160 |         ALLOCATE(BUY_BEG_FO_HR_TL(ACTIVE_PATH_NUMBER))|
02652 transobj2.f90                  (f90) 15161 |         ALLOCATE(BUY_END_FO_HR_TL(ACTIVE_PATH_NUMBER))|
02653 transobj2.f90                  (f90) 16840 |         ALLOCATE(TRANS_GROUP_POSITION(-1:MAX_TRANS_GROUP_NUMBER))|
02654 transobj2.f90                  (f90) 16862 |         ALLOCATE(CONSTRAINT_ID(MAX_CONSTRAINTS))|
02655 transobj2.f90                  (f90) 16863 |         ALLOCATE(SEASON(MAX_CONSTRAINTS))|
02656 transobj2.f90                  (f90) 16864 |         ALLOCATE(MW_LIMIT(MAX_CONSTRAINTS))|
02657 transobj2.f90                  (f90) 16865 |         ALLOCATE(FROM_AREA(MAX_CONSTRAINTS,MAX_CONSTRAINT_AREAS))|
02658 transobj2.f90                  (f90) 16866 |         ALLOCATE(TO_AREA(MAX_CONSTRAINTS,MAX_CONSTRAINT_AREAS))|
02659 transobj2.f90                  (f90) 16867 |         ALLOCATE(PEAK_MULT(MAX_CONSTRAINTS))|
02660 transobj2.f90                  (f90) 16868 |         ALLOCATE(OFF_PEAK_MULT(MAX_CONSTRAINTS))|
02661 transobj2.f90                  (f90) 16869 |         ALLOCATE(FORCED_OUTAGE_RATE(MAX_CONSTRAINTS))|
02662 transobj2.f90                  (f90) 16870 |         ALLOCATE(FORCED_OUTAGE_DERATE_MW(MAX_CONSTRAINTS))|
02663 transobj2.f90                  (f90) 16871 |         ALLOCATE(TRANS_LINE_POSITION(0:MAX_TRANS_LINE_INDEX))|
02664 transobj2.f90                  (f90) 16897 |         ALLOCATE(CONSTRAINT_PERCENT(MAX_CONSTRAINTS))|
02665 transobj2.f90                  (f90) 16898 |         ALLOCATE(CapTransMultiplier(0:NUM_TRANS_GROUPS, &|
02666 transobj2.f90                  (f90) 16902 |         ALLOCATE(CONSTRAINTS_PER_PAIR( &|
02667 transobj2.f90                  (f90) 16904 |         ALLOCATE(LOCAL_SEASON(MAX_CONSTRAINTS))|
02668 transobj2.f90                  (f90) 16905 |         ALLOCATE(CONSTRAINTS_INDEX( &|
02669 transobj2.f90                  (f90) 16908 |         ALLOCATE(TIES_PER_PAIR(-1:NUM_TRANS_GROUPS, &|
02670 transobj2.f90                  (f90) 16910 |         ALLOCATE(TIE_INDEX(-1:NUM_TRANS_GROUPS,-1:NUM_TRANS_GROUPS, &|
02671 transobj2.f90                  (f90) 16912 |         ALLOCATE(TIE_WHEEL_INDEX(-1:NUM_TRANS_GROUPS, &|
02672 transobj2.f90                  (f90) 16915 |         ALLOCATE(SEASON_CONSTRAINT_LIMIT( &|
02673 transobj2.f90                  (f90) 16918 |         ALLOCATE(HOUR_CONSTRAINT_LIMIT( &|
02674 transobj2.f90                  (f90) 16921 |         ALLOCATE(HOUR_TIE_LOADING(-1:NUM_TRANS_GROUPS, &|
02675 up_objt.f90                    (f90) 00290 |         ALLOCATE(VECTOR_VALUES(ACTIVE_VECTORS,AVAIL_DATA_YEARS), &|
02676 wh_objt.f90                    (f90) 00132 |         ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023), &|
02677 wh_objt.f90                    (f90) 00392 |      ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023), &|
02678 wh_objt.f90                    (f90) 00713 |            ALLOCATE( &|
02679 wh_objt.f90                    (f90) 01468 |      allocate(z(nCases))|
02680 wh_objt.f90                    (f90) 01469 |      allocate(OrgOrder(nCases))|
02681 wh_objt.f90                    (f90) 01664 |      allocate(OrgOrder(nCases))|
02682 wh_objt.f90                    (f90) 01845 |         ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023), &|
02683 wh_objt.f90                    (f90) 01999 |      ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023), &|
02684 wh_objt.f90                    (f90) 02242 |            ALLOCATE( &|
02685 wh_objt.f90                    (f90) 02298 |            ALLOCATE( &|
02686 wh_objt.f90                    (f90) 04354 |         ALLOCATE( &|
02687 wh_objt.f90                    (f90) 05007 |      ALLOCATE(CAPACITY_ADDER(0:TRANS_GROUPS_RECORDS), &|
02688 wh_objt.f90                    (f90) 05411 |            ALLOCATE( &|
02689 wh_objt.f90                    (f90) 05832 |         ALLOCATE (USER_DAY_DATA(HOURS_PER_DAY,USER_DAYS_IN_MONTH, &|
02690 wh_objt.f90                    (f90) 05936 |         ALLOCATE(CF_USER_DAY_DATA(24,7,R_MAX_USER_ID))|
02691 wh_objt.f90                    (f90) 07094 |      allocate( &|
02692 wh_objt.f90                    (f90) 07993 |            ALLOCATE( DEPTH_PRICE(DEPTH_MARKET_INTERVALS, &|
02693 wh_objt.f90                    (f90) 08203 |         ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023), &|
02694 wh_objt.f90                    (f90) 08328 |      ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023), &|
02695 wh_objt.f90                    (f90) 08802 |            ALLOCATE(TRANSACTION_NAME(NUM_TRANSACTIONS), &|
02696 wh_objt.f90                    (f90) 08822 |            ALLOCATE(CONTRACT_BEGIN_DATE(NUM_TRANSACTIONS), &|
02697 wh_objt.f90                    (f90) 08842 |            ALLOCATE(TRANSPORTATION_BASIS(NUM_TRANSACTIONS), &|
02698 wh_objt.f90                    (f90) 08882 |            ALLOCATE(BEGIN_DAY_IN_MONTH(NUM_TRANSACTIONS), &|
02699 wh_objt.f90                    (f90) 09055 |         ALLOCATE( &|
02700 wh_objt.f90                    (f90) 10795 |            ALLOCATE( &|
02701 wh_objt.f90                    (f90) 10810 |            ALLOCATE(REGIONAL_VARIABLE_ACTIVE(NUM_SCEN_VAR), &|
02702 wh_objt.f90                    (f90) 12206 |            ALLOCATE( &|
02703 wvpa_data_bases.f90            (f90) 00090 |         ALLOCATE(REPORT_DATA(0:MAX_COMP_ID,0:MAX_ACCOUNTS_TRACKED,0:15,0:12,0:30), &               |
02704 wvpa_data_bases.f90            (f90) 00296 |         ALLOCATE(OUT_RECORD(0:MAX_ACCOUNTS_TRACKED,PLANT_BAL_REPT_COLS))|
02705 wvpacooprevenuecals.for        (for) 00257 |            ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS),|
02706 wvpacooprevenuecals.for        (for) 00295 |               ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
02707 wvpacooprevenuecals.for        (for) 00309 |            ALLOCATE(REVENUES_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES,|
02708 wvpacooprevenuecals.for        (for) 01053 |            ALLOCATE(REVENUES_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES,|
02709 wvpacooprevenuecals.for        (for) 01072 |         ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))|
02710 wvpartrk.f90                   (f90) 00270 |         ALLOCATE(RATE_CODE_LIST(0:NUM_RATE_TABLES), &|
02711 wvpartrk.f90                   (f90) 00526 |         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))|
02712 wvpartrk.f90                   (f90) 00621 |            ALLOCATE(BC_ASSET_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))|
02713 wvpartrk.f90                   (f90) 00977 |         ALLOCATE(WVPA_TRACKED_EXPENSES(0:12,30,0:4), &|
02714 wvpartrk.f90                   (f90) 02255 |            ALLOCATE(ACTUAL_VALUES(12,7,200), &|

#endif
