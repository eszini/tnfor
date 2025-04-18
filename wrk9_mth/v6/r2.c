#include <stdio.h>
#include <string.h>
#include <ctype.h>

void rutina_x(char s[1024], int *n, int vn[100], char *vp[100]) {
    int i = 0, j = 0, len = strlen(s);
    *n = 0; // Inicializamos el contador de variables encontradas

    while (i < len) {
        // Saltamos los espacios en blanco iniciales
        while (i < len && isspace(s[i])) {
            i++;
        }
        
        if (i >= len) {
            break; // Si llegamos al final del string, salimos del bucle
        }

        // Hemos encontrado una posible variable
        vp[*n] = &s[i]; // Apuntamos al inicio de la variable
        j = i;

        // Continuamos hasta encontrar el final de la variable (un espacio o el final de la cadena)
        while (j < len && (isalnum(s[j]) || s[j] == '_')) {
            j++;
        }

        // Calculamos el largo de la variable encontrada
        vn[*n] = j - i;
        
        // Incrementamos el contador de variables
        (*n)++;

        // Continuamos buscando desde el final de la variable actual
        i = j;
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Uso: %s <archivo de entrada>\n", argv[0]);
        return 1;
    }

    FILE *archivo = fopen(argv[1], "r");
    if (archivo == NULL) {
        printf("Error al abrir el archivo %s\n", argv[1]);
        return 1;
    }

    char s[1024];
    int n, vn[100];
    char *vp[100];
    int linea = 1;

    // Leer el archivo línea por línea
    while (fgets(s, sizeof(s), archivo)) {
        printf("Procesando línea %d: %s", linea, s);

        rutina_x(s, &n, vn, vp);

        // Mostrar el resultado para cada línea
        printf("Se encontraron %d variables en la línea %d:\n", n, linea);
        for (int i = 0; i < n; i++) {
            printf("Variable %d: '%.*s' (largo: %d)\n", i + 1, vn[i], vp[i], vn[i]);
        }
        linea++;
    }

    fclose(archivo);
    return 0;
}

