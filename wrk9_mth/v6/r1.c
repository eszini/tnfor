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

int main() {
    char s[1024] = "esta es una_variable estaesunvariable otra_variable";
    int n, vn[100];
    char *vp[100];

    rutina_x(s, &n, vn, vp);

    // Mostramos el resultado
    printf("Se encontraron %d variables:\n", n);
    for (int i = 0; i < n; i++) {
        printf("Variable %d: '%.*s' (largo: %d)\n", i + 1, vn[i], vp[i], vn[i]);
    }

    return 0;
}

