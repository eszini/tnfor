#include <stdio.h>
#include <ctype.h>
#include <string.h>

void rutina_x(char s[1024], int *n, int vn[100], char *vp[100]) {
    *n = 0; // Inicializar el contador de variables
    int len = strlen(s);
    int i = 0;

    while (i < len) {
        // Saltar espacios en blanco
        while (i < len && isspace(s[i])) {
            i++;
        }

        // Si encontramos una letra, es el inicio de una variable
        if (i < len && isalpha(s[i])) {
            vp[*n] = &s[i]; // Guardar el puntero al inicio de la variable
            int start = i;

            // Avanzar hasta el final de la variable
            while (i < len && (isalnum(s[i]) || s[i] == ' ' || s[i] == '_')) {
                i++;
            }

            vn[*n] = i - start; // Guardar la longitud de la variable
            (*n)++; // Incrementar el contador de variables
        } else {
            i++;
        }
    }
}

int main() {
    char s[1024] = "                  CASH_VARIABLES(MO,Price of Issued Shares) =";
    int n;
    int vn[100];
    char *vp[100];

    rutina_x(s, &n, vn, vp);

    // Imprimir resultados para verificar
    for (int i = 0; i < n; i++) {
        printf("Variable %d: ", i + 1);
        for (int j = 0; j < vn[i]; j++) {
            printf("%c", vp[i][j]);
        }
        printf(" (longitud: %d)\n", vn[i]);
    }

    return 0;
}

