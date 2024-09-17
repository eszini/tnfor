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
    FILE *file = fopen("entrada.txt", "r");
    if (file == NULL) {
        perror("Error al abrir el archivo");
        return 1;
    }

    char line[1024];
    while (fgets(line, sizeof(line), file)) {
        int n;
        int vn[100];
        char *vp[100];

        // Imprimir la línea original
        printf("Línea: %s", line);

        // Procesar la línea para encontrar variables
        rutina_x(line, &n, vn, vp);

        // Imprimir las variables encontradas
        for (int i = 0; i < n; i++) {
            printf("%d (%2d) |%-*.*s|\n", i + 1, vn[i], vn[i], vn[i], vp[i]);
        }
        printf("\n");
    }

    fclose(file);
    return 0;
}

