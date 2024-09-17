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

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Uso: %s <archivo_entrada> <archivo_salida>\n", argv[0]);
        return 1;
    }

    FILE *file_in = fopen(argv[1], "r");
    if (file_in == NULL) {
        perror("Error al abrir el archivo de entrada");
        return 1;
    }

    FILE *file_out = fopen(argv[2], "w");
    if (file_out == NULL) {
        perror("Error al abrir el archivo de salida");
        fclose(file_in);
        return 1;
    }

    char line[1024];
    while (fgets(line, sizeof(line), file_in)) {
        int n;
        int vn[100];
        char *vp[100];

        // Escribir la línea original en el archivo de salida
        fprintf(file_out, "Línea: %s", line);

        // Procesar la línea para encontrar variables
        rutina_x(line, &n, vn, vp);

        // Escribir las variables encontradas en el archivo de salida
        for (int i = 0; i < n; i++) {
            fprintf(file_out, "%d (%2d) |%-*.*s|\n", i + 1, vn[i], vn[i], vn[i], vp[i]);
        }
        fprintf(file_out, "\n");
    }

    fclose(file_in);
    fclose(file_out);
    return 0;
}

