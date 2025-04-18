#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_VARS 100
#define MAX_LINE_LENGTH 1024

typedef struct {
    char word[50];
    int value;
} DictionaryEntry;

void rutina_x(char s[MAX_LINE_LENGTH], int *n, int vn[MAX_VARS], char *vp[MAX_VARS], int k[MAX_VARS], DictionaryEntry dict[], int dict_size) {
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

            // Verificar si la variable está en el diccionario
            k[*n] = 0; // Inicializar k[i] a 0
            for (int j = 0; j < dict_size; j++) {
                if (strncmp(vp[*n], dict[j].word, vn[*n]) == 0 && strlen(dict[j].word) == vn[*n]) {
                    k[*n] = dict[j].value;
                    break;
                }
            }

            (*n)++; // Incrementar el contador de variables
        } else {
            i++;
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc != 5) {
        fprintf(stderr, "Uso: %s <archivo_diccionario> <archivo_entrada> <archivo_salida> <archivo_mejora_diccionario>\n", argv[0]);
        return 1;
    }

    // Leer el archivo de diccionario
    FILE *file_dict = fopen(argv[1], "r");
    if (file_dict == NULL) {
        perror("Error al abrir el archivo de diccionario");
        return 1;
    }

    DictionaryEntry dict[MAX_VARS];
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

    // Abrir el archivo de mejora de diccionario
    FILE *file_dict_out = fopen(argv[4], "w");
    if (file_dict_out == NULL) {
        perror("Error al abrir el archivo de mejora de diccionario");
        fclose(file_in);
        fclose(file_out);
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
            // Escribir las variables encontradas en el archivo de mejora de diccionario
            fprintf(file_dict_out, "%.*s,0\n", vn[i], vp[i]);
        }
        fprintf(file_out, "\n");
    }

    fclose(file_in);
    fclose(file_out);
    fclose(file_dict_out);
    return 0;
}
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_VARS 100
#define MAX_LINE_LENGTH 1024

typedef struct {
    char word[50];
    int value;
} DictionaryEntry;

void rutina_x(char s[MAX_LINE_LENGTH], int *n, int vn[MAX_VARS], char *vp[MAX_VARS], int k[MAX_VARS], DictionaryEntry dict[], int dict_size) {
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

            // Verificar si la variable está en el diccionario
            k[*n] = 0; // Inicializar k[i] a 0
            for (int j = 0; j < dict_size; j++) {
                if (strncmp(vp[*n], dict[j].word, vn[*n]) == 0 && strlen(dict[j].word) == vn[*n]) {
                    k[*n] = dict[j].value;
                    break;
                }
            }

            (*n)++; // Incrementar el contador de variables
        } else {
            i++;
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc != 5) {
        fprintf(stderr, "Uso: %s <archivo_diccionario> <archivo_entrada> <archivo_salida> <archivo_mejora_diccionario>\n", argv[0]);
        return 1;
    }

    // Leer el archivo de diccionario
    FILE *file_dict = fopen(argv[1], "r");
    if (file_dict == NULL) {
        perror("Error al abrir el archivo de diccionario");
        return 1;
    }

    DictionaryEntry dict[MAX_VARS];
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

    // Abrir el archivo de mejora de diccionario
    FILE *file_dict_out = fopen(argv[4], "w");
    if (file_dict_out == NULL) {
        perror("Error al abrir el archivo de mejora de diccionario");
        fclose(file_in);
        fclose(file_out);
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
            // Escribir las variables encontradas en el archivo de mejora de diccionario
            fprintf(file_dict_out, "%.*s,0\n", vn[i], vp[i]);
        }
        fprintf(file_out, "\n");
    }

    fclose(file_in);
    fclose(file_out);
    fclose(file_dict_out);
    return 0;
}

