#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE 1024
#define MAX_VARS 100
#define STAT_DEFAULT "stv_er"

// Función para extraer variables de una sentencia allocate
void process_allocate_statement(const char *allocate_stmt, char **output, int *count) {
    char buffer[MAX_LINE];
    strcpy(buffer, allocate_stmt);

    // Buscar el inicio del allocate (
    char *start = strchr(buffer, '(');
    if (!start) return;
    start++; // Mover después de '('

    // Buscar el final del allocate )
    char *end = strrchr(start, ')');
    if (!end) return;
    *end = '\0'; // Reemplazar con terminador de string

    // Verificar si hay un `stat=` en la sentencia
    char *stat_pos = strstr(start, "stat=");
    char stat_var[MAX_LINE] = STAT_DEFAULT;
    if (stat_pos) {
        sscanf(stat_pos, "stat=%s", stat_var);
        *stat_pos = '\0'; // Remover `stat=...` de la cadena
    }

    // Procesar variables asegurando que los espacios embebidos sean ignorados correctamente
    char *token = start;
    int paren_count = 0;
    char var[MAX_LINE] = {0};
    *count = 0;
    int inside_var = 0; // Flag para evitar que espacios sueltos generen variables vacías

    while (*token) {
        if (*token == '(') paren_count++; // Abrimos paréntesis
        if (*token == ')') paren_count--; // Cerramos paréntesis

        if ((*token == ',' && paren_count == 0)) { 
            // Si encontramos una coma en el nivel base y la variable no está vacía, generamos la salida
            if (inside_var && strlen(var) > 0) {
                snprintf(output[*count], MAX_LINE, "allocate(%s, stat=%s)", var, stat_var);
                (*count)++;
            }
            var[0] = '\0'; // Reiniciar buffer para la siguiente variable
            inside_var = 0; // Resetear flag
        } else if (!isspace((unsigned char)*token) || paren_count > 0) {
            // Solo agregamos caracteres si no son espacios sueltos fuera de paréntesis
            strncat(var, token, 1);
            inside_var = 1; // Marcamos que hay una variable válida en proceso
        }

        token++;
    }

    // Última variable después de la última coma
    if (inside_var && strlen(var) > 0) {
        snprintf(output[*count], MAX_LINE, "allocate(%s, stat=%s)", var, stat_var);
        (*count)++;
    }
}

int main() {
    FILE *file = fopen("plan.txt", "r");
    if (!file) {
        perror("Error abriendo el archivo");
        return EXIT_FAILURE;
    }

    char line[MAX_LINE];
    while (fgets(line, sizeof(line), file)) {
        // Buscar la sentencia allocate entre |...|
        char *start = strchr(line, '|');
        if (!start) continue;
        start++;
        char *end = strchr(start, '|');
        if (!end) continue;
        *end = '\0';

        // Procesar la sentencia allocate
        char *alloc_statements[MAX_VARS];
        for (int i = 0; i < MAX_VARS; i++) {
            alloc_statements[i] = malloc(MAX_LINE);
        }

        int count = 0;
        process_allocate_statement(start, alloc_statements, &count);

        // Imprimir resultados
        for (int i = 0; i < count; i++) {
            printf("%s\n", alloc_statements[i]);
            free(alloc_statements[i]);
        }
    }

    fclose(file);
    return 0;
}

