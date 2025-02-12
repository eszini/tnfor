#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_VARS 100
#define MAX_LEN 256

void extract_allocated_variables(const char *input, char output[MAX_VARS][MAX_LEN], char var_names[MAX_VARS][MAX_LEN], int *count) {
    const char *start = strstr(input, "allocate(");
    if (!start) return;
    
    start += 9; // Avanzar después de "allocate("
    const char *end = strrchr(start, ')');
    if (!end) return;
    
    char buffer[MAX_LEN];
    strncpy(buffer, start, end - start);
    buffer[end - start] = '\0';
    
    const char *ptr = buffer;
    *count = 0;
    
    while (*ptr) {
        while (isspace(*ptr) || *ptr == ',') ptr++; // Saltar espacios y comas
        
        const char *var_start = ptr;
        char *paren_start = strchr(var_start, '(');
        if (!paren_start) break;
        
        // Buscar el cierre de los paréntesis correctamente
        char *paren_end = paren_start;
        int paren_count = 1;
        while (*paren_end && paren_count > 0) {
            paren_end++;
            if (*paren_end == '(') paren_count++;
            if (*paren_end == ')') paren_count--;
        }
        if (paren_count != 0) break; // Manejo de errores en paréntesis
        
        int len = paren_end - var_start + 1;
        strncpy(output[*count], var_start, len);
        output[*count][len] = '\0';
        
        int var_name_len = paren_start - var_start;
        strncpy(var_names[*count], var_start, var_name_len);
        var_names[*count][var_name_len] = '\0';
        
        (*count)++;
        
        ptr = paren_end + 1;
        while (*ptr == ',' || isspace(*ptr)) ptr++; // Moverse a la siguiente variable
    }
}

void process_allocate_and_write(FILE *input_file, FILE *output_file) {
    char line[MAX_LEN];
    int alloc_counter = 1;
    
    while (fgets(line, MAX_LEN, input_file)) {
        char vars[MAX_VARS][MAX_LEN];
        char var_names[MAX_VARS][MAX_LEN];
        int count = 0;
        
        extract_allocated_variables(line, vars, var_names, &count);
        
        for (int i = 0; i < count; i++) {
            fprintf(output_file, "allocate(%s)\n", vars[i]);
            fprintf(output_file, "call check_alloc(\"%04d\", \"%s\", stv_er)\n", alloc_counter, var_names[i]);
            alloc_counter++;
        }
    }
}

int main() {
    FILE *input_file = fopen("allocate_input.txt", "r");
    FILE *output_file = fopen("allocate_output.txt", "w");
    
    if (!input_file || !output_file) {
        perror("Error al abrir el archivo");
        return 1;
    }
    
    process_allocate_and_write(input_file, output_file);
    
    fclose(input_file);
    fclose(output_file);
    
    return 0;
}

