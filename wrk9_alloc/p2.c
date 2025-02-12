#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_VARS 100
#define MAX_LEN 256

void extract_allocated_variables(const char *input, char output[MAX_VARS][MAX_LEN], int *count) {
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
        while (isspace(*ptr)) ptr++; // Eliminar espacios iniciales
        
        const char *var_start = ptr;
        char *paren_start = strchr(var_start, '(');
        char *paren_end = strrchr(var_start, ')');
        
        if (paren_start && paren_end && paren_end > paren_start) {
            int len = paren_end - var_start + 1;
            strncpy(output[*count], var_start, len);
            output[*count][len] = '\0';
            (*count)++;
        }
        
        ptr = paren_end ? paren_end + 1 : ptr + 1;
        while (*ptr == ',') ptr++; // Saltar comas y espacios adicionales
    }
}

int main() {
    char input[] = "allocate(coal_vector_type_is_value(-1:narray),coal_vector_values(31,-1:narray))";
    char vars[MAX_VARS][MAX_LEN];
    int count = 0;
    
    extract_allocated_variables(input, vars, &count);
    
    printf("Variables allocadas:\n");
    for (int i = 0; i < count; i++) {
        printf("%s\n", vars[i]);
    }
    
    return 0;
}

