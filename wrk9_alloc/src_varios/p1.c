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
    
    char *token = strtok(buffer, ",");
    *count = 0;
    
    while (token && *count < MAX_VARS) {
        while (isspace(*token)) token++; // Eliminar espacios iniciales
        
        char *paren_start = strchr(token, '(');
        char *paren_end = strrchr(token, ')');
        
        if (paren_start && paren_end && paren_end > paren_start) {
            snprintf(output[*count], MAX_LEN, "%.*s", (int)(paren_end - token + 1), token);
            (*count)++;
        }
        
        token = strtok(NULL, ",");
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


