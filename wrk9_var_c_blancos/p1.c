#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#define MAX_VARS 1000
#define MAX_VAR_LEN 128

void leer_variables(const char *filename, char vars[MAX_VARS][MAX_VAR_LEN], int *num_vars) {
    FILE *file = fopen(filename, "r");
    char line[1024];
    *num_vars = 0;

    if (file == NULL) {
        printf("No se pudo abrir el archivo %s\n", filename);
        return;
    }

    while (fgets(line, sizeof(line), file)) {
        char *token = strtok(line, " ");
        char var[MAX_VAR_LEN] = "";

        while (token != NULL) {
            strcat(var, token);
            strcat(var, " ");
            token = strtok(NULL, " ");
        }

        // Remover el espacio extra al final
        var[strlen(var) - 1] = '\0';

        strcpy(vars[*num_vars], var);
        (*num_vars)++;
    }

    fclose(file);
}

bool tiene_variable_el_string(const char *variable, const char *s, int *posicion) {
    char *pos = strstr(s, variable);

    if (pos != NULL) {
        *posicion = pos - s;
        return true;
    }

    return false;
}

char vars[MAX_VARS][MAX_VAR_LEN];

int main() 
{
    int num_vars;
    char s[] = "Aquí va tu string donde buscar variables";
    int posicion;
    int i,j;


    leer_variables("mthnmcom.mon", vars, &num_vars);

    for (i=0; i<num_vars; i++)
    {
	printf ("var: %3d |%s| \n",i,vars[i]);
    } 



#if 0
    for (int i = 0; i < num_vars; i++) {
        if (tiene_variable_el_string(vars[i], s, &posicion)) {
            printf("Variable '%s' encontrada en la posición %d\n", vars[i], posicion);
        } else {
            printf("Variable '%s' no encontrada\n", vars[i]);
        }
    }
#endif

    return 0;
}

