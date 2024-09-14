#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

int rutina_v(char string[1024], int *nro, char **vector_ptr) 
{
    int count = 0;
    char *start = string;
    char *ptr = string;
    char *current_var = NULL;
    
    while (*ptr != '\0') 
    {
        // Ignoramos los espacios y '_' al principio
        if (isspace(*ptr) || *ptr == '_') 
        {
            ptr++;
            continue;
        }

        // Encontramos el inicio de una variable (debe comenzar con una letra)
        if (isalpha(*ptr)) 
        {
            current_var = ptr;
            while (isalnum(*ptr) || isspace(*ptr) || *ptr == '_') 
            {
                ptr++;
            }

            // Guardamos el final de la variable actual
            if (current_var) 
            {
                int length = ptr - current_var;
                char *variable = (char *)malloc(length + 1);
                strncpy(variable, current_var, length);
                variable[length] = '\0';  // Final del string

                // Almacenamos el puntero a la variable
                vector_ptr[count] = variable;
                count++;
            }
        } 
        else 
        {
            // Si encontramos un carácter no permitido, avanzamos
            ptr++;
        }
    }

    *nro = count; // Guardamos el número de variables encontradas
    return 0;     // Retornamos 0 en caso de éxito
}


int main() 
{
    char string[1024] = "a = fun(str1 str2 str2 str2 + x_var + y_var )";
    int nro = 0;
    char *vector[100]; // Vector para almacenar punteros a las variables encontradas

    // Llamada a la rutina
    rutina_v(string, &nro, vector);

    // Imprimimos los resultados
    printf("Variables encontradas: %d\n", nro);
    for (int i = 0; i < nro; i++) 
    {
        printf("Variable %d: %s\n", i + 1, vector[i]);
        free(vector[i]);  // Liberamos la memoria
    }

    return 0;
}



