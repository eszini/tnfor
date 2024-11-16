#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

int rutina_v(char string[1024], int *nro, char **vector_ptr) 
{
    int count = 0;
    char *token;
    char *separators = " _"; // Separadores permitidos (blancos y '_')
    char *temp_string = strdup(string); // Hacemos una copia temporal del string
    char *str_ptr = temp_string;

    // Tokenizamos el string de acuerdo con los separadores
    while ((token = strtok(str_ptr, separators)) != NULL) 
    {
        // Para continuar tokenizando el string original
        str_ptr = NULL; 
        
        // Validamos la sintaxis de str1 (debe comenzar con una letra y puede seguir con letras o números)
        if (isalpha(token[0])) 
        {
            int valid = 1;

            for (int i = 1; i < strlen(token); i++) 
            {
                if (!isalnum(token[i])) 
                {
                    // No válido si tiene algo que no sea letra o número
                    valid = 0; 
                    break;
                }
            }

            if (valid) 
            {
                // Guardamos el puntero al token válido
printf ("token: |%s| \n",token);
                vector_ptr[count] = token; 
                count++;
            }
        }
    }

    // Liberamos la memoria temporal
    free(temp_string);
    
    *nro = count; // Guardamos el número de variables encontradas
    return 0;     // Retornamos 0 en caso de éxito
}

int main() {
    char string[1024] = "str1 str2 str2_str2";
    int nro = 0;
    char *vector[100]; // Vector para almacenar punteros a las variables encontradas

    // Llamada a la rutina
    rutina_v(string, &nro, vector);

    // Imprimimos los resultados
    printf("Variables encontradas: %d\n", nro);
    for (int i = 0; i < nro; i++) {
        printf("Variable %d: %s\n", i + 1, vector[i]);
    }

    return 0;
}

