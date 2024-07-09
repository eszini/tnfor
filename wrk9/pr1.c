#include <stdio.h>
#include <string.h>
#include <stdbool.h>

bool tiene_include(char *s, char *t) 
{
    // Define la cadena a buscar
    const char *pattern = "   include  '";
    int pattern_len = strlen(pattern);

    // Busca el patrón en el string s
    char *start = strstr(s, pattern);

    if (start != NULL) 
    {
        // Encuentra el inicio del nombre de archivo
        start += pattern_len;
        char *end = strchr(start, '\'');
        
        if (end != NULL) 
        {
            // Copia el nombre de archivo sin las comillas en t
            strncpy(t, start, end - start);
            t[end - start] = '\0';
            return true;
        }
    }

    // No se encontró el patrón
    return false;
}

int main() 
{
    char s[] = "Este es un ejemplo de cadena con   include  'nombre_de_file_valido' en el medio.";
    char t[100];

    if (tiene_include(s, t)) 
    {
        printf("Se encontró la sentencia. El nombre de archivo es: %s\n", t);
    }
    else 
    {
        printf("No se encontró la sentencia.\n");
    }

    return 0;
}

