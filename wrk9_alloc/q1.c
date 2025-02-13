#include <stdio.h>
#include <stdarg.h>

// Definir el formato de z en un solo lugar
#if 0
const char *Z_FORMAT = "%-20.20s";  // Se puede cambiar en un solo lugar
#endif

	char	z_format[16] = "-20.20s";

// Función para imprimir mensajes de debug
int	debug_print(z, fmt, ...) 
char	*z;
char	*fmt;
{
    va_list args;
    va_start(args, fmt);

    // Corregir concatenación de formato
    printf("%s ", Z_FORMAT);  
    printf(fmt, z);  

    // Imprimir el resto de los argumentos con el formato recibido
    vprintf(fmt, args);

    va_end(args);
}

int main() {
    char *z = "DEBUG";
    int v1 = 10, v2 = 20, v3 = 30;

    // Ejemplo de uso de la nueva función
    debug_print(z, "Value1: %d, Value2: %d, Value3: %d\n", v1, v2, v3);
    debug_print(z, "Another message with %s and %d\n", "text", 42);

    return 0;
}

