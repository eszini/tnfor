#include <stdio.h>
#include <stdarg.h>

// Definir el formato fijo para el primer argumento (char *)
#define FIRST_ARG_FORMAT "%-20.20s"

// Función personalizada de impresión
void mi_printf(const char *first_arg, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    // Imprimir el primer argumento con el formato fijo
    printf(FIRST_ARG_FORMAT " ", first_arg);

    // Imprimir el resto de los argumentos usando `vprintf`
    vprintf(fmt, args);

    va_end(args);
}

int main() {
    // Pruebas con diferentes formatos y tipos de datos
    mi_printf("DEBUG", "Value1: %5d, Text: %-10s, Float: %.2f\n", 42, "Test", 3.1415);
    mi_printf("INFO", "Aligned: %-10d | Precision: %.3f | String: %s\n", 7, 1.234567, "Hello");
    
    return 0;
}

