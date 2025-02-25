#include <stdio.h>
#include <stdarg.h>

void mi_printf(const char *first_arg, const char *fmt, ...) 
{
    va_list args;
    va_start(args, fmt);

    // Imprimir el primer argumento con el formato fijo
    printf("%-20.20s ", first_arg);

    // Imprimir el resto de los argumentos usando `vprintf`
    vprintf(fmt, args);

    va_end(args);
}

int main() 
{
    mi_printf("DEBUG", "Value1: %5d, Text: %-10s, Float: %.2f\n", 42, "Test", 3.1415);
    mi_printf("INFO", "Aligned: %-10d | Precision: %.3f | String: %s\n", 7, 1.234567, "Hello");

    return 0;
}

