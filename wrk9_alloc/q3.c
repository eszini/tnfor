#include <stdio.h>
#include <stdarg.h>

// Función personalizada que maneja cualquier formato de `printf`
void mi_printf(const char *formato, ...) {
    va_list args;
    va_start(args, formato);

    vprintf(formato, args);  // Usa `vprintf` para imprimir el formato correctamente

    va_end(args);
}

int main() {
    // Pruebas con diferentes formatos
    mi_printf("Numero: %5d, Texto: %-20.20s, Decimal: %0.2f\n", 42, "Hola, mundo!", 3.141592);
    mi_printf("Alineado: %-10d | Precisión: %.3f | Ancho: %10s\n", 7, 1.234567, "Test");

    return 0;
}

