#include <stdio.h>
#include <stdarg.h>

// Función similar a printf
void mi_printf(const char *formato, ...) {
    va_list args;  // Lista de argumentos variables
    va_start(args, formato);  // Inicializa la lista con el último argumento fijo

    while (*formato) {
        if (*formato == '%' && *(formato + 1)) {  // Detectar un especificador de formato
            formato++;  // Avanzar al siguiente carácter para ver el tipo

            switch (*formato) {
                case 'd': {  // Entero
                    int i = va_arg(args, int);
                    printf("%d", i);
                    break;
                }
                case 's': {  // Cadena
                    char *s = va_arg(args, char *);
                    printf("%s", s);
                    break;
                }
                case 'f': {  // Número flotante
                    double f = va_arg(args, double);
                    printf("%f", f);
                    break;
                }
                default:
                    putchar('%');  // Imprimir el '%' literal si no es reconocido
                    putchar(*formato);
            }
        } else {
            putchar(*formato);  // Imprimir cualquier otro carácter
        }
        formato++;  // Avanzar al siguiente carácter
    }

    va_end(args);  // Finaliza el uso de va_list
}

int main() {
    // Pruebas con diferentes tipos de argumentos
    mi_printf("Numero: %d, Texto: %s, Decimal: %f\n", 42, "Hola", 3.14);
    mi_printf("Solo texto sin variables\n");
    mi_printf("Mas numeros: %d %d %d\n", 1, 2, 3);
    return 0;
}

