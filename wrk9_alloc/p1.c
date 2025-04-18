#include <stdio.h>
#include <stdlib.h>

int main() {
    // Nombre del archivo a verificar
    const char *filename = "errores.msg";

    // Intentar abrir el archivo en modo lectura
    FILE *file = fopen(filename, "r");

    // Verificar si el archivo existe
    if (file) {
        printf("El archivo '%s' existe.\n", filename);
        fclose(file);
    } else {
        printf("El archivo '%s' no existe.\n", filename);
    }

    return 0;
}

