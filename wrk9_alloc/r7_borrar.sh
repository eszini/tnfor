#!/bin/bash

# Función para eliminar un archivo si existe
delete_file_if_exists() {
    local file=$1
    if [ -f "$file" ]; then
        echo "Eliminando archivo: $file"
        rm -f "$file"
    fi
}

# Función para eliminar un directorio si existe
delete_dir_if_exists() {
    local dir=$1
    if [ -d "$dir" ]; then
        echo "Eliminando directorio: $dir"
        rm -rf "$dir"
    fi
}

# Eliminar los archivos generados por r8_crear_repo.sh
delete_file_if_exists "lr1"
delete_file_if_exists "lrj"
delete_file_if_exists "lrk"

# Eliminar los directorios generados por r8_crear_repo.sh
delete_dir_if_exists "repo1"
delete_dir_if_exists "repo2"
delete_dir_if_exists "repo3"

# Eliminar los archivos generados por r8_includes.sh
delete_file_if_exists "r_jres.txt"
delete_file_if_exists "r_jinc.txt"
delete_file_if_exists "r_jsrc.txt"
delete_file_if_exists "log8j"
delete_file_if_exists "r_kres.txt"
delete_file_if_exists "r_kinc.txt"
delete_file_if_exists "r_ksrc.txt"
delete_file_if_exists "log8k"
delete_file_if_exists "r_mres.txt"
delete_file_if_exists "r_minc.txt"
delete_file_if_exists "r_msrc.txt"
delete_file_if_exists "log8"

# Eliminar los directorios generados por r8_includes.sh
delete_dir_if_exists "repo_4j"
delete_dir_if_exists "repo_4k"
delete_dir_if_exists "repo3"
delete_dir_if_exists "repo4"

echo "Limpieza completada con éxito."


