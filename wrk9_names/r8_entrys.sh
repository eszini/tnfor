#!/bin/bas4

# Función para crear directorios si no existen
create_dir_if_not_exists() {
    local dir=$1
    if [ ! -d "$dir" ]; then
        echo "Creando directorio: $dir"
        mkdir -p "$dir"
    fi
}

# Mensaje de uso
usage() {
    echo "Uso: $0 [j|k|2|m]"
    exit 1
}

# Validar parámetro
param="${1:-m}"
if [[ ! "$param" =~ ^(j|k|2|m)$ ]]; then
    usage
fi

# Ejecutar comandos basados en el parámetro
case "$param" in
    j)
        echo "Ejecutando con opción 'j'..."
        create_dir_if_not_exists "repo_4j"
        ./tfor -v -opciones=d5 -exec=4 -inp=lrj -dato=repo_4j -out=r_jres.txt -aux=r_jinc.txt -log=r_jsrc.txt > log8j
        ;;
    k)
        echo "Ejecutando con opción 'k'..."
        create_dir_if_not_exists "repo_4k"
        ./tfor -v -opciones=d5 -exec=4 -inp=lrk -dato=repo_4k -out=r_kres.txt -aux=r_kinc.txt -log=r_ksrc.txt > log8k
        ;;
    2)
        echo "Ejecutando con opción '2' (j y k)..."
        create_dir_if_not_exists "repo_4j"
        ./tfor -v -opciones=d5 -exec=3 -inp=lrj -dato=repo_4j -out=r_jres.txt -aux=r_jinc.txt -log=r_jsrc.txt > log8j
        create_dir_if_not_exists "repo_4k"
        ./tfor -v -opciones=d5 -exec=4 -inp=lrk -dato=repo_4k -out=r_kres.txt -aux=r_kinc.txt -log=r_ksrc.txt > log8k
        ;;
    m)
        echo "Ejecutando con opción 'm'..."
        create_dir_if_not_exists "repo4"
        ./tfor -v -opciones=d5 -exec=4 -inp=lr1 -dato=repo4 -out=r_mres.txt -aux=r_minc.txt -log=r_msrc.txt > log8
        ;;
esac

echo "Script completado."

