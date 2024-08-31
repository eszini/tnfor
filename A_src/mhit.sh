#!/bin/bash


# Mount a hacer
# mount -o bind /cygdrive/e/cygwin64/home/123-D56SW/hit /home/Ernesto/hit



# Ruta de destino en tu home
TARGET_DIR="$HOME/hit"
USB_DIR="/cygdrive/e/cygwin64/home/123-D56SW/hit"

# Verifica si el directorio de destino existe
if [ ! -d "$TARGET_DIR" ]; then
  echo "El directorio de destino $TARGET_DIR no existe, creando..."
  mkdir -p "$TARGET_DIR"
fi

# Función para montar
mount_usb() {
  # Verifica si el USB ya está montado
  if mount | grep "$TARGET_DIR" > /dev/null; then
    echo "El USB ya está montado en $TARGET_DIR."
  else
    echo "Montando el USB..."
    mount -o bind "$USB_DIR" "$TARGET_DIR"
    if [ $? -eq 0 ]; then
      echo "USB montado correctamente en $TARGET_DIR."
    else
      echo "Error al montar el USB."
    fi
  fi
}

# Función para desmontar
umount_usb() {
  # Verifica si el USB está montado
  if mount | grep "$TARGET_DIR" > /dev/null; then
    echo "Desmontando el USB..."
    umount "$TARGET_DIR"
    if [ $? -eq 0 ]; then
      echo "USB desmontado correctamente de $TARGET_DIR."
    else
      echo "Error al desmontar el USB."
    fi
  else
    echo "El USB no está montado en $TARGET_DIR."
  fi
}

# Main
case "$1" in
  m)
    mount_usb
    ;;
  u)
    umount_usb
    ;;
  *)
    echo "Uso: $0 {m|u}"
    echo "m: montar el USB"
    echo "u: desmontar el USB"
    exit 1
    ;;
esac






