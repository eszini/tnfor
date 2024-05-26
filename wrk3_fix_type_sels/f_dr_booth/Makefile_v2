# Definimos el objetivo "all" para construir el ejecutable
all:    tfor

COMPILER  = gcc
CFLAGS    = -w

# Regla para construir el ejecutable "tfor"
tfor:   tfor.c
	@echo "Compilando tfor.c..."
	@${COMPILER} ${CFLAGS} -o tfor tfor.c 2> errors.log
	@if [ ! -s errors.log ]; then \
		echo "Compilación ok."; \
		echo "Copiando tfor.c a src..."; \
		cp tfor.c ~/wrk/tnfor/A_src; \
		echo "Copiando Makefile a src..."; \
		cp Makefile ~/wrk/tnfor/A_src; \
		echo "Copiando ejecutables a bin..."; \
		if [ -e tfor ]; then \
			cp tfor ~/bin; \
			echo "Executable tfor copied to ~/bin"; \
		else \
			echo "Executable tfor not found."; \
		fi; \
		if [ -e tfor.exe ]; then \
			cp tfor.exe ~/bin; \
			echo "Executable tfor.exe copied to ~/bin"; \
		else \
			echo "Executable tfor.exe not found."; \
		fi; \
	else \
		echo "Errores encontrados durante la compilación:"; \
		cat errors.log; \
	fi
	@rm -f errors.log

# Regla para editar tfor.c
vi:     tfor.c
	vi tfor.c

# Regla para hacer limpieza
clean:
	@echo "Haciendo limpieza ... "
	@rm -f *.exe *.log log *.err tfor



#
# se incluse tfor a secar en clean para compatibilidad 
# con wsl - ubuntu
#




# Comprobar si el archivo existe y copiar si es así
#if [ -e "$FILE" ]; then
#    cp "$FILE" "$DEST_DIR"
#    echo "File copied to $DEST_DIR"
#else
#    echo "File does not exist, no copy performed."
#fi
