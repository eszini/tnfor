# Definimos el objetivo "all" para construir el ejecutable
all: tfor

COMPILER  = gcc
CFLAGS    = -w
COPY     ?= 1  # Por defecto, copiar archivos (1 = copiar, 0 = no copiar)
VERSION   =  10001

BIN_DIR := $(HOME)/bin
SRC_BCK := $(HOME)/bck1
SRC_DIR := $(HOME)/wrk/tnfor/A_src

# Objetivo setup para crear directorios si no existen
setup:
	@echo "Checking and creating directories if they do not exist..."
	@[ -d $(BIN_DIR) ] || (mkdir -p $(BIN_DIR) && echo "Created directory: $(BIN_DIR)")
	@[ -d $(SRC_DIR) ] || (mkdir -p $(SRC_DIR) && echo "Created directory: $(SRC_DIR)")

# Ver version del makefile
version:
	@echo "Makefile - version $(VERSION)"

# Regla para construir el ejecutable "tfor"
tfor: tfor.c
	@echo "Compilando tfor.c..."
	@${COMPILER} ${CFLAGS} -o tfor tfor.c 2> errors.log
	@if [ ! -s errors.log ]; then \
		echo "Compilacion ok."; \
		if [ "$(COPY)" -eq "1" ]; then \
			echo "Copiando tfor.c a src..."; \
			cp tfor.c $(SRC_DIR); \
			echo "Copiando Makefile a src..."; \
			cp tfor.c $(SRC_BCK); \
			echo "Copiando Makefile a backup..."; \
			cp Makefile $(SRC_DIR); \
			echo "Copiando ejecutables a bin..."; \
			if [ -e tfor ]; then \
				cp tfor $(BIN_DIR); \
				echo "Executable tfor copied to $(BIN_DIR)"; \
			else \
				echo "Executable tfor not found."; \
			fi; \
			if [ -e tfor.exe ]; then \
				cp tfor.exe $(BIN_DIR); \
				echo "Executable tfor.exe copied to $(BIN_DIR)"; \
			else \
				echo "Executable tfor.exe not found."; \
			fi; \
		else \
			echo "Skipping file copy"; \
		fi; \
	else \
		echo "Errores encontrados durante la compilación:"; \
		cat errors.log; \
	fi
	@rm -f errors.log

# Regla para copiar archivos desde A_src
get:
	@if [ "$(file)" ]; then \
		echo "Copiando $(file) desde $(SRC_DIR) a $(CURDIR)..."; \
		cp $(SRC_DIR)/$(file) .; \
	else \
		echo "Por favor, especifica el archivo a copiar. Ejemplo: make get file=tfor.c"; \
	fi

# Regla para editar tfor.c
vi: tfor.c
	vi tfor.c

# Regla para hacer limpieza
clean:
	@echo "Haciendo limpieza ... "
	@rm -f *.exe *.log log *.err log? *.chr tool.sta tfor 

