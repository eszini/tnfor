# Definimos el objetivo "all" para construir el ejecutable
all:	tfor


# Regla para construir el ejecutable "tfor"
tfor:	tfor.c
	@echo "Compilando tfor.c..."
	@gcc -o tfor tfor.c 2> errors.log
	@if [ ! -s errors.log ]; then \
		echo "Compilación ok. "; \
		echo "copiando tfor.c a src..."; \
		echo "copiando Makefile a src..."; \
		echo "copiando tfor.exe a bin..."; \
		cp tfor.c ~/wrk/tnfor/A_src; \
		cp Makefile ~/wrk/tnfor/A_src; \
		cp tfor.exe ~/bin; \
	else \
		echo "Errores encontrados durante la compilación:"; \
		cat errors.log; \
	fi
	@rm -f errors.log

# Regla para editar tfor.c
vi:	tfor.c
	vi tfor.c

# Regla para hace limpieza 
clean:	
	@echo "Haciendo limpieza ... "
	@rm -f *.exe *.log log *.err
