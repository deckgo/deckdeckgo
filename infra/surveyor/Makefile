PREFIX = /usr/lib/surveyor

surveyor.so: surveyor.c
	gcc -Wall -nostartfiles -fpic -shared surveyor.c -o surveyor.so -ldl -D_GNU_SOURCE

install: surveyor.so
	install -d $(PREFIX)/lib/
	install -m 644 surveyor.so $(PREFIX)/lib/
