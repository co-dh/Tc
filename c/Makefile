# C shim libraries for tc
LEAN_HOME := $(shell ~/.elan/bin/lean --print-prefix)
CFLAGS := -g -fPIC -I$(LEAN_HOME)/include

all: libtermbox2.a libadbcshim.a libtermshim.a libkdbshim.a

# termbox2: use -std=c11 to avoid C23 sscanf redirect
libtermbox2.a: termbox2.c termbox2.h
	gcc -std=c11 -g -fPIC -c termbox2.c -o termbox2.o
	ar rcs $@ termbox2.o

# ADBC: core + DuckDB driver (replaces monolithic adbc_shim.c)
libadbcshim.a: adbc_core.o duckdb_driver.o
	ar rcs $@ $^

adbc_core.o: adbc_core.c
	$(CC) $(CFLAGS) -c $< -o $@

duckdb_driver.o: duckdb_driver.c
	$(CC) $(CFLAGS) -c $< -o $@

libtermshim.a: term_shim.o
	ar rcs $@ $<

libkdbshim.a: kdb_shim.o kdb/c.o
	ar rcs $@ $^

term_shim.o: term_shim.c termbox2.h
	$(CC) $(CFLAGS) -c $< -o $@

kdb_shim.o: kdb_shim.c kdb/k.h
	$(CC) $(CFLAGS) -I. -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *.a

.PHONY: all clean
