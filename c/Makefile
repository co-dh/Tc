# C shim libraries for tv
LEAN_HOME := $(shell ~/.elan/bin/lean --print-prefix)
CFLAGS := -g -fPIC -I$(LEAN_HOME)/include

all: libadbcshim.a libtermshim.a

libadbcshim.a: adbc_shim.o
	ar rcs $@ $<

libtermshim.a: term_shim.o
	ar rcs $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *.a

# Force clean rebuild of C libs + Lean project
rebuild: clean all
	cd .. && ~/.elan/bin/lake clean && ~/.elan/bin/lake build tv

.PHONY: all clean rebuild
