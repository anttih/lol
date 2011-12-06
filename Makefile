
.PHONY: test

objects = eval.o read.o pretty.o repl.o lol.o

all : $(objects)
	csc *.o -o bin/lol

$(objects) :
	csc -c *.scm

clean :
	rm *.o
	rm bin/lol

test:
	@csi -qb test/*_test.scm
