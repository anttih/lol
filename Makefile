
.PHONY: test

all : .o
	csc *.o -o bin/lol

.o :
	csc -c *.scm

clean :
	rm *.o

test:
	@csi -qb test/*_test.scm
