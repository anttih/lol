DEPS="eval.o parser-combinators.o read.o pretty.o std.o repl.o lol.o"
redo-ifchange $DEPS
csc $DEPS -o bin/lol
