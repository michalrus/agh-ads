C=g++ -Wall -O2 -c -o

all:
	mkdir -p obj
	${C} obj/main.o src/main.cc
	${C} obj/bit_file.o src/bit_file.cc

	g++ -Wall -O2 -o huffman \
		obj/bit_file.o \
		obj/main.o

	strip huffman
