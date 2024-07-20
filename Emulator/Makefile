FLAGS := -std=c99
FLAGS += -g
FLAGS += -pedantic-errors
FLAGS += -Wall
FLAGS += -Wextra
FLAGS += -Werror=vla
#FLAGS += -Werror
FLAGS += -O0

COMM := gcc $(FLAGS)

OBJ_FILES := emulator.o

emulator: $(OBJ_FILES)
	$(COMM) -o emulator $(OBJ_FILES)

emulator.o: emulator.c
	$(COMM) -c emulator.c

.PHONY: clean
clean:
	rm -i *.o
	rm -i emulator
