CC=gcc
CFLAGS=-I. -Wall

# Object files for the emulator functionality
OBJS=helpers6502.o opcodes6502.o tests.o

# Target executable name for tests
TEST_TARGET=test_emulator

# Default target
all: $(TEST_TARGET)

# Linking the test executable
$(TEST_TARGET): $(OBJS)
	$(CC) -o $(TEST_TARGET) $(OBJS)

# Compiling each source file to object file
helpers6502.o: helpers6502.c helpers6502.h
	$(CC) -c helpers6502.c $(CFLAGS)

opcodes6502.o: opcodes6502.c opcodes6502.h
	$(CC) -c opcodes6502.c $(CFLAGS)

tests.o: tests.c
	$(CC) -c tests.c $(CFLAGS)

# Clean up the build
clean:
	rm -f $(OBJS) $(TEST_TARGET)

# Run the tests
run: $(TEST_TARGET)
	./$(TEST_TARGET)

