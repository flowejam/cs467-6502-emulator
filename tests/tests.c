#include <assert.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "state6502.h"
#include "flags.h"
#include "helpers6502.h"
#include "opcodes6502.h"

// Run your tests for each opcode here

//Abraham's tests

void test_execute_0x58() {
    // Set up initial state
    Flags flags = {0};
    State6502 state_cpu;
    state_cpu.flgs = &flags;
    state_cpu.pc = 0x1000;
    state_cpu.memory = (uint8_t*)calloc(0x10000, 1); // Allocate 64KB of memory
    state_cpu.flgs->inter_disable_flag = 0x01; // Initial state with interrupt disable flag set

    // Execute opcode 0x58 (CLI)
    execute_0x58(&state_cpu);

    // Assert final state
    assert(state_cpu.flgs->inter_disable_flag == 0x00); // Interrupt disable flag should be cleared
    assert(state_cpu.pc == 0x1001); // Program counter should be incremented

    free(state_cpu.memory);
}

void test_execute_0x59() {
    // Set up initial state
    Flags flags = {0};
    State6502 state_cpu;
    state_cpu.flgs = &flags;
    state_cpu.pc = 0x1000;
    state_cpu.a = 0x55;
    state_cpu.y = 0x01;
    state_cpu.memory = (uint8_t*)calloc(0x10000, 1); // Allocate 64KB of memory
    state_cpu.memory[0x1001] = 0x00; // Low byte of address
    state_cpu.memory[0x1002] = 0x10; // High byte of address
    state_cpu.memory[0x1001 + 0x10 + state_cpu.y] = 0xAA; // Value at effective address

    // Execute opcode 0x59 (EOR Absolute Y)
    execute_0x59(&state_cpu);

    // Assert final state
    assert(state_cpu.a == (0x55 ^ 0xAA)); // Accumulator should be XORed with the value at effective address
    assert(state_cpu.flgs->zro_flag == 0x00); // Zero flag should be cleared
    assert(state_cpu.flgs->neg_flag == 0x01); // Negative flag should be set
    assert(state_cpu.pc == 0x1003); // Program counter should be incremented by 3

    free(state_cpu.memory);
}

void test_execute_0x5d() {
    // Set up initial state
    Flags flags = {0};
    State6502 state_cpu;
    state_cpu.flgs = &flags;
    state_cpu.pc = 0x1000;
    state_cpu.a = 0x55;
    state_cpu.x = 0x01;
    state_cpu.memory = (uint8_t*)calloc(0x10000, 1); // Allocate 64KB of memory
    state_cpu.memory[0x1001] = 0x00; // Low byte of address
    state_cpu.memory[0x1002] = 0x10; // High byte of address
    state_cpu.memory[0x1001 + 0x10 + state_cpu.x] = 0xAA; // Value at effective address

    // Execute opcode 0x5d (EOR Absolute X)
    execute_0x5d(&state_cpu);

    // Assert final state
    assert(state_cpu.a == (0x55 ^ 0xAA)); // Accumulator should be XORed with the value at effective address
    assert(state_cpu.flgs->zro_flag == 0x00); // Zero flag should be cleared
    assert(state_cpu.flgs->neg_flag == 0x01); // Negative flag should be set
    assert(state_cpu.pc == 0x1003); // Program counter should be incremented by 3

    free(state_cpu.memory);
}

void test_execute_0x68() {
    // Set up initial state
    Flags flags = {0};
    State6502 state_cpu;
    state_cpu.flgs = &flags;
    state_cpu.pc = 0x1000;
    state_cpu.sp = 0x01FF; // Stack pointer at 0x01FF
    state_cpu.memory = (uint8_t*)calloc(0x10000, 1); // Allocate 64KB of memory
    state_cpu.memory[0x01FF] = 0xAA; // Value to be pulled from the stack

    // Execute opcode 0x68 (PLA)
    execute_0x68(&state_cpu);

    // Assert final state
    assert(state_cpu.a == 0xAA); // Accumulator should hold the value from the stack
    assert(state_cpu.flgs->zro_flag == 0x00); // Zero flag should be cleared
    assert(state_cpu.flgs->neg_flag == 0x01); // Negative flag should be set
    assert(state_cpu.sp == 0x01FF + 1); // Stack pointer should be incremented
    assert(state_cpu.pc == 0x1001); // Program counter should be incremented

    free(state_cpu.memory);
}

int main() {
    test_execute_0x58();
    test_execute_0x59();
    test_execute_0x5d();
    test_execute_0x68();

    printf("All tests passed!\n");

    return 0;
}
