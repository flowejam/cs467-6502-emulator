#include "opcodes6502.h"
#include "state6502.h"
#include "opcodes6502.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include "flags.h"
#include "helpers6502.h"

/**
 * 
 * Instructions:
 * From your implementation in the emulator.c file, add in your opcode functions that you want to test (simple copy/paste).
 * Remove the static keyword from each function. The opcodes.h file already has every opcode function declared to make this easier.
 * 
 * 
 */



// Abraham opcode functions
void execute_0x58(State6502* state) {
    // Opccode 0x58: CLI - Clear Interrupt Disable Flag
    fprintf(stdout, "Executing opcode 0x58: CLI\n");
    // clearing the interrupt disable flag
    state->flgs->inter_disable_flag = 0x00;
    // advance the program counter to the next instruction
    state->pc++;

}

void execute_0x59(State6502* state) {
    // Opccode 0x59: EOR - Exclusive OR Absolute Y
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#EOR
    
    fprintf(stdout, "Executing opcode 0x59: EOR\n");

    // Fetch the base adddress for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = base_addr + state->y;

    // Perform the EOR operation
    uint8_t value = state->memory[effective_addr];
    state->a = state->a ^ value;

    // Update the Zero flag (Z)
    state->flgs->zro_flag = (state->a == 0) ? 0x01 : 0x00;
    //Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (state->a & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;

}

void execute_0x5d(State6502* state) {
    // Opccode 0x5D: EOR - Exclusive OR Absolute X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#EOR
    
    fprintf(stdout, "Executing opcode 0x5D: EOR\n");

    // Fetch the base adddress for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = base_addr + state->x;

    // Perform the EOR operation
    uint8_t value = state->memory[effective_addr];
    state->a = state->a ^ value;

    // Update the Zero flag (Z)
    state->flgs->zro_flag = (state->a == 0) ? 0x01 : 0x00;
    //Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (state->a & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;

}

void execute_0x5e(State6502* state) {
    // Opccode 0x5E: LSR - Logical Shift Right Absolute X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#LSR
    
    fprintf(stdout, "Executing opcode 0x5E: LSR\n");

    // Fetch the base adddress for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = base_addr + state->x;

    // Perform the LSR operation
    uint8_t value = state->memory[effective_addr];
    // Update the carry flag (C) based on the least significant bit of the result
    state->flgs->crry_flag = (value & 0x01) ? 0x01 : 0x00;
    // Shift the value to the right by 1
    value = value >> 1;
    // Update the Zero flag (Z)
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[effective_addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;

}

void execute_0x60(State6502* state) {
    // Opccode 0x60: RTS - Return from Subroutine
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#RTS
    
    fprintf(stdout, "Executing opcode 0x60: RTS\n");

    // Pop the program counter from the stack
    unsigned char pc_bytes[2];
    int size = sizeof(pc_bytes);
    int pop_result = pop_stack(state, size, pc_bytes);
    if (pop_result < 0) {
        // error in call to pop_stack
        return;
    }
    state->pc = pc_bytes[0] | (pc_bytes[1] << 8);

    // Increment the program counter to the next instruction
    state->pc++;

}

void execute_0x61(State6502* state) {
    // Opccode 0x61: ADC - Add with Carry Indirect X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
    fprintf(stdout, "Executing opcode 0x61: ADC\n");
    
    // Fetch the base address for the next byte
    uint16_t base_addr = state->memory[state->pc + 1];
    state->pc++;

    // Calculate the effective address
    uint16_t effective_addr = base_addr + state->x;

    // Fetch the low byte of the effective address
    uint16_t low_byte = state->memory[effective_addr];
    // Fetch the high byte of the effective address
    uint16_t high_byte = state->memory[effective_addr + 1];
    // Combine the low and high bytes to get the final address
    uint16_t final_addr = low_byte | (high_byte << 8);

    // Fetch the value at the final address
    uint8_t value = state->memory[final_addr];

    // Perform the ADC operation
    uint16_t result = state->a + value + state->flgs->crry_flag;
    // Update the carry flag (C) based on the most significant bit of the result
    state->flgs->crry_flag = (result > 0xFF) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the overflow flag (V) based on the result
    state->flgs->of_flag = ( (state->a ^ result) & (value ^ result) & 0x80) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Store the result in the accumulator
    state->a = result & 0xFF;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x65(State6502* state) {
    // Opccode 0x65: ADC - Add with Carry Zero Page
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
    fprintf(stdout, "Executing opcode 0x65: ADC\n");

    // Fetch the value to be added
    uint8_t value = state->memory[state->pc + 1];
    state->pc++;

    // Perform the ADC operation
    uint16_t result = state->a + value + state->flgs->crry_flag;
    // Update the carry flag (C) based on the most significant bit of the result
    state->flgs->crry_flag = (result > 0xFF) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the overflow flag (V) based on the result
    state->flgs->of_flag = ( (state->a ^ result) & (value ^ result) & 0x80) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Store the result in the accumulator
    state->a = result & 0xFF;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x66(State6502* state) {
    // Opccode 0x66: ROR - Rotate Right Zero Page
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ROR
    fprintf(stdout, "Executing opcode 0x66: ROR\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1];
    state->pc++;

    // Perform the ROR operation
    uint8_t value = state->memory[addr];
    // Calculate the new carry flag (C) based on the least significant bit of the value
    state->flgs->crry_flag = (value & 0x01) ? 0x01 : 0x00;
    // Shift the value to the right by 1
    value = value >> 1;
    // Update the most significant bit of the value based on the old carry flag
    value = value | (state->flgs->crry_flag << 7);
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x68(State6502* state) {
    // Opccode 0x68: PLA - Pull Accumulator
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#PLA
    fprintf(stdout, "Executing opcode 0x68: PLA\n");

    // Pop the value from the stack
    uint8_t value;
    int size = sizeof(value);
    int pop_result = pop_stack(state, size, &value);
    if (pop_result < 0) {
        // error in call to pop_stack
        return;
    }

    // Store the value in the accumulator
    state->a = value;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (state->a == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (state->a & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x69(State6502* state) {
    // Opccode 0x69: ADC - Add with Carry Immediate
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
    fprintf(stdout, "Executing opcode 0x69: ADC\n");

    // Fetch the value to be added
    uint8_t value = state->memory[state->pc + 1];
    state->pc++;

    // Perform the ADC operation
    uint16_t result = state->a + value + state->flgs->crry_flag;
    // Update the carry flag (C) based on the most significant bit of the result
    state->flgs->crry_flag = (result > 0xFF) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the overflow flag (V) based on the result
    state->flgs->of_flag = ( (state->a ^ result) & (value ^ result) & 0x80) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Store the result in the accumulator
    state->a = result & 0xFF;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x6a(State6502* state) {
    // Opccode 0x6A: ROR - Rotate Right Accumulator
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ROR
    fprintf(stdout, "Executing opcode 0x6A: ROR\n");

    // Perform the ROR operation
    // Calculate the new carry flag (C) based on the least significant bit of the value
    state->flgs->crry_flag = (state->a & 0x01) ? 0x01 : 0x00;
    // Shift the value to the right by 1
    state->a = state->a >> 1;
    // Update the most significant bit of the value based on the old carry flag
    state->a = state->a | (state->flgs->crry_flag << 7);
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (state->a == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (state->a & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x6c(State6502* state) {
    // Opccode 0x6C: JMP - Jump Indirect
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
    fprintf(stdout, "Executing opcode 0x6C: JMP\n");

    // Fetch the address for the next two bytes
    uint16_t addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Jump to the new address
    state->pc = addr;

}

void execute_0x6d(State6502* state) {
    // Opccode 0x6D: ADC - Add with Carry Absolute
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
    fprintf(stdout, "Executing opcode 0x6D: ADC\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Fetch the value at the base address
    uint8_t value = state->memory[base_addr];

    // Perform the ADC operation
    uint16_t result = state->a + value + state->flgs->crry_flag;
    // Update the carry flag (C) based on the most significant bit of the result
    state->flgs->crry_flag = (result > 0xFF) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the overflow flag (V) based on the result
    state->flgs->of_flag = ( (state->a ^ result) & (value ^ result) & 0x80) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Store the result in the accumulator
    state->a = result & 0xFF;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x6e(State6502* state) {
    // Opccode 0x6E: ROR - Rotate Right Absolute
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ROR
    fprintf(stdout, "Executing opcode 0x6E: ROR\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Fetch the value at the base address
    uint8_t value = state->memory[base_addr];
    // Perform the ROR operation
    // Calculate the new carry flag (C) based on the least significant bit of the value
    state->flgs->crry_flag = (value & 0x01) ? 0x01 : 0x00;
    // Shift the value to the right by 1
    value = value >> 1;
    // Update the most significant bit of the value based on the old carry flag
    value = value | (state->flgs->crry_flag << 7);
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[base_addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x70(State6502* state) {
    // Opccode 0x70: BVS - Branch if Overflow Set
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#BVS
    fprintf(stdout, "Executing opcode 0x70: BVS\n");

    // Fetch the value for the next byte
    int8_t value = state->memory[state->pc + 1];
    state->pc++;

    // Check if the overflow flag (V) is set
    if (state->flgs->of_flag == 0x01) {
        // Branch to the new address
        state->pc += value;
    }

    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x71(State6502* state) {
    // Opccode 0x71: ADC - Add with Carry Indirect Y
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
    fprintf(stdout, "Executing opcode 0x71: ADC\n");

    // Fetch the base address for the next byte
    uint16_t base_addr = state->memory[state->pc + 1];
    state->pc++;

    // Calculate the effective address
    uint16_t effective_addr = base_addr + state->y;

    // Fetch the low byte of the effective address
    uint16_t low_byte = state->memory[effective_addr];
    // Fetch the high byte of the effective address
    uint16_t high_byte = state->memory[effective_addr + 1];
    // Combine the low and high bytes to get the final address
    uint16_t final_addr = low_byte | (high_byte << 8);

    // Fetch the value at the final address
    uint8_t value = state->memory[final_addr];

    // Perform the ADC operation
    uint16_t result = state->a + value + state->flgs->crry_flag;
    // Update the carry flag (C) based on the most significant bit of the result
    state->flgs->crry_flag = (result > 0xFF) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the overflow flag (V) based on the result
    state->flgs->of_flag = ( (state->a ^ result) & (value ^ result) & 0x80) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Store the result in the accumulator
    state->a = result & 0xFF;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x75(State6502* state) {
    // Opccode 0x75: ADC - Add with Carry Zero Page X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
    fprintf(stdout, "Executing opcode 0x75: ADC\n");

    // Fetch the value to be added
    uint8_t value = state->memory[state->pc + 1] + state->x;
    state->pc++;

    // Perform the ADC operation
    uint16_t result = state->a + value + state->flgs->crry_flag;
    // Update the carry flag (C) based on the most significant bit of the result
    state->flgs->crry_flag = (result > 0xFF) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the overflow flag (V) based on the result
    state->flgs->of_flag = ( (state->a ^ result) & (value ^ result) & 0x80) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Store the result in the accumulator
    state->a = result & 0xFF;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x76(State6502* state) {
    // Opccode 0x76: ROR - Rotate Right Zero Page X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ROR
    fprintf(stdout, "Executing opcode 0x76: ROR\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1] + state->x;
    state->pc++;

    // Perform the ROR operation
    uint8_t value = state->memory[addr];
    // Calculate the new carry flag (C) based on the least significant bit of the value
    state->flgs->crry_flag = (value & 0x01) ? 0x01 : 0x00;
    // Shift the value to the right by 1
    value = value >> 1;
    // Update the most significant bit of the value based on the old carry flag
    value = value | (state->flgs->crry_flag << 7);
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x78(State6502* state) {
    // Opccode 0x78: SEI - Set Interrupt Disable Flag
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SEI
    fprintf(stdout, "Executing opcode 0x78: SEI\n");

    // Set the interrupt disable flag
    state->flgs->inter_disable_flag = 0x01;
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x79(State6502* state) {
    // Opccode 0x79: ADC - Add with Carry Absolute Y
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
    fprintf(stdout, "Executing opcode 0x79: ADC\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = base_addr + state->y;

    // Fetch the value at the effective address
    uint8_t value = state->memory[effective_addr];

    // Perform the ADC operation
    uint16_t result = state->a + value + state->flgs->crry_flag;
    // Update the carry flag (C) based on the most significant bit of the result
    state->flgs->crry_flag = (result > 0xFF) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the overflow flag (V) based on the result
    state->flgs->of_flag = ( (state->a ^ result) & (value ^ result) & 0x80) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Store the result in the accumulator
    state->a = result & 0xFF;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x7d(State6502* state) {
    // Opccode 0x7D: ADC - Add with Carry Absolute X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
    fprintf(stdout, "Executing opcode 0x7D: ADC\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = base_addr + state->x;

    // Fetch the value at the effective address
    uint8_t value = state->memory[effective_addr];

    // Perform the ADC operation
    uint16_t result = state->a + value + state->flgs->crry_flag;
    // Update the carry flag (C) based on the most significant bit of the result
    state->flgs->crry_flag = (result > 0xFF) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the overflow flag (V) based on the result
    state->flgs->of_flag = ( (state->a ^ result) & (value ^ result) & 0x80) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Store the result in the accumulator
    state->a = result & 0xFF;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x7e(State6502* state) {
    // Opccode 0x7E: ROR - Rotate Right Absolute X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ROR
    fprintf(stdout, "Executing opcode 0x7E: ROR\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = base_addr + state->x;

    // Fetch the value at the effective address
    uint8_t value = state->memory[effective_addr];
    // Perform the ROR operation
    // Calculate the new carry flag (C) based on the least significant bit of the value
    state->flgs->crry_flag = (value & 0x01) ? 0x01 : 0x00;
    // Shift the value to the right by 1
    value = value >> 1;
    // Update the most significant bit of the value based on the old carry flag
    value = value | (state->flgs->crry_flag << 7);
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[effective_addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x81(State6502* state) {
    // Opccode 0x81: STA - Store Accumulator Indirect X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#STA
    fprintf(stdout, "Executing opcode 0x81: STA\n");

    // Fetch the base address for the next byte
    uint16_t base_addr = state->memory[state->pc + 1];
    state->pc++;

    // Calculate the effective address
    uint16_t effective_addr = base_addr + state->x;

    // Store the value in the accumulator at the effective address
    state->memory[effective_addr] = state->a;

    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x84(State6502* state) {
	// TODO
}

void execute_0x85(State6502* state) {
    // Opccode 0x85: STA - Store Accumulator Zero Page
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#STA
    fprintf(stdout, "Executing opcode 0x85: STA\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1];
    state->pc++;

    // Store the value in the accumulator at the address
    state->memory[addr] = state->a;

    // advance the program counter to the next instruction
    state->pc++;
}

void execute_0x86(State6502* state) {
    // Opccode 0x86: STX - Store X Register Zero Page
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#STX
    fprintf(stdout, "Executing opcode 0x86: STX\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1];
    state->pc++;

    // Store the value in the X register at the address
    state->memory[addr] = state->x;

    // advance the program counter to the next instruction
    state->pc++;
}
// end of Abraham's opcodes up to 0x86

// ================== end of opcode functions ===============================