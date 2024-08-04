#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>



// TODO: let's consider adding/extern-ing some of the structs & helper functions to a 
// header file to enable having tests in a separate file. 

typedef struct Flags {
	// These flags should be set to either 0x01 or 0x00.
	// 6502 has these flags:
	// carry
	uint8_t crry_flag;
	// zero
	uint8_t zro_flag;
	// interrupt disable
	uint8_t inter_disable_flag;
	// decimal mode (I don't think this is used in the NES though? Need to check
	uint8_t dec_flag;
	// break command
	uint8_t brk_flag;
	// overflow flag
	uint8_t of_flag;
	// negative flag
	uint8_t neg_flag;
} Flags;



typedef struct State6502 {
    uint8_t a;
    uint8_t x;
    uint8_t y;

	// Refer to https://www.nesdev.org/wiki/Status_flags for details of the 
	// architectural 'P' register, which we refer to here as 'processor_status'.
	// The processor_status register is updated with the update_processor_status
	// helper function.
	// This special status register holds the various flag bits: 
	// Bit #:
	// 0) crry_flag
	// 1) zro_flag
	// 2) inter_disable_flag
	// 3) dec_flag
	// 4) brk_flag
	// 5) (no CPU effect, always pushed as 1)
	// 6) of_flag
	// 7) neg_flag
	uint8_t processor_status;
	
	// The stack pointer holds the lower 8 bits of the next free location on 
	// the stack. This works because the stack is 256 bytes. Note that the
	// stack pointer should be decremented on push, not incremented. See 
	// https://www.nesdev.org/obelisk-6502-guide/registers.html for details.
    uint8_t sp;
    uint16_t pc;
    uint8_t* memory;
    struct Flags* flgs;

	// This is not part of the 6502 CPU. It is just a flag used to exit the program. 
	bool exit_prog;
} State6502;



// ================== helper functions ======================================

/*
 * This helper function is used before pushing the processor state register
 * to the stack. Don't use this helper for any other purpose. E.g. for updating 
 * the flags, they should be set/cleared as follows:
 * state->flgs->crry_flag = 0x01;
 */
static void update_processor_status(State6502* state) {
	// As per https://www.nesdev.org/wiki/Status_flags, bit #5 is always set.
	// It 'has no CPU effect'.
	state->processor_status = 0x20;

	state->processor_status |= ( (state->flgs->crry_flag) << 0);
	state->processor_status |= ( (state->flgs->zro_flag) << 1);
	state->processor_status |= ( (state->flgs->inter_disable_flag) << 2);
	state->processor_status |= ( (state->flgs->dec_flag) << 3);
	state->processor_status |= ( (state->flgs->brk_flag) << 4);
	state->processor_status |= ( (state->flgs->of_flag) << 6);
	state->processor_status |= ( (state->flgs->neg_flag) << 7);
}

/*
 * Assumes the MSB is pushed first, so that the LSB is popped first. The bytes
 * stored in the stack will then be in little endian order.
 * This is confirmed in https://en.wikipedia.org/wiki/Interrupts_in_65xx_processors.
 * 'size' argument should always be 2:
 * by default, we should only push a word (16 bits) at a time, even
 * if only 1 byte needs to be pushed.
 */
static int push_stack(State6502* state, int size, unsigned char* byte_arr) {
	uint16_t stack_addr = state->sp | 0x0100;

	for (int i = 0; i < size; ++i) {
		// Note that the 6502 doesn't detect stack overflow. I have implemented
		// it here for debugging purposes.
		if (stack_addr < 0x0100) {
			fprintf(stderr, "Error in func push_stack: stack overflow!\n");
			state->exit_prog = true;
			return -1;
		}

		state->memory[stack_addr] = byte_arr[i];
		--stack_addr;
	}	
	state->sp = stack_addr & 0x00FF;
	return 0;
}

/*
 * Assumes the MSB is pushed first, so that the LSB is popped first. The bytes
 * stored in the stack will then be in little endian order.
 * This is confirmed in https://en.wikipedia.org/wiki/Interrupts_in_65xx_processors.
 * 'size' argument should always be 2:
 * by default, we should only pop a word (16 bits) at a time, even
 * if only 1 byte needs to be popped.
 */
static int pop_stack(State6502* state, int size, unsigned char* byte_arr) {
	uint16_t stack_addr = state->sp | 0x0100;

	for (int i = 0; i < size; ++i) {
		// Note that the 6502 doesn't detect stack underflow. I have implemented
		// it here for debugging purposes.
		if (stack_addr > 0x01FF) {
			fprintf(stderr, "Error in func push_stack: stack underflow!\n");
			state->exit_prog = true;
			return -1;
		}
		byte_arr[i] = state->memory[stack_addr];
		++stack_addr;
	}
	state->sp = stack_addr & 0x00FF;
	return 0;
}

// ================== end of helper functions ===============================


// ================== opcode functions ======================================

// James opcode functions
static void execute_0x00(State6502* state) {
	// more details on the BRK instruction can be found here: 
	// http://www.6502.org/tutorials/interrupts.html#2.2
	
	fprintf(stdout, "Executing opcode 0x00: BRK\n");
	
	// push the program counter to the stack
	// according to https://en.wikipedia.org/wiki/Interrupts_in_65xx_processors,
	// 2 is added to the program counter prior to pushing to the stack.
	// little endian - addresses stored in memory with LSB first
	uint16_t tmp_pc = state->pc + 2;
	unsigned char pc_bytes[] = {tmp_pc >> 8, tmp_pc & 0x00FF};
	int size = sizeof(pc_bytes);
	int push_result = push_stack(state, size, pc_bytes);
	if (push_result < 0) {
		// error in call to push_stack
		return;
	}

	// push the processor status to the stack
	update_processor_status(state);
	// size is 2 because by default a full word (16 bits) should be pushed at
	// a time.
	size = 2;
	unsigned char processor_status_bytes[] = {0x00, state->processor_status};
	push_result = push_stack(state, size, processor_status_bytes); 
	if (push_result < 0) {
		// error in call to push_stack
		return;
	}

	// load the BRK/IRQ vector into the program counter.
	// I don't see anything re. an initial value for 0xFFFE-0xFFFF on power up:
	// https://www.nesdev.org/wiki/CPU_power_up_state
	state->pc = (state->memory[0xFFFF] << 8) | (state->memory[0xFFFE]);

	// set the break flag
	state->flgs->brk_flag = 0x01;

	// also set the interrupt disable flag as per 
	// http://www.6502.org/tutorials/interrupts.html#2.2
	state->flgs->inter_disable_flag = 0x01;

	// TODO: re-examine this
	// We made a decision to exit the program if an interrupt is encountered.
	state->exit_prog = true;
	
}

static void execute_0x01(State6502* state) {
	fprintf(stdout, "Executing opcode 0x01: ORA - (indirect, X)\n");
	state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];

	// As per http://www.emulator101.com/6502-addressing-modes.html,
	// there is wrap around to an address on the 'zero page'.
	uint16_t addr_of_addr = (state->x + zero_page_addr) & 0x00FF;
	unsigned char addr_bytes[] = {state->memory[addr_of_addr], state->memory[++addr_of_addr]}; 

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];

	unsigned char byte_to_or = state->memory[addr];

	// inclusive OR on accumulator contents.
	state->a |= byte_to_or;

	// set zero flag if applicable.
	if (state->a == 0x00) {
		state->flgs->zro_flag = 1;
	}

	// set negative flag if bit 7 is set.
	if ((state->a & 0x80) == 0x80) {
		state->flgs->neg_flag = 1;
	}
}
static void execute_0x05(State6502* state) {
	fprintf(stdout, "Executing opcode 0x05: ORA - zero page\n");
	state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char byte_to_or = state->memory[zero_page_addr];

	// inclusive OR on accumulator contents.
	state->a |= byte_to_or;

	// set zero flag if applicable.
	if (state->a == 0x00) {
		state->flgs->zro_flag = 1;
	}

	// set negative flag if bit 7 is set.
	if ((state->a & 0x80) == 0x80) {
		state->flgs->neg_flag = 1;
	}
}

static void execute_0x06(State6502* state) {
	fprintf(stdout, "Executing opcode 0x06: ASL - zero page\n");
	state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char selected_byte = state->memory[zero_page_addr];
	unsigned char op_result = (selected_byte << 1);
	uint8_t old_bit7 = (selected_byte & 0x80) == 0x80 ? 0x01 : 0x00;
	state->memory[zero_page_addr] = op_result;

	state->flgs->crry_flag = old_bit7;
	
	// The author of the guide here (https://www.nesdev.org/obelisk-6502-guide/reference.html#ASL)
	// indicated that the zero flag should be set if the result of the instruction 
	// applied to its operand is zero, not (as it states in the guide) the 
	// accumulator. 
	// See the author's comment here: http://forum.6502.org/viewtopic.php?f=12&t=5351
	if (op_result == 0x00) {
		state->flgs->zro_flag = 0x01;
	}
}

static void execute_0x08(State6502* state) {
	// This opcode just pushes the processer_status register to the stack.
	fprintf(stdout, "Executing opcode 0x08: PHP - Implied\n");
	update_processor_status(state);
	int size = 2;
	unsigned char processor_status_bytes[] = {0x00, state->processor_status};
	int push_result = push_stack(state, size, processor_status_bytes); 
	if (push_result < 0) {
		// error in call to push_stack
		return;
	}
}

static void execute_0x09(State6502* state) {
	// TODO
	fprintf(stdout, "Executing opcode 0x09: ORA - Immediate\n");
	state->pc++;
	unsigned char byte_to_or = state->memory[state->pc];

	// inclusive OR on accumulator contents.
	state->a |= byte_to_or;

	// set zero flag if applicable.
	if (state->a == 0x00) {
		state->flgs->zro_flag = 1;
	}

	// set negative flag if bit 7 is set.
	if ((state->a & 0x80) == 0x80) {
		state->flgs->neg_flag = 1;
	}
}

// Chris' opcode functions
static void execute_0x2c(State6502* state) {
	// TODO
}

// Abraham opcode functions
static void execute_0x58(State6502* state) {
    // Opccode 0x58: CLI - Clear Interrupt Disable Flag
    fprintf(stdout, "Executing opcode 0x58: CLI\n");
    // clearing the interrupt disable flag
    state->flgs->inter_disable_flag = 0x00;

}

static void execute_0x59(State6502* state) {
    // Opccode 0x59: EOR - Exclusive OR Absolute Y
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#EOR
    
    fprintf(stdout, "Executing opcode 0x59: EOR\n");

    // Fetch the base adddress for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->y) & 0xFFFF;

    // Perform the EOR operation
    uint8_t value = state->memory[effective_addr];
    state->a = state->a ^ value;

    // Update the Zero flag (Z)
    state->flgs->zro_flag = (state->a == 0) ? 0x01 : 0x00;
    //Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (state->a & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);

}

static void execute_0x5d(State6502* state) {
    // Opccode 0x5D: EOR - Exclusive OR Absolute X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#EOR
    
    fprintf(stdout, "Executing opcode 0x5D: EOR\n");

    // Fetch the base adddress for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->x) & 0xFFFF;

    // Perform the EOR operation
    uint8_t value = state->memory[effective_addr];
    state->a = state->a ^ value;

    // Update the Zero flag (Z)
    state->flgs->zro_flag = (state->a == 0) ? 0x01 : 0x00;
    //Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (state->a & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);

}

static void execute_0x5e(State6502* state) {
    // Opccode 0x5E: LSR - Logical Shift Right Absolute X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#LSR
    
    fprintf(stdout, "Executing opcode 0x5E: LSR\n");

    // Fetch the base adddress for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->x) & 0xFFFF;

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
}

static void execute_0x60(State6502* state) {
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

}

static void execute_0x61(State6502* state) {
    // Opccode 0x61: ADC - Add with Carry Indirect X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
    fprintf(stdout, "Executing opcode 0x61: ADC\n");
    
    // Fetch the base address for the next byte
    uint16_t base_addr = state->memory[state->pc + 1];
    state->pc++;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->x) & 0xFF;

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
}

static void execute_0x65(State6502* state) {
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
}

static void execute_0x66(State6502* state) {
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
}

static void execute_0x68(State6502* state) {
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
}

static void execute_0x69(State6502* state) {
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
}

static void execute_0x6a(State6502* state) {
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

}

static void execute_0x6c(State6502* state) {
    // Opccode 0x6C: JMP - Jump Indirect
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
    fprintf(stdout, "Executing opcode 0x6C: JMP\n");

    // Fetch the address for the next two bytes
    uint16_t addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Jump to the new address
    state->pc = addr;

}

static void execute_0x6d(State6502* state) {
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
}

static void execute_0x6e(State6502* state) {
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

}

static void execute_0x70(State6502* state) {
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

}

static void execute_0x71(State6502* state) {
    // Opccode 0x71: ADC - Add with Carry Indirect Y
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
    fprintf(stdout, "Executing opcode 0x71: ADC\n");

    // Fetch the base address for the next byte
    uint16_t base_addr = state->memory[state->pc + 1];
    state->pc++;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->y) & 0xFF;

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
}

static void execute_0x75(State6502* state) {
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
}

static void execute_0x76(State6502* state) {
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

}

static void execute_0x78(State6502* state) {
    // Opccode 0x78: SEI - Set Interrupt Disable Flag
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SEI
    fprintf(stdout, "Executing opcode 0x78: SEI\n");

    // Set the interrupt disable flag
    state->flgs->inter_disable_flag = 0x01;
}

static void execute_0x79(State6502* state) {
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

}

static void execute_0x7d(State6502* state) {
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

}

static void execute_0x7e(State6502* state) {
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

}

static void execute_0x81(State6502* state) {
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
}

static void execute_0x84(State6502* state) {
    // Opccode 0x84: STY - Store Y Register Zero Page
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#STY
    fprintf(stdout, "Executing opcode 0x84: STY\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1];
    state->pc++;

    // Store the value in the Y register at the address
    state->memory[addr] = state->y;
}

static void execute_0x85(State6502* state) {
    // Opccode 0x85: STA - Store Accumulator Zero Page
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#STA
    fprintf(stdout, "Executing opcode 0x85: STA\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1];
    state->pc++;

    // Store the value in the accumulator at the address
    state->memory[addr] = state->a;

}

static void execute_0x86(State6502* state) {
    // Opccode 0x86: STX - Store X Register Zero Page
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#STX
    fprintf(stdout, "Executing opcode 0x86: STX\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1];
    state->pc++;

    // Store the value in the X register at the address
    state->memory[addr] = state->x;

}
// end of Abraham's opcodes up to 0x86


// Abraham remaining opcodes
static void execute_0xd5(State6502* state) {
    // Opccode 0xD5: CMP - Compare Zero Page X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#CMP
    fprintf(stdout, "Executing opcode 0xD5: CMP\n");

    // Fetch the value to be compared
    uint8_t value = state->memory[state->pc + 1] + state->x;
    state->pc++;

    // Perform the CMP operation
    uint16_t result = state->a - value;
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);
}

static void execute_0xd6(State6502* state) {
    // Opccode 0xD6: DEC - Decrement Zero Page X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#DEC
    fprintf(stdout, "Executing opcode 0xD6: DEC\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1] + state->x;
    state->pc++;

    // Perform the DEC operation
    uint8_t value = state->memory[addr];
    value--;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
}

static void execute_0xd8(State6502* state) {
    // Opccode 0xD8: CLD - Clear Decimal Mode
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#CLD
    fprintf(stdout, "Executing opcode 0xD8: CLD\n");

    // Clear the decimal mode flag
    state->flgs->dec_flag = 0x00;
}

static void execute_0xd9(State6502* state) {
    // Opccode 0xD9: CMP - Compare Absolute Y
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#CMP
    fprintf(stdout, "Executing opcode 0xD9: CMP\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->y) & 0xFFFF;

    // Fetch the value at the effective address
    uint8_t value = state->memory[effective_addr];

    // Perform the CMP operation
    uint16_t result = state->a - value;
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);
}

static void execute_0xdd(State6502* state) {
    // Opccode 0xDD: CMP - Compare Absolute X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#CMP
    fprintf(stdout, "Executing opcode 0xDD: CMP\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->x) & 0xFFFF;

    // Fetch the value at the effective address
    uint8_t value = state->memory[effective_addr];

    // Perform the CMP operation
    uint16_t result = state->a - value;
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);
}

static void execute_0xde(State6502* state) {
    // Opccode 0xDE: DEC - Decrement Absolute X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#DEC
    fprintf(stdout, "Executing opcode 0xDE: DEC\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->x) & 0xFFFF;

    // Perform the DEC operation
    uint8_t value = state->memory[effective_addr];
    value--;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[effective_addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
}

static void execute_0xe0(State6502* state) {
    // Opccode 0xE0: CPX - Compare X Register Immediate
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#CPX
    fprintf(stdout, "Executing opcode 0xE0: CPX\n");

    // Fetch the value to be compared
    uint8_t value = state->memory[state->pc + 1];
    state->pc++;

    // Perform the CPX operation
    uint16_t result = state->x - value;
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->x >= value) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);
}

static void execute_0xe1(State6502* state) {
    // Opccode 0xE1: SBC - Subtract with Carry Indirect X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
    fprintf(stdout, "Executing opcode 0xE1: SBC\n");

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

    // Perform the SBC operation
    uint16_t result = state->a - value - (1 - state->flgs->crry_flag);
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
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
}

static void execute_0xe4(State6502* state) {
    // Opccode 0xE4: CPX - Compare X Register Zero Page
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#CPX
    fprintf(stdout, "Executing opcode 0xE4: CPX\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1];
    state->pc++;

    // Fetch the value to be compared
    uint8_t value = state->memory[addr];

    // Perform the CPX operation
    uint16_t result = state->x - value;
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->x >= value) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);
}

static void execute_0xe5(State6502* state) {
    // Opccode 0xE5: SBC - Subtract with Carry Zero Page
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
    fprintf(stdout, "Executing opcode 0xE5: SBC\n");

    // Fetch the value to be subtracted
    uint8_t value = state->memory[state->pc + 1];
    state->pc++;

    // Perform the SBC operation
    uint16_t result = state->a - value - (1 - state->flgs->crry_flag);
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
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
}

static void execute_0xe6(State6502* state) {
    // Opccode 0xE6: INC - Increment Zero Page
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#INC
    fprintf(stdout, "Executing opcode 0xE6: INC\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1];
    state->pc++;

    // Perform the INC operation
    uint8_t value = state->memory[addr];
    value++;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
}

static void execute_0xe8(State6502* state) {
    // Opccode 0xE8: INX - Increment X Register
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#INX
    fprintf(stdout, "Executing opcode 0xE8: INX\n");

    // Increment the X register
    state->x++;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (state->x == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (state->x & 0x80) ? 0x01 : 0x00;
}

static void execute_0xe9(State6502* state) {
    // Opccode 0xE9: SBC - Subtract with Carry Immediate
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
    fprintf(stdout, "Executing opcode 0xE9: SBC\n");

    // Fetch the value to be subtracted
    uint8_t value = state->memory[state->pc + 1];
    state->pc++;

    // Perform the SBC operation
    uint16_t result = state->a - value - (1 - state->flgs->crry_flag);
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
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
}

static void execute_0xea(State6502* state) {
    // Opccode 0xEA: NOP - No Operation
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#NOP
    fprintf(stdout, "Executing opcode 0xEA: NOP\n");

    // No operation is performed
}

static void execute_0xec(State6502* state) {
    // Opccode 0xEC: CPX - Compare X Register Absolute
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#CPX
    fprintf(stdout, "Executing opcode 0xEC: CPX\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Fetch the value to be compared
    uint8_t value = state->memory[base_addr];

    // Perform the CPX operation
    uint16_t result = state->x - value;
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->x >= value) ? 0x01 : 0x00;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (result == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (result & 0x80) ? 0x01 : 0x00;
    // Updating the processor status if needed
    update_processor_status(state);
}


static void execute_0xed(State6502* state) {
    // Opccode 0xED: SBC - Subtract with Carry Absolute
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
    fprintf(stdout, "Executing opcode 0xED: SBC\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Fetch the value to be subtracted
    uint8_t value = state->memory[base_addr];

    // Perform the SBC operation
    uint16_t result = state->a - value - (1 - state->flgs->crry_flag);
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
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
}

static void execute_0xee(State6502* state) {
    // Opccode 0xEE: INC - Increment Absolute
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#INC
    fprintf(stdout, "Executing opcode 0xEE: INC\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Perform the INC operation
    uint8_t value = state->memory[base_addr];
    value++;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[base_addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
}

static void execute_0xf0(State6502* state) {
    // Opccode 0xF0: BEQ - Branch if Equal
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#BEQ
    fprintf(stdout, "Executing opcode 0xF0: BEQ\n");

    // Fetch the offset for the next byte
    int8_t offset = state->memory[state->pc + 1];
    state->pc++;

    // Check if the zero flag (Z) is set
    if (state->flgs->zro_flag == 0x01) {
        // Update the program counter with the offset
        state->pc += offset;
    }
}

static void execute_0xf1(State6502* state) {
    // Opccode 0xF1: SBC - Subtract with Carry Indirect Y
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
    fprintf(stdout, "Executing opcode 0xF1: SBC\n");

    // Fetch the base address for the next byte
    uint16_t base_addr = state->memory[state->pc + 1];
    state->pc++;

    // Fetch the low byte of the base address
    uint16_t low_byte = state->memory[base_addr];
    // Fetch the high byte of the base address
    uint16_t high_byte = state->memory[base_addr + 1];
    // Combine the low and high bytes to get the final address
    uint16_t final_addr = low_byte | (high_byte << 8);

    // Fetch the value at the final address
    uint8_t value = state->memory[final_addr + state->y];

    // Perform the SBC operation
    uint16_t result = state->a - value - (1 - state->flgs->crry_flag);
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
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
}

static void execute_0xf5(State6502* state) {
    // Opccode 0xF5: SBC - Subtract with Carry Zero Page X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
    fprintf(stdout, "Executing opcode 0xF5: SBC\n");

    // Fetch the value to be subtracted
    uint8_t value = state->memory[state->pc + 1] + state->x;
    state->pc++;

    // Perform the SBC operation
    uint16_t result = state->a - value - (1 - state->flgs->crry_flag);
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
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
}


static void execute_0xf6(State6502* state) {
    // Opccode 0xF6: INC - Increment Zero Page X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#INC
    fprintf(stdout, "Executing opcode 0xF6: INC\n");

    // Fetch the address for the next byte
    uint16_t addr = state->memory[state->pc + 1] + state->x;
    state->pc++;

    // Perform the INC operation
    uint8_t value = state->memory[addr];
    value++;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
}

static void execute_0xf8(State6502* state) {
    // Opccode 0xF8: SED - Set Decimal Flag
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SED
    fprintf(stdout, "Executing opcode 0xF8: SED\n");

    // Set the decimal mode flag
    state->flgs->dec_flag = 0x01;
}

static void execute_0xf9(State6502* state) {
    // Opccode 0xF9: SBC - Subtract with Carry Absolute Y
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
    fprintf(stdout, "Executing opcode 0xF9: SBC\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->y) & 0xFFFF;

    // Fetch the value at the effective address
    uint8_t value = state->memory[effective_addr];

    // Perform the SBC operation
    uint16_t result = state->a - value - (1 - state->flgs->crry_flag);
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
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
}

static void execute_0xfa(State6502* state) {
    // Opccode 0xFA: NOP - No Operation
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#NOP
    fprintf(stdout, "Executing opcode 0xFA: NOP\n");

    // No operation is performed
}

static void execute_0xfd(State6502* state) {
    // Opccode 0xFD: SBC - Subtract with Carry Absolute X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
    fprintf(stdout, "Executing opcode 0xFD: SBC\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->x) & 0xFFFF;

    // Fetch the value at the effective address
    uint8_t value = state->memory[effective_addr];

    // Perform the SBC operation
    uint16_t result = state->a - value - (1 - state->flgs->crry_flag);
    // Update the carry flag (C) based on the result
    state->flgs->crry_flag = (state->a >= value) ? 0x01 : 0x00;
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
}

static void execute_0xfe(State6502* state) {
    // Opccode 0xFE: INC - Increment Absolute X
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#INC
    fprintf(stdout, "Executing opcode 0xFE: INC\n");

    // Fetch the base address for the next two bytes
    uint16_t base_addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Calculate the effective address
    uint16_t effective_addr = (base_addr + state->x) & 0xFFFF;

    // Perform the INC operation
    uint8_t value = state->memory[effective_addr];
    value++;
    // Update the zero flag (Z) based on the result
    state->flgs->zro_flag = (value == 0) ? 0x01 : 0x00;
    // Update the negative flag (N) based on the most significant bit of the result
    state->flgs->neg_flag = (value & 0x80) ? 0x01 : 0x00;
    // Store the result back in memory
    state->memory[effective_addr] = value;
    // Updating the processor status if needed
    update_processor_status(state);
}



// ================== end of opcode functions ===============================

int Emulate(State6502* state) {
    uint8_t* opcode = &state->memory[state->pc];

    switch (*opcode)
    {
		// this is a good reference for opcodes: 
		// https://www.nesdev.org/obelisk-6502-guide/reference.html
		// Note that emulator101 is incorrect about some flags that are set (e.g.,
		// opcode 0x00 or BRK is supposed to set the B flag but it doesn't mention
		// this), so use with caution.
		
        // James implementation
        case 0x00: 
			// Note: until other opcodes have been properly implemented, 0x00
			// read from what should be part of an immediate value or address
			// etc. will be interpreted as BRK (opcode 0). 
			execute_0x00(state);
			break;
        case 0x01: 
			execute_0x01(state);
			break;
        case 0x05: 
			execute_0x05(state);
			break;
        case 0x06: 
			execute_0x06(state);
			break;
        case 0x08: 
			execute_0x08(state);
			break;
        case 0x09: 
			execute_0x09(state);
			break;
        case 0x0a: 
			printf("Not yet implemented\n"); 
			break;
        case 0x0d: printf("Not yet implemented\n"); break;
        case 0x0e: printf("Not yet implemented\n"); break;
        case 0x10: printf("Not yet implemented\n"); break;
        case 0x11: printf("Not yet implemented\n"); break;
        case 0x15: printf("Not yet implemented\n"); break;
        case 0x16: printf("Not yet implemented\n"); break;
        case 0x18: printf("Not yet implemented\n"); break;
        case 0x19: printf("Not yet implemented\n"); break;
        case 0x1d: printf("Not yet implemented\n"); break;
        case 0x1e: printf("Not yet implemented\n"); break;
        case 0x20: printf("Not yet implemented\n"); break;
        case 0x21: printf("Not yet implemented\n"); break;
        case 0x24: printf("Not yet implemented\n"); break;
        case 0x25: printf("Not yet implemented\n"); break;
        case 0x26: printf("Not yet implemented\n"); break;
        case 0x28: printf("Not yet implemented\n"); break;
        case 0x29: printf("Not yet implemented\n"); break;
        case 0x2a: printf("Not yet implemented\n"); break;

        // Chris implementation
        case 0x2c:
            execute_0x2c(state);
            break;
        case 0x2d: printf("Not yet implemented\n"); break;
        case 0x2e: printf("Not yet implemented\n"); break;
        case 0x30: printf("Not yet implemented\n"); break;
        case 0x31: printf("Not yet implemented\n"); break;
        case 0x35: printf("Not yet implemented\n"); break;
        case 0x36: printf("Not yet implemented\n"); break;
        case 0x38: printf("Not yet implemented\n"); break;
        case 0x39: printf("Not yet implemented\n"); break;
        case 0x3d: printf("Not yet implemented\n"); break;
        case 0x3e: printf("Not yet implemented\n"); break;
        case 0x40: printf("Not yet implemented\n"); break;
        case 0x41: printf("Not yet implemented\n"); break;
        case 0x45: printf("Not yet implemented\n"); break;
        case 0x46: printf("Not yet implemented\n"); break;
        case 0x48: printf("Not yet implemented\n"); break;
        case 0x49: printf("Not yet implemented\n"); break;
        case 0x4a: printf("Not yet implemented\n"); break;
        case 0x4c: printf("Not yet implemented\n"); break;
        case 0x4d: printf("Not yet implemented\n"); break;
        case 0x4e: printf("Not yet implemented\n"); break;
        case 0x50: printf("Not yet implemented\n"); break;
        case 0x51: printf("Not yet implemented\n"); break;
        case 0x55: printf("Not yet implemented\n"); break;
        case 0x56: printf("Not yet implemented\n"); break;

        // Abraham implementation
        case 0x58:
            execute_0x58(state);
            break;
        case 0x59: 
            execute_0x59(state);
            break;
        case 0x5d: 
            execute_0x5d(state);
            break;
        case 0x5e: 
            execute_0x5e(state);
            break;
        case 0x60:
            execute_0x60(state); 
            break;
        case 0x61:
            execute_0x61(state);
            break;
        case 0x65:
            execute_0x65(state); 
            break;
        case 0x66:
            execute_0x66(state);
            break;
        case 0x68:
            execute_0x68(state);
            break;
        case 0x69:
            execute_0x69(state);
            break;
        case 0x6a:
            execute_0x6a(state);
            break;
        case 0x6c:
            execute_0x6c(state);
            break;
        case 0x6d:
            execute_0x6d(state);
            break;
        case 0x6e:
            execute_0x6e(state);
            break;
        case 0x70:
            execute_0x70(state);
            break;
        case 0x71:
            execute_0x71(state);
            break;
        case 0x75:
            execute_0x75(state);
            break;
        case 0x76:
            execute_0x76(state);
            break;
        case 0x78:
            execute_0x78(state);
            break;
        case 0x79:
            execute_0x79(state);
            break;
        case 0x7d:
            execute_0x7d(state);
            break;
        case 0x7e:
            execute_0x7e(state);
            break;
        case 0x81:
            execute_0x81(state);
            break;
        case 0x84:
            execute_0x84(state);
            break;
        case 0x85:
            execute_0x85(state);
            break;
        
        // James implementation
        case 0x86: printf("Not yet implemented\n"); break;
        case 0x88: printf("Not yet implemented\n"); break;
        case 0x8a: printf("Not yet implemented\n"); break;
        case 0x8c: printf("Not yet implemented\n"); break;
        case 0x8d: printf("Not yet implemented\n"); break;
        case 0x8e: printf("Not yet implemented\n"); break;
        case 0x90: printf("Not yet implemented\n"); break;
        case 0x91: printf("Not yet implemented\n"); break;
        case 0x94: printf("Not yet implemented\n"); break;
        case 0x95: printf("Not yet implemented\n"); break;
        case 0x96: printf("Not yet implemented\n"); break;
        case 0x98: printf("Not yet implemented\n"); break;
        case 0x99: printf("Not yet implemented\n"); break;
        case 0x9a: printf("Not yet implemented\n"); break;
        case 0x9d: printf("Not yet implemented\n"); break;
        case 0xa0: printf("Not yet implemented\n"); break;
        case 0xa1: printf("Not yet implemented\n"); break;
        case 0xa2: printf("Not yet implemented\n"); break;
        case 0xa4: printf("Not yet implemented\n"); break;
        case 0xa5: printf("Not yet implemented\n"); break;
        case 0xa6: printf("Not yet implemented\n"); break;
        case 0xa8: printf("Not yet implemented\n"); break;
        case 0xa9: printf("Not yet implemented\n"); break;
        case 0xaa: printf("Not yet implemented\n"); break;
        case 0xac: printf("Not yet implemented\n"); break;

        // Chris implementation
        case 0xad: printf("Not yet implemented\n"); break;
        case 0xae: printf("Not yet implemented\n"); break;
        case 0xb0: printf("Not yet implemented\n"); break;
        case 0xb1: printf("Not yet implemented\n"); break;
        case 0xb4: printf("Not yet implemented\n"); break;
        case 0xb5: printf("Not yet implemented\n"); break;
        case 0xb6: printf("Not yet implemented\n"); break;
        case 0xb8: printf("Not yet implemented\n"); break;
        case 0xb9: printf("Not yet implemented\n"); break;
        case 0xba: printf("Not yet implemented\n"); break;
        case 0xbc: printf("Not yet implemented\n"); break;
        case 0xbd: printf("Not yet implemented\n"); break;
        case 0xbe: printf("Not yet implemented\n"); break;
        case 0xc0: printf("Not yet implemented\n"); break;
        case 0xc1: printf("Not yet implemented\n"); break;
        case 0xc4: printf("Not yet implemented\n"); break;
        case 0xc5: printf("Not yet implemented\n"); break;
        case 0xc6: printf("Not yet implemented\n"); break;
        case 0xc8: printf("Not yet implemented\n"); break;
        case 0xc9: printf("Not yet implemented\n"); break;
        case 0xca: printf("Not yet implemented\n"); break;
        case 0xcc: printf("Not yet implemented\n"); break;
        case 0xcd: printf("Not yet implemented\n"); break;
        case 0xce: printf("Not yet implemented\n"); break;
        case 0xd0: printf("Not yet implemented\n"); break;
        case 0xd1: printf("Not yet implemented\n"); break;

        // Abraham implementation
        case 0xd5:
            execute_0xd5(state); 
            break;
        case 0xd6: 
            execute_0xd6(state); 
            break;
        case 0xd8:
            execute_0xd8(state); 
            break;
        case 0xd9: 
            execute_0xd9(state);
            break;
        case 0xdd:
            execute_0xdd(state);
            break;
        case 0xde:
            execute_0xde(state); 
            break;
        case 0xe0:
            execute_0xe0(state); break;
        case 0xe1:
            execute_0xe1(state);
            break;
        case 0xe4:
            execute_0xe4(state); 
            break;
        case 0xe5:
            execute_0xe5(state);
            break;
        case 0xe6:
            execute_0xe6(state); 
            break;
        case 0xe8:
            execute_0xe8(state);
            break;
        case 0xe9:
            execute_0xe9(state);
            break;
        case 0xea:
            execute_0xea(state);
            break;
        case 0xec:
            execute_0xec(state);
            break;
        case 0xed:
            execute_0xed(state);
            break;
        case 0xee:
            execute_0xee(state);
            break;
        case 0xf0:
            execute_0xf0(state);
            break;
        case 0xf1:
            execute_0xf1(state);
            break;
        case 0xf5:
            execute_0xf5(state);
            break;
        case 0xf6:
            execute_0xf6(state);
            break;
        case 0xf8:
            execute_0xf8(state);
            break;
        case 0xf9:
            execute_0xf9(state);
            break;
        case 0xfd:
            execute_0xfd(state);
            break;
        case 0xfe:
            execute_0xfe(state);
            break;
        default: printf("Invalid opcode: %02x\n", *opcode); break;
    }
	
	// If pc is 0xFFFF at this point, an increment will cause an overflow, 
	// and pc will be set to 0x0000, which we may not want. For now,
	// we get the program to exit in this case.
	if (state->pc == 0xFFFF) {
		state->exit_prog = true;
	}

	// To get the opcode.
	// PC shouldn't be modified before the instruction is executed so that
	// the current value can be obtained/pushed to the stack etc. by the 
	// instruction that was executed.
    state->pc++;
	return 0;
}

int main(int argc, char* argv[]) {
	if (argc != 2) {
		fprintf(stderr, "Usage: %s __ROM_file__\n", argv[0]);
		exit(EXIT_FAILURE);
	}

	char* fn = argv[1];
	FILE* fp = fopen(fn, "r"); 
	if (!fp) {
		fprintf(stderr, "Error in func main: problem opening the ROM file.\n");
		exit(EXIT_FAILURE);
	}

	int seek_res = fseek(fp, 0L, SEEK_END);
	if (seek_res < 0) {
		fprintf(stderr, "Error in func main: problem encountered when calling fseek.\n");
		exit(EXIT_FAILURE);
	}

	long end_offset = ftell(fp);
	if (end_offset < 0) {
		fprintf(stderr, "Error in func main: problem encountered when calling ftell.\n");
		exit(EXIT_FAILURE);
	}

	if (end_offset > 0xFFFF) {
		fprintf(stderr, "Error in func main: file is larger than the addressable space for the 6502.\n");
		exit(EXIT_FAILURE);
	}

	// seek to the beginning of the file
	seek_res = fseek(fp, 0L, SEEK_SET);
	if (seek_res < 0) {
		fprintf(stderr, "Error in func main: problem encountered when calling fseek.\n");
		exit(EXIT_FAILURE);
	}

	// Memory map shows it going to 0xFFFF: https://www.nesdev.org/wiki/CPU_memory_map
	// Because of buffer overflow issues, extra memory is allocated here.
	// It is unclear why the ROM file takes up more space than (0xFFFF - 0x8000) bytes.
	unsigned char* buf = calloc(0xFFFF+end_offset, 1);
	if (!buf) {
		fprintf(stderr, "Error in func main: problem encountered when calling calloc.\n");
		goto CLEANUP;
	}

	// The Falling game repo indicates the PRG-ROM layout 
	// (line 30 of https://github.com/xram64/falling-nes/blob/master/source/falling.asm)
	// It seems to start at 0x8000, which is consistent with the nesdev memory 
	// map showing $4000-$FFFF as available for cartridge use.
	
	// The bytes from the ROM that should be copied to memory start after the 
	// 16 byte header of the iNES format (for .nes files), and a 'trainer' if it is present
	// (there is no indication of a 'trainer' being present for the 'Falling'
	// game.
	// See this reference: https://www.nesdev.org/wiki/INES
	long prg_length = end_offset - 16;
	// Seek past the iNES header.
	seek_res = fseek(fp, 16L, SEEK_SET);
	if (seek_res < 0) {
		fprintf(stderr, "Error in func main: problem encountered when calling fseek.\n");
		exit(EXIT_FAILURE);
	}

	uint16_t prg_start = 0x8000;

	size_t nread = fread(buf + prg_start, 1, (size_t)prg_length, fp);
	if (nread != (size_t)prg_length) {
		fprintf(stderr, "Error in func main: wrong number of bytes read from file.\n");
		goto CLEANUP;
	}

	Flags flags = {0};
	
	// As per https://www.nesdev.org/wiki/CPU_power_up_state, the interrupt
	// disable flag is set and all other flags are cleared at power up.
	flags.inter_disable_flag = 0x01;

	State6502 state_cpu;
	state_cpu.flgs = &flags; 
	state_cpu.memory = buf; 
	//state_cpu.pc = prg_start;

	// As per https://www.nesdev.org/wiki/CPU_power_up_state, PC is initialized
	// to 16 bit address found at 0xFFFC.
	// Reading the .nes file into memory. See 
	// https://www.nesdev.org/wiki/INES & https://forums.nesdev.org/viewtopic.php?t=15104 
	// for details.
	unsigned char start_addr_bytes[] = {state_cpu.memory[0xFFFC], state_cpu.memory[0xFFFD]};
	uint16_t start_addr = (start_addr_bytes[1] << 8) | start_addr_bytes[0];
	fprintf(stdout, "start_addr is: 0x%.04X\n", start_addr);
	state_cpu.pc = start_addr;

	// The stack pointer holds the lower 8 bits of the next free location on 
	// the stack. This works because the stack is 256 bytes.
	state_cpu.sp = 0x01FF & 0x00FF;
	state_cpu.a = 0;
	state_cpu.x = 0;
	state_cpu.y = 0;
	state_cpu.exit_prog = false;

	while (!state_cpu.exit_prog) {
		// the program counter is advanced in the Emulate function
		int res = Emulate(&state_cpu);
		if (res < 0) {
			goto CLEANUP;
		}
	}

	
CLEANUP:;
	free(buf);
	fclose(fp);
	return EXIT_SUCCESS;

}
