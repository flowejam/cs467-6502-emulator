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
 * Does not attempt to handle invalid results.
 * This helper function takes a raw byte containing a signed integer value, 
 * determines if the value is negative or positive, and applies the offset to 
 * to the 16 bit value accordingly. 
 */
uint16_t apply_signed_offset_in_unsigned_char(uint16_t val, unsigned char offset) {
	// TODO: consider adding error handling. E.g., what happens if there is 
	// integer overflow or underflow? Should it only jump to zero if there is 
	// underflow? Needs research.
	
	// Used for branches.
    uint16_t res = val;                                                        
                                                                               
    if ( (offset & 0x80) == 0x80) {                                            
		// two's complement
        unsigned char new_offset = (~offset + 1);                              
        res -= new_offset;                                                     
    } else {                                                                   
        res += offset;                                                         
    }                                                                          
                                                                               
    return res;                                                                
}       

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
 * This helper function is used after popping the processor state register
 * from the stack (e.g.: the PLP instruction). It updates all of the flags 
 * using the value in the processor_status register.
 * Don't use this helper for any other purpose.
 */
static void update_flags_from_processor_status(State6502* state) {
	state->flgs->crry_flag = (state->processor_status & 0x01) == 0x01 ? 1 : 0;
	state->flgs->zro_flag = (state->processor_status & 0x02) == 0x02 ? 1 : 0;
	state->flgs->inter_disable_flag = (state->processor_status & 0x04) == 0x04 ? 1 : 0;
	state->flgs->dec_flag = (state->processor_status & 0x08) == 0x08 ? 1 : 0;
	state->flgs->brk_flag = (state->processor_status & 0x10) == 0x10 ? 1 : 0;
	state->flgs->of_flag = (state->processor_status & 0x40) == 0x40 ? 1 : 0;
	state->flgs->neg_flag = (state->processor_status & 0x80) == 0x80 ? 1 : 0;
}

/*
 * Assumes the MSB is pushed first, so that the LSB is popped first. 
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
 * Assumes the MSB is pushed first, so that the LSB is popped first. 
 * This is confirmed in https://en.wikipedia.org/wiki/Interrupts_in_65xx_processors.
 * 'size' argument should always be 2:
 * by default, we should only pop a word (16 bits) at a time, even
 * if only 1 byte needs to be popped.
 */
static int pop_stack(State6502* state, int size, unsigned char* byte_arr) {
	uint16_t stack_addr = state->sp | 0x0100;

	for (int i = 0; i < size; ++i) {
		++stack_addr;
		// Note that the 6502 doesn't detect stack underflow. I have implemented
		// it here for debugging purposes.
		if (stack_addr > 0x01FF) {
			fprintf(stderr, "Error in func push_stack: stack underflow!\n");
			state->exit_prog = true;
			return -1;
		}
		byte_arr[i] = state->memory[stack_addr];
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
	
	fprintf(stdout, "0x%.04X Executing opcode 0x00: BRK\n", (state->pc));
	
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
	// indirect addressing -> using an address to get another address, and
	// retrieving the data from that address.
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
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x05(State6502* state) {
	fprintf(stdout, "Executing opcode 0x05: ORA - zero page\n");
	state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char byte_to_or = state->memory[zero_page_addr];

	// inclusive OR on accumulator contents.
	state->a |= byte_to_or;

	// set zero flag if applicable.
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 1 : 0;
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
	state->flgs->zro_flag = (op_result == 0x00) ? 1 : 0;

	state->flgs->neg_flag = (op_result & 0x80) ? 1 : 0;
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
	fprintf(stdout, "Executing opcode 0x09: ORA - Immediate\n");
	state->pc++;
	unsigned char byte_to_or = state->memory[state->pc];

	// inclusive OR on accumulator contents.
	state->a |= byte_to_or;

	// set zero flag if applicable.
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x0a(State6502* state) {
	fprintf(stdout, "Executing opcode 0x0a: ASL - Accumulator\n");
	unsigned char op_result = (state->a << 1);
	uint8_t old_bit7 = (state->a & 0x80) == 0x80 ? 0x01 : 0x00;
	state->a = op_result;

	state->flgs->crry_flag = old_bit7;
	
	state->flgs->zro_flag = (op_result == 0x00) ? 1 : 0;

	state->flgs->neg_flag = ((op_result & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x0d(State6502* state) {
	fprintf(stdout, "Executing opcode 0x0d: ORA - Absolute\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;

	unsigned char byte_to_or = state->memory[addr];

	// inclusive OR on accumulator contents.
	state->a |= byte_to_or;

	// set zero flag if applicable.
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x0e(State6502* state) {
	fprintf(stdout, "Executing opcode 0x0e: ASL - Absolute\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;

	unsigned char selected_byte = state->memory[addr];
	unsigned char op_result = (selected_byte << 1);
	uint8_t old_bit7 = (selected_byte & 0x80) == 0x80 ? 0x01 : 0x00;
	state->memory[addr] = op_result;

	state->flgs->crry_flag = old_bit7;
	
	// The author of the guide here (https://www.nesdev.org/obelisk-6502-guide/reference.html#ASL)
	// indicated that the zero flag should be set if the result of the instruction 
	// applied to its operand is zero, not (as it states in the guide) the 
	// accumulator. 
	// See the author's comment here: http://forum.6502.org/viewtopic.php?f=12&t=5351
	state->flgs->zro_flag = (op_result == 0x00) ? 1 : 0;

	state->flgs->neg_flag = ((op_result & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x10(State6502* state) {
	fprintf(stdout, "Executing opcode 0x10: BPL - Relative\n");
	++state->pc;
	unsigned char offset = state->memory[state->pc];
	if (state->flgs->neg_flag == 0x00) {
		// Note that, as per pages 38-39 of 
		// https://web.archive.org/web/20221112230813if_/http://archive.6502.org/books/mcs6500_family_programming_manual.pdf
		// the offset is added to the value of the automatically incremented 
		// program counter (i.e., the PC value after reading the offset value).
		// It is therefore not necessary to make any adjustment here (as is done
		// with the JMP instruction).
		uint16_t new_pc = apply_signed_offset_in_unsigned_char(state->pc, offset);
		state->pc = new_pc;
	}
}

static void execute_0x11(State6502* state) {
	fprintf(stdout, "Executing opcode 0x11: ORA - (Indirect), Y\n");
	state->pc++;
	unsigned char addr_of_addr = state->memory[state->pc];
	unsigned char addr_bytes[] = {state->memory[addr_of_addr], state->memory[++addr_of_addr]}; 

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];
	addr += state->y;

	unsigned char byte_to_or = state->memory[addr];

	// inclusive OR on accumulator contents.
	state->a |= byte_to_or;

	// set zero flag if applicable.
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x15(State6502* state) {
	fprintf(stdout, "Executing opcode 0x15: ORA - Zero Page, X\n");
	state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char byte_to_or = state->memory[zero_page_addr];

	// inclusive OR on accumulator contents.
	state->a |= byte_to_or;

	// set zero flag if applicable.
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x16(State6502* state) {
	fprintf(stdout, "Executing opcode 0x16: ASL - Zero Page, X\n");
	state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->x) & 0xFF;
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
	state->flgs->zro_flag = (op_result == 0x00) ? 1 : 0;
	state->flgs->neg_flag = ((op_result & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x18(State6502* state) {
	fprintf(stdout, "Executing opcode 0x18: CLC - Implied\n");
	state->flgs->crry_flag = 0x00;
}

static void execute_0x19(State6502* state) {
	fprintf(stdout, "Executing opcode 0x19: ORA - Absolute, Y\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->y;

	unsigned char byte_to_or = state->memory[addr];

	// inclusive OR on accumulator contents.
	state->a |= byte_to_or;

	// set zero flag if applicable.
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x1d(State6502* state) {
	fprintf(stdout, "Executing opcode 0x1d: ORA - Absolute, X\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->x;

	unsigned char byte_to_or = state->memory[addr];

	// inclusive OR on accumulator contents.
	state->a |= byte_to_or;

	// set zero flag if applicable.
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x1e(State6502* state) {
	fprintf(stdout, "Executing opcode 0x1e: ASL - Absolute, X\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->x;

	unsigned char selected_byte = state->memory[addr];
	unsigned char op_result = (selected_byte << 1);
	uint8_t old_bit7 = (selected_byte & 0x80) == 0x80 ? 0x01 : 0x00;
	state->memory[addr] = op_result;

	state->flgs->crry_flag = old_bit7;
	
	// The author of the guide here (https://www.nesdev.org/obelisk-6502-guide/reference.html#ASL)
	// indicated that the zero flag should be set if the result of the instruction 
	// applied to its operand is zero, not (as it states in the guide) the 
	// accumulator. 
	// See the author's comment here: http://forum.6502.org/viewtopic.php?f=12&t=5351
	state->flgs->zro_flag = (op_result == 0x00) ? 1 : 0;

	state->flgs->neg_flag = ((op_result & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x20(State6502* state) {
	fprintf(stdout, "Executing opcode 0x20: JSR - Absolute\n");
	// The address of the next instruction (pc + 3) - 1 is pushed to the stack.
	uint16_t saved_addr = state->pc + 3 - 1;
	fprintf(stdout, "JSR pushed address 0x%04X to the stack.\n", saved_addr);
	int size = 2;
	unsigned char bytes[] = {(saved_addr >> 8), (saved_addr & 0xFF)};
	int push_result = push_stack(state, size, bytes);
	if (push_result < 0) {
		// error in call to push_stack
		return;
	}

	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];
	uint16_t addr = (byte2 << 8) | byte1;

	// assumes an increment will occur after this instruction.
	state->pc = addr - 1;
}

static void execute_0x21(State6502* state) {
	fprintf(stdout, "Executing opcode 0x21: AND - (Indirect, X)\n");
	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	uint16_t addr_of_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char addr_bytes[] = {state->memory[addr_of_addr],state->memory[++addr_of_addr]};
	uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];
	unsigned char byte_to_and = state->memory[addr];

	state->a &= byte_to_and;

	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	state->flgs->neg_flag = ( (state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x24(State6502* state) {
	fprintf(stdout, "Executing opcode 0x24: BIT - Zero Page\n");
	++state->pc;
	unsigned char zero_addr = state->memory[state->pc];
	unsigned char zero_addr_byte = state->memory[zero_addr];
	uint8_t bit_res = state->a & zero_addr_byte;

	state->flgs->neg_flag = (zero_addr_byte & 0x80) == 0x80 ? 1 : 0;
	state->flgs->of_flag = (zero_addr_byte & 0x40) == 0x40 ? 1 : 0;
	state->flgs->zro_flag = bit_res == 0x00 ? 1 : 0;
}

static void execute_0x25(State6502* state) {
	fprintf(stdout, "Executing opcode 0x25: AND - Zero Page\n");
	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char byte_to_and = state->memory[zero_page_addr];

	state->a &= byte_to_and;
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;
	state->flgs->neg_flag = ( (state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x26(State6502* state) {
	fprintf(stdout, "Executing opcode 0x26: ROL - Zero Page\n");
	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char byte_to_rotate = state->memory[zero_page_addr];
	uint8_t old_bit7 = (byte_to_rotate & 0x80) == 0x80 ? 1 : 0;
	uint8_t old_bit6 = (byte_to_rotate & 0x40) == 0x40 ? 1 : 0;

	unsigned char op_result = (byte_to_rotate << 1);
	// set or clear bit 0 based on the value of the carry flag.
	if (state->flgs->crry_flag == 0x01) {
		op_result |= state->flgs->crry_flag;
	} else {
		op_result &= ~(0x01);
	}

	state->flgs->crry_flag = old_bit7;
	state->flgs->neg_flag = old_bit6;
	state->flgs->zro_flag = op_result == 0x00 ? 1 : 0;
	state->memory[zero_page_addr] = op_result;
}

static void execute_0x28(State6502* state) {
	fprintf(stdout, "Executing opcode 0x28: PLP - Implied\n");
	// popped_bytes: {LSB, MSB};
	unsigned char popped_bytes[2] = {0};

	int pop_result = pop_stack(state, 2, popped_bytes);
	if (pop_result < 0) {
		// error in call to pop_stack
		return;
	}

	// Assumes the processor_status flag was pushed as a 16-bit value
	state->processor_status = popped_bytes[0];
	update_flags_from_processor_status(state);
}

static void execute_0x29(State6502* state) {
	fprintf(stdout, "Executing opcode 0x29: AND - Immediate\n");
	++state->pc;
	unsigned char byte_to_and = state->memory[state->pc];

	state->a &= byte_to_and;
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;
	state->flgs->neg_flag = ( (state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x2a(State6502* state) {
	fprintf(stdout, "Executing opcode 0x2a: ROL - Accumulator\n");
	uint8_t old_bit7 = (state->a & 0x80) == 0x80 ? 1 : 0;
	uint8_t old_bit6 = (state->a & 0x40) == 0x40 ? 1 : 0;

	unsigned char op_result = (state->a << 1);
	// set or clear bit 0 based on the value of the carry flag.
	if (state->flgs->crry_flag == 0x01) {
		op_result |= state->flgs->crry_flag;
	} else {
		op_result &= ~(0x01);
	}

	state->flgs->crry_flag = old_bit7;
	state->flgs->neg_flag = old_bit6;
	state->flgs->zro_flag = op_result == 0x00 ? 1 : 0;
	state->a = op_result;
}

//=============================================================================
// James opcode functions

static void execute_0x86(State6502* state) {
	fprintf(stdout, "Executing opcode 0x86: STX - Zero Page\n");
	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	state->memory[zero_page_addr] = state->x;
}

static void execute_0x88(State6502* state) {
	fprintf(stdout, "Executing opcode 0x88: DEY - Implied\n");

	// Y register is decremented
	--state->y;

	state->flgs->zro_flag = state->y == 0x00 ? 1 : 0; 
	state->flgs->neg_flag = (state->y & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0x8a(State6502* state) {
	fprintf(stdout, "Executing opcode 0x8a: TXA - Implied\n");
	state->a = state->x;
	state->flgs->zro_flag = state->a == 0x00 ? 1 : 0; 
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0x8c(State6502* state) {
	fprintf(stdout, "Executing opcode 0x8c: STY - Absolute\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];
	uint16_t addr = (byte2 << 8) | byte1;
	state->memory[addr] = state->y;
}

static void execute_0x8d(State6502* state) {
	fprintf(stdout, "Executing opcode 0x8d: STA - Absolute\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];
	uint16_t addr = (byte2 << 8) | byte1;
	state->memory[addr] = state->a;
}

static void execute_0x8e(State6502* state) {
	fprintf(stdout, "Executing opcode 0x8e: STX - Absolute\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];
	uint16_t addr = (byte2 << 8) | byte1;
	state->memory[addr] = state->x;
}

static void execute_0x90(State6502* state) {
	fprintf(stdout, "Executing opcode 0x90: BCC - Relative\n");
	++state->pc;
	unsigned char offset = state->memory[state->pc];
	if (state->flgs->crry_flag == 0x00) {
		// Note that, as per pages 38-39 of 
		// https://web.archive.org/web/20221112230813if_/http://archive.6502.org/books/mcs6500_family_programming_manual.pdf
		// the offset is added to the value of the automatically incremented 
		// program counter (i.e., the PC value after reading the offset value).
		// It is therefore not necessary to make any adjustment here (as is done
		// with the JMP instruction).
		uint16_t new_pc = apply_signed_offset_in_unsigned_char(state->pc, offset);
		state->pc = new_pc;
	}
}

static void execute_0x91(State6502* state) {
	fprintf(stdout, "Executing opcode 0x91: STA - (Indirect), Y\n");
	++state->pc;
	unsigned char addr_of_addr = state->memory[state->pc];
	unsigned char addr_bytes[2] = {state->memory[addr_of_addr], state->memory[addr_of_addr + 1]};
	uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];  
	addr += state->y;
	state->memory[addr] = state->a;
}

static void execute_0x94(State6502* state) {
	fprintf(stdout, "Executing opcode 0x94: STY - Zero Page, X\n");
	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->x) & 0xFF;
	state->memory[zero_page_addr] = state->y;
}

static void execute_0x95(State6502* state) {
	fprintf(stdout, "Executing opcode 0x95: STA - Zero Page, X\n");
	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->x) & 0xFF;
	state->memory[zero_page_addr] = state->a;
}

static void execute_0x96(State6502* state) {
	// Note that the wrap around effect occurs here, just as it does with 
	// Zero Page, X addressing mode. There is no indication to the contrary from 
	// the MCS6500 programming manual.
	fprintf(stdout, "Executing opcode 0x96: STX - Zero Page, Y\n");
	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->y) & 0xFF;
	state->memory[zero_page_addr] = state->x;
}

static void execute_0x98(State6502* state) {
	fprintf(stdout, "Executing opcode 0x98: TYA - Implied\n");
	state->a = state->y;
	state->flgs->zro_flag = state->a == 0x00 ? 1 : 0; 
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0x99(State6502* state) {
	fprintf(stdout, "Executing opcode 0x99: STA - Absolute, Y\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->y;

	state->memory[addr] = state->a;
}

static void execute_0x9a(State6502* state) {
	fprintf(stdout, "Executing opcode 0x9a: TXS - Implied\n");
	state->sp = state->x;
}

static void execute_0x9d(State6502* state) {
	fprintf(stdout, "Executing opcode 0x9d: STA - Absolute, X\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->x;

	state->memory[addr] = state->a;
}

static void execute_0xa0(State6502* state) {
	fprintf(stdout, "Executing opcode 0xa0: LDY - Immediate\n");
	++state->pc;
	unsigned char byte_to_load = state->memory[state->pc];
	state->y = byte_to_load;

	state->flgs->zro_flag = (state->y == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->y & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xa1(State6502* state) {
	fprintf(stdout, "Executing opcode 0xa1: LDA - (Indirect, X)\n");

	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	uint16_t addr_of_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char addr_bytes[] = {state->memory[addr_of_addr],state->memory[++addr_of_addr]};
	uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];
	unsigned char byte_to_load = state->memory[addr];

	state->a = byte_to_load;
	state->flgs->zro_flag = (state->a == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xa2(State6502* state) {
	fprintf(stdout, "Executing opcode 0xa2: LDX - Immediate\n");
	++state->pc;
	unsigned char byte_to_load = state->memory[state->pc];
	state->x = byte_to_load;

	state->flgs->zro_flag = (state->x == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->x & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xa4(State6502* state) {
	fprintf(stdout, "Executing opcode 0xa4: LDY - Zero Page\n");
	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char byte_to_load = state->memory[zero_page_addr];
	state->y = byte_to_load;
	state->flgs->zro_flag = (state->y == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->y & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xa5(State6502* state) {
	fprintf(stdout, "Executing opcode 0xa5: LDA - Zero Page\n");
	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char byte_to_load = state->memory[zero_page_addr];
	state->a = byte_to_load;
	state->flgs->zro_flag = (state->a == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xa6(State6502* state) {
	fprintf(stdout, "Executing opcode 0xa6: LDX - Zero Page\n");
	++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char byte_to_load = state->memory[zero_page_addr];
	state->x = byte_to_load;
	state->flgs->zro_flag = (state->x == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->x & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xa8(State6502* state) {
	fprintf(stdout, "Executing opcode 0xa8: TAY - Implied\n");
	state->y = state->a;
	state->flgs->zro_flag = state->y == 0x00 ? 1 : 0; 
	state->flgs->neg_flag = (state->y & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xa9(State6502* state) {
	fprintf(stdout, "Executing opcode 0xa9: LDA - Immediate\n");
	++state->pc;
	unsigned char byte_to_load = state->memory[state->pc];
	state->a = byte_to_load;
	state->flgs->zro_flag = state->a == 0x00 ? 1 : 0; 
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;

}

static void execute_0xaa(State6502* state) {
	fprintf(stdout, "Executing opcode 0xaa: TAX - Implied\n");
	state->x = state->a;
	state->flgs->zro_flag = state->x == 0x00 ? 1 : 0; 
	state->flgs->neg_flag = (state->x & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xac(State6502* state) {
	fprintf(stdout, "Executing opcode 0xac: LDY - Absolute\n");
	++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	unsigned char byte_to_load = state->memory[addr];
	state->y = byte_to_load;
	state->flgs->zro_flag = (state->y == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->y & 0x80) == 0x80 ? 1 : 0;
}


// Chris' opcode functions/////////////////////////////////////////////////////////////////////////////
static void execute_0x2c(State6502* state) {
	fprintf(stdout, "Executing opcode 0x2c: BIT - Absolute\n");
    // zero flag, set if the result if the AND is zero
    // The mask pattern in A is ANDed with the value in memory to set or clear the zero flag, but the result
    // is not kept. Bits 7 and 6 of the value from memory are copied into the N and V flags.
    // BIT $NNNN
    ++state->pc;
	
    // byte1 has the lower memory address
    unsigned char byte1 = state->memory[state->pc];
    ++state->pc;
    unsigned char byte2 = state->memory[state->pc];

    // 16 bit addresses are stored in little endian order
    uint16_t addr = (byte2 << 8) | byte1;

    unsigned char byte_to_and = state->memory[addr];

    // if the result of the ANDing is zero, set zero flag
    state->flgs->zro_flag = ((byte_to_and & state->a) == 0x00) ? 1 : 0;

    // set overflow flag to bit 6 of the memory value
    state->flgs->of_flag = (byte_to_and & 0x40) >> 6;

    // set neg flag to bit 7 of the memory value
    state->flgs->neg_flag = (byte_to_and & 0x80) >> 7;
}

static void execute_0x2d(State6502* state) {
    fprintf(stdout, "Executing opcode 0x2d: AND - Absolute\n");
    ++state->pc;
    // byte1 has the lower memory address
    unsigned char byte1 = state->memory[state->pc];
    ++state->pc;
    unsigned char byte2 = state->memory[state->pc];

    // 16 bit addresses are stored in little endian order
    uint16_t addr = (byte2 << 8) | byte1;

    unsigned char byte_to_and = state->memory[addr];

    // AND the contents of register A
    state->a &= byte_to_and;

	state->flgs->zro_flag = (state->a == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0x2e(State6502* state) {
    fprintf(stdout, "Executing opcode 0x2e: ROL - Absolute\n");
    ++state->pc;
    // byte1 has the lower memory address
    unsigned char byte1 = state->memory[state->pc];
    ++state->pc;
    unsigned char byte2 = state->memory[state->pc];

    // 16 bit addresses are stored in little endian order
    uint16_t addr = (byte2 << 8) | byte1;

    unsigned char target_byte = state->memory[addr];
    unsigned char res = (target_byte << 1);
    uint8_t old_bit7 = (target_byte & 0x80) == 0x80 ? 0x01 : 0x00;

    // fill bit 0 with current value of the carry flag
    if (state->flgs->crry_flag == 1){
        // this sets the 0th bit
        res |= state->flgs->crry_flag;
    } else {
        // this clears the 0th bit
        res &= ~(0x01);
    }
    state->memory[addr] = res;
    state->flgs->crry_flag = old_bit7;

	state->flgs->zro_flag = (res == 0) ? 1 : 0;
	state->flgs->neg_flag = (res & 0x80) == 0x80 ? 1 : 0;

}

static void execute_0x30(State6502* state) {
    fprintf(stdout, "Executing opcode 0x30: BMI - Relative\n");
    // if the negative flag is set then add the relative
    // displacement to the program counter to cause a branch
    // to a new location
    ++state->pc;
    int8_t val = state->memory[state->pc];

    // check if the negative flag is set
    if (state->flgs->neg_flag == 0x01) {
        state->pc += val;
    }
}

static void execute_0x31(State6502* state) {
    fprintf(stdout, "Executing opcode 0x31: AND - Indirect Indexed\n");
    // A logical AND is performed, bit by bit, on the accumulator contents
    // using the contents of a byte of memory
    // zero flag, set if (A or M) equals 0
    // negative flag, set if bit 7 set
    // this instructions uses 2 bytes, as follows: AND ($NN), Y
    ++state->pc;
    unsigned char addr_of_addr = state->memory[state->pc];
    unsigned char addr_bytes[] = {state->memory[addr_of_addr], state->memory[++addr_of_addr]};

    // little endian
    uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];
    addr += state->y;

    unsigned char byte_to_and = state->memory[addr];

    // modifying register a
    state->a &= byte_to_and;

    state->flgs->zro_flag = (state->a == 0x00) ? 0x01 : 0x00;
    state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 0x01 : 0x00;
}


static void execute_0x35(State6502* state) {
    // A logical AND is performed, bit by bit, on the accumulator contents
    // using the contents of a byte of memory
    // zero flag, set if (A or M) equals 0
    // negative flag, set if bit 7 set
    // used as follows: AND $NN, X
    fprintf(stdout, "Executing opcode 0x35: AND - Zero Page, X\n");
    ++state->pc;
    unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char byte_to_and = state->memory[zero_page_addr];

	// modify register a
	state->a &= byte_to_and;

	// set zero flag if applicable.
	state->flgs->zro_flag = (state->a == 0x00) ? 0x01 : 0x00;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 0x01 : 0x00;
}

static void execute_0x36(State6502* state) {
    fprintf(stdout, "Executing opcode 0x36: ROL - Zero Page, X\n");
    //Move each of the bits in either A or M one place to the left. Bit 0 is filled with
    // the current value of the carry flag whilst the old bit 7 becomes the new carry
    // flag value.
    ++state->pc;
    unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char target_byte = state->memory[zero_page_addr];

    // rotate left by 1
    unsigned char res = (target_byte << 1);

    uint8_t old_bit7 = (target_byte & 0x80) == 0x80 ? 0x01 : 0x00;

    // fill bit 0 with current value of the carry flag
    if (state->flgs->crry_flag == 1){
        // this sets the 0th bit
        res |= state->flgs->crry_flag;
    } else {
        // this clears the 0th bit
        res &= ~(0x01);
    }

    state->memory[zero_page_addr] = res;
    state->flgs->crry_flag = old_bit7;

    if (res == 0x00) {
        state->flgs->zro_flag = 0x01;
    }

    if ((res & 0x80) == 0x80) {
        state->flgs->neg_flag = 0x01;
    }
}

static void execute_0x38(State6502* state) {
    fprintf(stdout, "Executing opcode 0x38: SEC - Implied\n");
    // This is a 1 byte instruction code
    // set the carry flag to 1
    state->flgs->crry_flag = 0x01;
}

static void execute_0x39(State6502* state) {
    fprintf(stdout, "Executing opcode 0x39: AND - Absolute, Y\n");
    // use as follows: AND $NNNN, Y
    ++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->y;

	unsigned char byte_to_and = state->memory[addr];

	// modifying register a
	state->a &= byte_to_and;

	// set zero flag if applicable.
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x3d(State6502* state) {
    fprintf(stdout, "Executing opcode 0x3d: AND - Absolute, X\n");
    // use as follows: AND $NNNN, X
    ++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->x;

	unsigned char byte_to_and = state->memory[addr];

	// modifying register a
	state->a &= byte_to_and;

	// set zero flag if applicable.
	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x3e(State6502* state) {
    fprintf(stdout, "Executing opcode 0x3e: ROL - Absolute, X\n");
    // used as follows: ROL $NNNN, X
    ++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;

	unsigned char target_byte = state->memory[addr];

    // rotate left by 1
    unsigned char res = (target_byte << 1);

    uint8_t old_bit7 = (target_byte & 0x80) == 0x80 ? 0x01 : 0x00;

    // fill bit 0 with current value of the carry flag
    if (state->flgs->crry_flag == 1){
        // this sets the 0th bit
        res |= state->flgs->crry_flag;
    } else {
        // this clears the 0th bit
        res &= ~(0x01);
    }

    state->memory[addr] = res;
    state->flgs->crry_flag = old_bit7;

	// set zero flag if applicable.
	state->flgs->zro_flag = (res == 0x00) ? 1 : 0;

	// set negative flag if bit 7 is set.
	state->flgs->neg_flag = ((res & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x40(State6502* state) {
    fprintf(stdout, "Executing opcode 0x40: RTI - Implied\n");
    // RTI - Return from Interrupt
    // This is a 1 byte instruction
    // The RTI instruction is used at the end of an interrupt processing routine.
    // It pulls the processor flags from the stack followed by the program counter.
    // sets flags and pc to what they were before the interrupt took place

    unsigned char popped_processor_flag_bytes[2] = {0};

	int pop_result = pop_stack(state, 2, popped_processor_flag_bytes);
	if (pop_result < 0) {
		// error in call to pop_stack
		return;
	}

    // move byte from what was in stack to processor status register
	state->processor_status = popped_processor_flag_bytes[0];
    // update flags from processor register
	update_flags_from_processor_status(state);

    // pull program counter from the stack
    unsigned char pc_bytes[2] = {0};
	pop_result = pop_stack(state, 2, pc_bytes);
	if (pop_result < 0) {
		// error in call to pop_stack
		return;
	}

    // resets the program counter to what it was before the interrupt
    // since the BRK opcode pushed (pc + 2) to stack, pc must be decremented by 2
    // when pulled
    state->pc = pc_bytes[0] - 2;
}

static void execute_0x41(State6502* state) {
    fprintf(stdout, "Executing opcode 0x41: EOR - Indexed Indirect\n");
    // EOR ($NN, X)
    // An exclusive OR is performed, bit by bit, on the accumulator contents 
    // using the contents of a byte of memory.
    // instruction is 2 bytes
    ++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	uint16_t addr_of_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char addr_bytes[] = {state->memory[addr_of_addr],state->memory[++addr_of_addr]};
	uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];
	unsigned char byte_to_eor = state->memory[addr];

	state->a ^= byte_to_eor;

	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;

	state->flgs->neg_flag = ( (state->a & 0x80) == 0x80) ? 1 : 0;

}

static void execute_0x45(State6502* state) {
    fprintf(stdout, "Executing opcode 0x45: EOR - Zero Page\n");
    // EOR $NN
    ++state->pc;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char byte_to_eor = state->memory[zero_page_addr];

	state->a ^= byte_to_eor;

	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;
	state->flgs->neg_flag = ( (state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x46(State6502* state) {
    fprintf(stdout, "Executing opcode 0x46: LSR - Zero Page\n");
    // use as follows: LSR $NN
    // Each of the bits in A or M is shift one place to the right.
    // The bit that was in bit 0 is shifted into the carry flag. Bit 7 is set to zero.
    ++state->pc;
    unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char target_byte = state->memory[zero_page_addr];

    // bit 0 is placed into the carry flag
    state->flgs->crry_flag = (target_byte & 0x01) == 0x01 ? 1 : 0;

    // perform shift to the right
    uint8_t res = target_byte >> 1;

    // set bit 7 to zero
    res &= 0x7f;

    // set zero flag if res is zero
    state->flgs->zro_flag = (res == 0x00) ? 0x01 : 0x00;

    // set negative flag if bit 7 of the result is set
    // although the instructions state the above, it seems off since
    // bit 7 will always be a zero
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 0x01 : 0x00;

    // store back the result in the specified address
    state->memory[zero_page_addr] = res;
	
}

static void execute_0x48(State6502* state) {
    fprintf(stdout, "Executing opcode 0x48: PHA - implied\n");
    // pushes a copy of the accumulator on to the stack
    int size = 2;
    unsigned char accumulator_bytes[] = {0x00, state->a};
    int push_result = push_stack(state, size, accumulator_bytes);
    if (push_result < 0){
        // error in call to push stack
        return;
    }
}

static void execute_0x49(State6502* state) {
    fprintf(stdout, "Executing opcode 0x49: EOR - immediate\n");
    // EOR #$NN
    // uses 2 bytes
    // An exclusive OR is performed, bit by bit, on the accumulator contents
    // using the contents of a byte of memory.
    ++state->pc;
    unsigned char byte_to_eor = state->memory[state->pc];

	state->a ^= byte_to_eor;

    state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;
	state->flgs->neg_flag = ( (state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x4a(State6502* state) {
    fprintf(stdout, "Executing opcode 0x4a: LSR A - Accumulator\n");
    // uses only 1 byte
    // Each of the bits in A or M is shift one place to the right.
    // The bit that was in bit 0 is shifted into the carry flag. Bit 7 is set to zero.
    
    unsigned char op_result = (state->a >> 1);

    // bit 0 is placed into the carry flag
    state->flgs->crry_flag = (state->a & 0x01) == 0x01 ? 1 : 0;

    // set bit 7 to zero
    op_result &= 0x7f;

    // set zero flag if res is zero
    state->flgs->zro_flag = (op_result == 0x00) ? 0x01 : 0x00;

    // set negative flag if bit 7 of the result is set
    // although the instructions state the above, it seems off since
    // bit 7 will always be a zero
    state->flgs->neg_flag = (op_result & 0x80) == 0x80 ? 0x01 : 0x00;

    // store back the result to the accumulator
    state->a = op_result;
}

static void execute_0x4c(State6502* state) {
    fprintf(stdout, "Executing opcode 0x4c: JMP - Absolute\n");
    // use as follows: JMP $NNNN
    // instruction is 3 bytes
    // sets the program counter to the address specified by the operand.
    ++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;

    // subtract by 1 because the emulate function increments pc by 1
    // after instruction execution
	state->pc = addr - 1;
}

static void execute_0x4d(State6502* state) {
    fprintf(stdout, "Executing opcode 0x4d: EOR - Absolute\n");
    ++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;

    unsigned char byte_to_eor = state->memory[addr];

    state->a ^= byte_to_eor;

	state->flgs->zro_flag = (state->a == 0x00) ? 1 : 0;
	state->flgs->neg_flag = ( (state->a & 0x80) == 0x80) ? 1 : 0;
}

static void execute_0x4e(State6502* state) {
    fprintf(stdout, "Executing opcode 0x4e: LSR - Absolute\n");
    ++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;

    unsigned char target_byte = state->memory[addr];

    // bit 0 is placed into the carry flag
    state->flgs->crry_flag = (target_byte & 0x01) == 0x01 ? 1 : 0;

    // perform shift to the right
    uint8_t res = target_byte >> 1;

    // set bit 7 to zero
    res &= 0x7f;

    // set zero flag if res is zero
    state->flgs->zro_flag = (res == 0x00) ? 0x01 : 0x00;

    // set negative flag if bit 7 of the result is set
    // although the instructions state the above, it seems off since
    // bit 7 will always be a zero
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 0x01 : 0x00;

    // store back the result in the specified address
    state->memory[addr] = res;
}

static void execute_0x50(State6502* state) {
    fprintf(stdout, "Executing opcode 0x50: BVC - Relative\n");
    // BVC $NN
    ++state->pc;
    // If the overflow flag is clear then add the relative displacement to the
    // program counter to cause a branch to a new location.
    int8_t value = state->memory[state->pc];

    // if flag is clear, then branch out
    if (state->flgs->of_flag == 0x00){
        // subtract 1 because pc is incremented in the emulate function
        state->pc += (value - 1);
    }
}

static void execute_0x51(State6502* state) {
    fprintf(stdout, "Executing opcode 0x51: EOR - Indirect Indexed\n");
    // EOR ($NN), Y
    ++state->pc;
    unsigned char addr_of_addr = state->memory[state->pc];
    unsigned char addr_bytes[] = {state->memory[addr_of_addr], state->memory[++addr_of_addr]};

    // little endian
    uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];
    addr += state->y;

    unsigned char byte_to_eor = state->memory[addr];

    // modifying register a
    state->a ^= byte_to_eor;

    state->flgs->zro_flag = (state->a == 0x00) ? 0x01 : 0x00;
    state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 0x01 : 0x00;
}

static void execute_0x55(State6502* state) {
    fprintf(stdout, "Executing opcode 0x55: EOR - Zero Page, X\n");
    // EOR $NN, X
    state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char byte_to_eor = state->memory[zero_page_addr];

    // modifying register a
    state->a ^= byte_to_eor;

    state->flgs->zro_flag = (state->a == 0x00) ? 0x01 : 0x00;
    state->flgs->neg_flag = ((state->a & 0x80) == 0x80) ? 0x01 : 0x00;
}

static void execute_0x56(State6502* state) {
    fprintf(stdout, "Executing opcode 0x56: LSR - Zero Page, X\n");
    state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char target_byte = state->memory[zero_page_addr];

    // bit 0 is placed into the carry flag
    state->flgs->crry_flag = (target_byte & 0x01) == 0x01 ? 1 : 0;

    // perform shift to the right
    uint8_t res = target_byte >> 1;

    // set bit 7 to zero
    res &= 0x7f;

    // set zero flag if res is zero
    state->flgs->zro_flag = (res == 0x00) ? 0x01 : 0x00;

    // set negative flag if bit 7 of the result is set
    // although the instructions state the above, it seems off since
    // bit 7 will always be a zero
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 0x01 : 0x00;

    // store back the result in the specified address
    state->memory[zero_page_addr] = res;
}

// Abraham opcode functions
static void execute_0x58(State6502* state) {
    // Opccode 0x58: CLI - Clear Interrupt Disable Flag
    fprintf(stdout, "Executing opcode 0x58: CLI\n");
    // clearing the interrupt disable flag
    state->flgs->inter_disable_flag = 0x00;
    // advance the program counter to the next instruction
    state->pc++;

}

static void execute_0x59(State6502* state) {
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

static void execute_0x5d(State6502* state) {
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

static void execute_0x5e(State6502* state) {
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
	fprintf(stdout, "RTS popped address 0x%04X from the stack.\n", state->pc);
}

static void execute_0x61(State6502* state) {
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
    // advance the program counter to the next instruction
    state->pc++;
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
    // advance the program counter to the next instruction
    state->pc++;
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
    // advance the program counter to the next instruction
    state->pc++;
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
    // advance the program counter to the next instruction
    state->pc++;
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
    // advance the program counter to the next instruction
    state->pc++;
}

static void execute_0x6c(State6502* state) {
    // Opccode 0x6C: JMP - Jump Indirect
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
    fprintf(stdout, "Executing opcode 0x6C: JMP\n");

    // Fetch the address for the next two bytes
    uint16_t addr = state->memory[state->pc + 1] | (state->memory[state->pc + 2] << 8);
    state->pc += 2;

    // Jump to the new address
    state->pc = addr - 1;

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
    // advance the program counter to the next instruction
    state->pc++;
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
    // advance the program counter to the next instruction
    state->pc++;
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

    // advance the program counter to the next instruction
    state->pc++;
}

static void execute_0x71(State6502* state) {
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
    // advance the program counter to the next instruction
    state->pc++;
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
    // advance the program counter to the next instruction
    state->pc++;
}

static void execute_0x78(State6502* state) {
    // Opccode 0x78: SEI - Set Interrupt Disable Flag
    // https://www.nesdev.org/obelisk-6502-guide/reference.html#SEI
    fprintf(stdout, "0x%.04X Executing opcode 0x78: SEI\n", state->pc);

    // Set the interrupt disable flag
    state->flgs->inter_disable_flag = 0x01;
    // advance the program counter to the next instruction
    state->pc++;
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
    // advance the program counter to the next instruction
    state->pc++;
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
    // advance the program counter to the next instruction
    state->pc++;
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
    // advance the program counter to the next instruction
    state->pc++;
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

    // advance the program counter to the next instruction
    state->pc++;
}

static void execute_0x84(State6502* state) {
	// TODO
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

    // advance the program counter to the next instruction
    state->pc++;
}

// TODO: remove commented out code
//static void execute_0x86(State6502* state) {
//    // Opccode 0x86: STX - Store X Register Zero Page
//    // https://www.nesdev.org/obelisk-6502-guide/reference.html#STX
//    fprintf(stdout, "Executing opcode 0x86: STX\n");
//
//    // Fetch the address for the next byte
//    uint16_t addr = state->memory[state->pc + 1];
//    state->pc++;
//
//    // Store the value in the X register at the address
//    state->memory[addr] = state->x;
//
//    // advance the program counter to the next instruction
//    state->pc++;
//}
// end of Abraham's opcodes up to 0x86

// Chris remaining opcode implementation /////////xyz//////////////////////////////////////////////////////////////////////
static void execute_0xad(State6502* state) {
    fprintf(stdout, "Executing opcode 0xad: LDA Absolute\n");
    // Loads a byte of memory into the accumulator setting the zero and negative flags
    //  as appropriate.
    ++state->pc;
	
    // byte1 has the lower memory address
    unsigned char byte1 = state->memory[state->pc];
    ++state->pc;
    unsigned char byte2 = state->memory[state->pc];
 
    // 16 bit addresses are stored in little endian order
    uint16_t addr = (byte2 << 8) | byte1;

    unsigned char target_byte = state->memory[addr];

    state->a = target_byte;

    state->flgs->zro_flag = (state->a == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xae(State6502* state) {
    fprintf(stdout, "Executing opcode 0xae: LDX Absolute\n");
    // Loads a byte of memory into the X register setting the zero
    // and negative flags as appropriate.
    ++state->pc;
    // byte1 has the lower memory address
    unsigned char byte1 = state->memory[state->pc];
    ++state->pc;
    unsigned char byte2 = state->memory[state->pc];
 
    // 16 bit addresses are stored in little endian order
    uint16_t addr = (byte2 << 8) | byte1;

    unsigned char target_byte = state->memory[addr];

    state->x = target_byte;

    state->flgs->zro_flag = (state->x == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->x & 0x80) == 0x80 ? 1 : 0;

}

static void execute_0xb0(State6502* state) {
    fprintf(stdout, "Executing opcode 0xb0: BCS Relative\n");
    //If the carry flag is set then add the relative displacement to the program
    //counter to cause a branch to a new location.
	++state->pc;
	unsigned char offset = state->memory[state->pc];
	if (state->flgs->crry_flag == 0x01) {
		uint16_t new_pc = apply_signed_offset_in_unsigned_char(state->pc, offset);
		state->pc = new_pc;
	}

}

static void execute_0xb1(State6502* state) {
    fprintf(stdout, "Executing opcode 0xb1: LDA Indirect Indexed\n");
    ++state->pc;
    unsigned char addr_of_addr = state->memory[state->pc];
    unsigned char addr_bytes[] = {state->memory[addr_of_addr], state->memory[++addr_of_addr]};

    // little endian
    uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];
    addr += state->y;

    unsigned char target_byte = state->memory[addr];

    state->a = target_byte;

    state->flgs->zro_flag = (state->a == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xb4(State6502* state) {
    fprintf(stdout, "Executing opcode 0xb4: LDY Zero Page, X\n");
    state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char target_byte = state->memory[zero_page_addr];

    state->y = target_byte;

    state->flgs->zro_flag = (state->y == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->y & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xb5(State6502* state) {
    fprintf(stdout, "Executing opcode 0xb5: LDA Zero Page, X \n");
    state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char target_byte = state->memory[zero_page_addr];

    state->a = target_byte;

    state->flgs->zro_flag = (state->a == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xb6(State6502* state) {
    fprintf(stdout, "Executing opcode 0xb6: LDX Zero Page, Y\n");
    //Loads a byte of memory into the X register setting the zero and 
    // negative flags as appropriate.
    state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	zero_page_addr = (zero_page_addr + state->y) & 0xFF;
	unsigned char target_byte = state->memory[zero_page_addr];

    state->x = target_byte;

    state->flgs->zro_flag = (state->x == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->x & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xb8(State6502* state) {
    fprintf(stdout, "Executing opcode 0xb8: CLV - Implied\n");
    // clears the overflow flag
    state->flgs->of_flag = 0;
}

static void execute_0xb9(State6502* state) {
    fprintf(stdout, "Executing opcode 0xb9: LDA - Absolute, Y\n");
    ++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->y;
    unsigned char target_byte = state->memory[addr];

    state->a = target_byte;

    state->flgs->zro_flag = (state->a == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xba(State6502* state) {
    fprintf(stdout, "Executing opcode 0xba: TSX - implied\n");
    //Copies the current contents of the stack register into the X register
    //and sets the zero and negative flags as appropriate.
    state->x = state->sp;

    state->flgs->zro_flag = (state->x == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->x & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xbc(State6502* state) {
    fprintf(stdout, "Executing opcode 0xbc: LDY - Absolute,X\n");
    // load the y register
    ++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->x;
    unsigned char target_byte = state->memory[addr];

    state->y = target_byte;

    state->flgs->zro_flag = (state->y == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->y & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xbd(State6502* state) {
    fprintf(stdout, "Executing opcode 0xbd: LDA - Absolute,X\n");
    ++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->x;
    unsigned char target_byte = state->memory[addr];

    state->a = target_byte;

    state->flgs->zro_flag = (state->a == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->a & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xbe(State6502* state) {
    fprintf(stdout, "Executing opcode 0xbe: LDX - Absolute Y\n");

    ++state->pc;
	unsigned char byte1 = state->memory[state->pc];
	++state->pc;
	unsigned char byte2 = state->memory[state->pc];

	// 16 bit addresses are stored in little endian order
	uint16_t addr = (byte2 << 8) | byte1;
	addr += state->y;
    unsigned char target_byte = state->memory[addr];

    state->x = target_byte;

    state->flgs->zro_flag = (state->x == 0) ? 1 : 0;
	state->flgs->neg_flag = (state->x & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xc0(State6502* state) {
    fprintf(stdout, "Executing opcode 0xc0: CPY - Immediate\n");
    // This instruction compares the contents of the Y register with
    //another memory held value and sets the zero and carry flags
    // as appropriate.
    state->pc++;
	unsigned char target_byte = state->memory[state->pc];

    state->flgs->crry_flag = (state->y >= target_byte) ? 1 : 0;
    state->flgs->zro_flag = (state->y == target_byte) ? 1 : 0;

    // to set/clear the negative flag
    unsigned char res = state->y - target_byte;
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xc1(State6502* state) {
    fprintf(stdout, "Executing opcode 0xc1: CMP - Indexed Indirect\n");
    // compares accumulator with memory
    state->pc++;
	
    unsigned char zero_page_addr = state->memory[state->pc];
	uint16_t addr_of_addr = (zero_page_addr + state->x) & 0xFF;
	unsigned char addr_bytes[] = {state->memory[addr_of_addr],state->memory[++addr_of_addr]};
	uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];

	unsigned char target_byte = state->memory[addr];


    state->flgs->crry_flag = (state->a >= target_byte) ? 1 : 0;
    state->flgs->zro_flag = (state->a == target_byte) ? 1 : 0;

    // to set/clear the negative flag
    unsigned char res = state->a - target_byte;
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 1 : 0;

}

static void execute_0xc4(State6502* state) {
    fprintf(stdout, "Executing opcode 0xc4: CPY - Zero Page\n");
    state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char target_byte = state->memory[zero_page_addr];

    state->flgs->crry_flag = (state->y >= target_byte) ? 1 : 0;
    state->flgs->zro_flag = (state->y == target_byte) ? 1 : 0;

    // to set/clear the negative flag
    unsigned char res = state->y - target_byte;
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xc5(State6502* state) {
    fprintf(stdout, "Executing opcode 0xc5: CMP - Zero Page\n");

    state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char target_byte = state->memory[zero_page_addr];

    state->flgs->crry_flag = (state->a >= target_byte) ? 1 : 0;
    state->flgs->zro_flag = (state->a == target_byte) ? 1 : 0;

    // to set/clear the negative flag
    unsigned char res = state->a - target_byte;
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xc6(State6502* state) {
    fprintf(stdout, "Executing opcode 0xc6: DEC - Zero Page\n");
    // Subtracts one from the value held at a specified memory location
    // setting the zero and negative flags as appropriate.

    state->pc++;
	unsigned char zero_page_addr = state->memory[state->pc];
	unsigned char target_byte = state->memory[zero_page_addr];

    // decrement
    target_byte -= 1;

    // save result back in memory
    state->memory[zero_page_addr] = target_byte;

    // update flags
    state->flgs->zro_flag = (target_byte == 0) ? 1 : 0;
    state->flgs->neg_flag = (target_byte & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xc8(State6502* state) {
    fprintf(stdout, "Executing opcode 0xc8: INY - Implied\n");
    // Adds one to the Y register setting the zero and negative flags
    // as appropriate.
    ++state->y;

	state->flgs->zro_flag = (state->y == 0x00) ? 1 : 0; 
	state->flgs->neg_flag = (state->y & 0x80) == 0x80 ? 1 : 0;
    
}

static void execute_0xc9(State6502* state) {
    fprintf(stdout, "Executing opcode 0xc9: CMP - Immediate\n");
    state->pc++;
	unsigned char target_byte = state->memory[state->pc];
    
    state->flgs->crry_flag = (state->a >= target_byte) ? 1 : 0;
    state->flgs->zro_flag = (state->a == target_byte) ? 1 : 0;

    // to set/clear the negative flag
    unsigned char res = state->a - target_byte;
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xca(State6502* state) {
    fprintf(stdout, "Executing opcode 0xca: DEX - Implied\n");
    // decrements x register
    --state->x;

	state->flgs->zro_flag = (state->x == 0x00) ? 1 : 0; 
	state->flgs->neg_flag = (state->x & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xcc(State6502* state) {
    fprintf(stdout, "Executing opcode 0xcc: CPY - Absolute\n");
    ++state->pc;
    unsigned char byte1 = state->memory[state->pc];
    ++state->pc;
    unsigned char byte2 = state->memory[state->pc];
 
    // 16 bit addresses are stored in little endian order
    uint16_t addr = (byte2 << 8) | byte1;

    unsigned char target_byte = state->memory[addr];

    state->flgs->crry_flag = (state->y >= target_byte) ? 1 : 0;
    state->flgs->zro_flag = (state->y == target_byte) ? 1 : 0;

    // to set/clear the negative flag
    unsigned char res = state->y - target_byte;
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xcd(State6502* state) {
    fprintf(stdout, "Executing opcode 0xcd: CMP - Absolute\n");
    ++state->pc;
    unsigned char byte1 = state->memory[state->pc];
    ++state->pc;
    unsigned char byte2 = state->memory[state->pc];
 
    // 16 bit addresses are stored in little endian order
    uint16_t addr = (byte2 << 8) | byte1;

    unsigned char target_byte = state->memory[addr];

    state->flgs->crry_flag = (state->a >= target_byte) ? 1 : 0;
    state->flgs->zro_flag = (state->a == target_byte) ? 1 : 0;

    // to set/clear the negative flag
    unsigned char res = state->a - target_byte;
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xce(State6502* state) {
    fprintf(stdout, "Executing opcode 0xce: DEC - Absolute\n");
    ++state->pc;
    unsigned char byte1 = state->memory[state->pc];
    ++state->pc;
    unsigned char byte2 = state->memory[state->pc];
 
    // 16 bit addresses are stored in little endian order
    uint16_t addr = (byte2 << 8) | byte1;

    unsigned char target_byte = state->memory[addr];

    // decrement
    target_byte -= 1;

    // save result back in memory
    state->memory[addr] = target_byte;

    // update flags
    state->flgs->zro_flag = (target_byte == 0) ? 1 : 0;
    state->flgs->neg_flag = (target_byte & 0x80) == 0x80 ? 1 : 0;
}

static void execute_0xd0(State6502* state) {
    fprintf(stdout, "Executing opcode 0xd0: BNE - Relative\n");
    ++state->pc;
	unsigned char offset = state->memory[state->pc];

    // if the zero flag is clear, then add the relative displacement
	if (state->flgs->zro_flag == 0x00) {
		uint16_t new_pc = apply_signed_offset_in_unsigned_char(state->pc, offset);
		state->pc = new_pc;
	}
}

static void execute_0xd1(State6502* state) {
    fprintf(stdout, "Executing opcode 0xd1: CMP - Indirect Indexed\n");
    ++state->pc;
    unsigned char addr_of_addr = state->memory[state->pc];
    unsigned char addr_bytes[] = {state->memory[addr_of_addr], state->memory[++addr_of_addr]};

    // little endian
    uint16_t addr = (addr_bytes[1] << 8) | addr_bytes[0];
    addr += state->y;

    unsigned char target_byte = state->memory[addr];

    state->flgs->crry_flag = (state->a >= target_byte) ? 1 : 0;
    state->flgs->zro_flag = (state->a == target_byte) ? 1 : 0;

    // to set/clear the negative flag
    unsigned char res = state->a - target_byte;
    state->flgs->neg_flag = (res & 0x80) == 0x80 ? 1 : 0;
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
			execute_0x0a(state);
			break;
        case 0x0d: 
			execute_0x0d(state);
			break;
        case 0x0e: 
			execute_0x0e(state);
			break;
        case 0x10: 
			execute_0x10(state);
			break;
        case 0x11: 
			execute_0x11(state);
			break;
        case 0x15: 
			execute_0x15(state);
			break;
        case 0x16: 
			execute_0x16(state);
			break;
        case 0x18: 
			execute_0x18(state);
			break;
        case 0x19: 
			execute_0x19(state);
			break;
        case 0x1d: 
			execute_0x1d(state);
			break;
        case 0x1e: 
			execute_0x1e(state);
			break;
        case 0x20: 
			execute_0x20(state);
			break;
        case 0x21: 
			execute_0x21(state);
			break;
        case 0x24: 
			execute_0x24(state);
			break;
        case 0x25: 
			execute_0x25(state);
			break;
        case 0x26: 
			execute_0x26(state);
			break;
        case 0x28: 
			execute_0x28(state);
			break;
        case 0x29: 
			execute_0x29(state);
			break;
        case 0x2a: 
			execute_0x2a(state);
			break;

        // Chris implementation
        case 0x2c:
            execute_0x2c(state);
            break;
        case 0x2d:
            execute_0x2d(state);
            break;
        case 0x2e:
            execute_0x2e(state);
            break;
        case 0x30:
            execute_0x30(state);
            break;
        case 0x31:
            execute_0x31(state);
            break;
        case 0x35:
            execute_0x35(state);
            break;
        case 0x36:
            execute_0x36(state);
            break;
        case 0x38:
            execute_0x38(state);
            break;
        case 0x39:
            execute_0x39(state);
            break;
        case 0x3d:
            execute_0x3d(state);
            break;
        case 0x3e:
            execute_0x3e(state);
            break;
        case 0x40:
            execute_0x40(state);
            break;
        case 0x41:
            execute_0x41(state);
            break;
        case 0x45:
            execute_0x45(state);
            break;
        case 0x46:
            execute_0x46(state);
            break;
        case 0x48:
            execute_0x48(state);
            break;
        case 0x49:
            execute_0x49(state);
            break;
        case 0x4a:
            execute_0x4a(state);
            break;
        case 0x4c:
            execute_0x4c(state);
            break;
        case 0x4d:
            execute_0x4d(state);
            break;
        case 0x4e:
            execute_0x4e(state);
            break;
        case 0x50:
            execute_0x50(state);
            break;
        case 0x51:
            execute_0x51(state);
            break;
        case 0x55:
            execute_0x55(state);
            break;
        case 0x56:
            execute_0x56(state);
            break;

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
        case 0x86: 
            execute_0x86(state);
			break;
        case 0x88: 
            execute_0x88(state);
			break;
        case 0x8a: 
            execute_0x8a(state);
			break;
        case 0x8c: 
            execute_0x8c(state);
			break;
        case 0x8d: 
            execute_0x8d(state);
			break;
        case 0x8e: 
            execute_0x8e(state);
			break;
        case 0x90: 
            execute_0x90(state);
			break;
        case 0x91: 
            execute_0x91(state);
			break;
        case 0x94: 
            execute_0x94(state);
			break;
        case 0x95: 
            execute_0x95(state);
			break;
        case 0x96: 
            execute_0x96(state);
			break;
        case 0x98: 
            execute_0x98(state);
			break;
        case 0x99: 
            execute_0x99(state);
			break;
        case 0x9a: 
            execute_0x9a(state);
			break;
        case 0x9d: 
            execute_0x9d(state);
			break;
        case 0xa0: 
            execute_0xa0(state);
			break;
        case 0xa1: 
            execute_0xa1(state);
			break;
        case 0xa2: 
            execute_0xa2(state);
			break;
        case 0xa4: 
            execute_0xa4(state);
			break;
        case 0xa5: 
            execute_0xa5(state);
			break;
        case 0xa6: 
            execute_0xa6(state);
			break;
        case 0xa8: 
            execute_0xa8(state);
			break;
        case 0xa9: 
            execute_0xa9(state);
			break;
        case 0xaa: 
            execute_0xaa(state);
			break;
        case 0xac: 
            execute_0xac(state);
			break;

        // Chris implementation
        case 0xad:
            execute_0xad(state);
            break;
        case 0xae:
            execute_0xae(state);
            break;
        case 0xb0:
            execute_0xb0(state);
            break;
        case 0xb1:
            execute_0xb1(state);
            break;
        case 0xb4:
            execute_0xb4(state);
            break;
        case 0xb5:
            execute_0xb5(state);
            break;
        case 0xb6:
            execute_0xb6(state);
            break;
        case 0xb8:
            execute_0xb8(state);
            break;
        case 0xb9:
            execute_0xb9(state);
            break;
        case 0xba:
            execute_0xba(state);
            break;
        case 0xbc:
            execute_0xbc(state);
            break;
        case 0xbd:
            execute_0xbd(state);
            break;
        case 0xbe:
            execute_0xbe(state);
            break;
        case 0xc0:
            execute_0xc0(state);
            break;
        case 0xc1:
            execute_0xc1(state);
            break;
        case 0xc4:
            execute_0xc4(state);
            break;
        case 0xc5:
            execute_0xc5(state);
            break;
        case 0xc6:
            execute_0xc6(state);
            break;
        case 0xc8:
            execute_0xc8(state);
            break; 
        case 0xc9:
            execute_0xc9(state);
            break;
        case 0xca:
            execute_0xca(state);
            break; 
        case 0xcc:
            execute_0xcc(state);
            break; 
        case 0xcd:
            execute_0xcd(state);
            break; 
        case 0xce:
            execute_0xce(state);
            break;
        case 0xd0:
            execute_0xd0(state);
            break;
        case 0xd1:
            execute_0xd1(state);
            break;

        // Abraham implementation
        case 0xd5: printf("Not yet implemented\n"); break;
        case 0xd6: printf("Not yet implemented\n"); break;
        case 0xd8: printf("Not yet implemented\n"); break;
        case 0xd9: printf("Not yet implemented\n"); break;
        case 0xdd: printf("Not yet implemented\n"); break;
        case 0xde: printf("Not yet implemented\n"); break;
        case 0xe0: printf("Not yet implemented\n"); break;
        case 0xe1: printf("Not yet implemented\n"); break;
        case 0xe4: printf("Not yet implemented\n"); break;
        case 0xe5: printf("Not yet implemented\n"); break;
        case 0xe6: printf("Not yet implemented\n"); break;
        case 0xe8: printf("Not yet implemented\n"); break;
        case 0xe9: printf("Not yet implemented\n"); break;
        case 0xea: printf("Not yet implemented\n"); break;
        case 0xec: printf("Not yet implemented\n"); break;
        case 0xed: printf("Not yet implemented\n"); break;
        case 0xee: printf("Not yet implemented\n"); break;
        case 0xf0: printf("Not yet implemented\n"); break;
        case 0xf1: printf("Not yet implemented\n"); break;
        case 0xf5: printf("Not yet implemented\n"); break;
        case 0xf6: printf("Not yet implemented\n"); break;
        case 0xf8: printf("Not yet implemented\n"); break;
        case 0xf9: printf("Not yet implemented\n"); break;
        case 0xfd: printf("Not yet implemented\n"); break;
        case 0xfe: printf("Not yet implemented\n"); break;
        default: printf("Invalid opcode: %02x\n", *opcode); break;
    }
	
	// If pc is 0xFFFF at this point, an increment will cause an overflow, 
	// and pc will be set to 0x0000. For now, we get the program to exit in 
	// this case, but we will want to remove this.
	if (state->pc == 0xFFFF) {
		state->exit_prog = true;
	}

	// Note that some instructions are adjusted for this increment, assuming
	// it will always occur. E.g. JSR, JMP
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

	// Initialization code on the NES has a loop to wait for at least 30,000 
	// cycles to pass. This is related to the PPU and the memory location $2002 
	// is used for this. Since we are only emulating the CPU, we will set this
	// location to 0x80 to avoid an infinite loop.  
	// See the references here: 
	// 1) https://www.nesdev.org/wiki/PPU_power_up_state
	// 2) https://www.nesdev.org/wiki/Init_code
	// 3) falling.asm code, lines 229 & 212-215: https://github.com/xram64/falling-nes/blob/master/source/falling.asm
	state_cpu.memory[0x2002] = 0x80;
	
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
