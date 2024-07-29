#include "helpers6502.h"
#include <stdio.h>
#include <stdlib.h>

// ================== helper functions ======================================


/*
 * This helper function is used before pushing the processor state register
 * to the stack. Don't use this helper for any other purpose. E.g. for updating 
 * the flags, they should be set/cleared as follows:
 * state->flgs->crry_flag = 0x01;
 */
void update_processor_status(State6502* state) {
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
int push_stack(State6502* state, int size, unsigned char* byte_arr) {
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
int pop_stack(State6502* state, int size, unsigned char* byte_arr) {
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