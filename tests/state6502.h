#ifndef STATE6502_H
#define STATE6502_H

#include <stdint.h>
#include <stdbool.h>
#include "flags.h"

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

#endif