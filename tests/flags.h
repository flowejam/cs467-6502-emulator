#ifndef FLAGS_H
#define FLAGS_H

#include <stdint.h>

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

#endif

