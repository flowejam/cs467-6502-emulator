#ifndef HELPER6502_H
#define HELPER6502_H

#include "state6502.h"

void update_processor_status(State6502* state);
int push_stack(State6502* state, int size, unsigned char* byte_arr);
int pop_stack(State6502* state, int size, unsigned char* byte_arr);

#endif
