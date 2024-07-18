#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>


static int Disassembler6502(unsigned char *buffer, int pc) {

    unsigned char *opcodes = &buffer[pc];
    int opbytes = 1;

    switch (opcodes[0]) {
        // James implementation
        case 0x00: printf("Not yet implemented\n"); break;
        case 0x01: printf("Not yet implemented\n"); break;
        case 0x05: printf("Not yet implemented\n"); break;
        case 0x06: printf("Not yet implemented\n"); break;
        case 0x08: printf("Not yet implemented\n"); break;
        case 0x09: printf("Not yet implemented\n"); break;
        case 0x0a: printf("Not yet implemented\n"); break;
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
        case 0x2c: printf("Not yet implemented\n"); break;
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
        case 0x58: printf("Not yet implemented\n"); break;
        case 0x59: printf("Not yet implemented\n"); break;
        case 0x5d: printf("Not yet implemented\n"); break;
        case 0x5e: printf("Not yet implemented\n"); break;
        case 0x60: printf("Not yet implemented\n"); break;
        case 0x61: printf("Not yet implemented\n"); break;
        case 0x65: printf("Not yet implemented\n"); break;
        case 0x66: printf("Not yet implemented\n"); break;
        case 0x68: printf("Not yet implemented\n"); break;
        case 0x69: printf("Not yet implemented\n"); break;
        case 0x6a: printf("Not yet implemented\n"); break;
        case 0x6c: printf("Not yet implemented\n"); break;
        case 0x6d: printf("Not yet implemented\n"); break;
        case 0x6e: printf("Not yet implemented\n"); break;
        case 0x70: printf("Not yet implemented\n"); break;
        case 0x71: printf("Not yet implemented\n"); break;
        case 0x75: printf("Not yet implemented\n"); break;
        case 0x76: printf("Not yet implemented\n"); break;
        case 0x78: printf("Not yet implemented\n"); break;
        case 0x79: printf("Not yet implemented\n"); break;
        case 0x7d: printf("Not yet implemented\n"); break;
        case 0x7e: printf("Not yet implemented\n"); break;
        case 0x81: printf("Not yet implemented\n"); break;
        case 0x84: printf("Not yet implemented\n"); break;
        case 0x85: printf("Not yet implemented\n"); break;
        
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
        default: printf("Invalid opcode: %02x\n", opcodes[0]); break;


    }
}

int main (int argc, char**argv) {
    FILE *file = fopen(argv[1], "rb");
    if (file == NULL) {
        printf("Error: Couldn't open %s\n", argv[1]);
        exit(1);
    }

    // get the file size and read it into a memory buffer
    fseek(file, 0L, SEEK_END);
    int fsize = ftell(file);
    fseek(file, 0L, SEEK_SET);

    unsigned char *buffer = malloc(fsize);
    fread(buffer, fsize, 1, file);
    fclose(file);

    int pc = 0;

    while (pc < fsize) {
        pc += Disassembler6502(buffer, pc);
    }
    
    return 0;
}