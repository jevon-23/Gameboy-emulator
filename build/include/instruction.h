#include "cpu.h"
#include <inttypes.h>
#ifndef  INSTRUCTION_H 
#define  INSTRUCTION_H 

typedef struct instruction {
    uint8_t opcode;
    uint8_t len;
    uint8_t num_cycles;
    uint8_t full_opcode[3];

}instruction;
instruction exec_next_instruction(cpu *core, uint8_t opcode);
#endif
