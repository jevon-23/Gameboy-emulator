#include "cpu.h"
#include <inttypes.h>
#ifndef  INSTRUCTION_H 
#define  INSTRUCTION_H 

typedef struct arguments {
    enum reg_enum src_reg;
    enum reg_enum dst_reg;
    enum reg_pairs src_pair;
    enum reg_pairs dst_pair;
} arguments;

typedef struct instruction {
    uint8_t opcode;
    uint8_t len;
    uint8_t num_cycles;
    uint8_t full_opcode[3];
    arguments args;

}instruction;
instruction exec_next_instruction(cpu *core, uint8_t opcode);
#endif
