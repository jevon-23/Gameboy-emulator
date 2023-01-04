#include "../../include/instruction.h"
#include "../../include/cpu.h"
#include "../../include/r_utils.h"
#include <float.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void inc_reg(cpu *core, instruction i, enum reg_enum reg, enum reg_pairs pair) {
  uint16_t r_pair = get_reg_pair(core->regs, pair);
  RV16 add_result = add_overflow16(r_pair, 1);
  set_reg_pair(core->regs, pair, add_result.rv);
}

/* Loads in a value into the register pair based on i */
void load_reg(cpu *core, instruction i, enum reg_enum reg,
              enum reg_pairs pair) {
  switch (i.len) {
  case 1:; /* Loading value from a register into address @ reg pair */
    uint16_t address = get_reg_pair(core->regs, pair);
    mem_write8(core->mem, address, *core->regs->a);
    break;
  case 3:; /* Loading nn into reg pair */
    uint16_t nn = conv8_to16(i.full_opcode[1], i.full_opcode[2]);
    set_reg_pair(core->regs, pair, nn);
    break;
  default:
    printf("load reg invalid \n");
  }
}
/* Noop instruction does nothing */
void noop(cpu *core, instruction i) { return; }

void set_instruction_vars(cpu *core, instruction *i, uint8_t len,
                          uint8_t num_cycles) {
  i->len = len;
  i->num_cycles = num_cycles;
  i->full_opcode[0] = i->opcode;
  for (int j = 1; j < i->len; j++) {
    i->full_opcode[j] = mem_read8(core->mem, (core->regs->pc + j));
  }
}

instruction exec_next_instruction(cpu *core, uint8_t opcode) {
  instruction out;
  out.opcode = opcode;

  switch (opcode) {
  case 0x00:
    set_instruction_vars(core, &out, 1, 4);
    break;
  case 0x01: /* LD BC, d16 => 0x01nnnn */
    set_instruction_vars(core, &out, 3, 12);
    load_reg(core, out, _, _BC);
    break;
  case 0x02: /* LD BC, A => 0x02 */
    set_instruction_vars(core, &out, 1, 8);
    load_reg(core, out, _A, _BC);
    break;
  case 0x03: /* INC BC */
    set_instruction_vars(core, &out, 1, 8);
    inc_reg(core, out, _, _BC);
    break;
  default:
    printf("Invalid opcode: %x\n", opcode);
  }

  core->regs->pc += out.len;
  return out;
}
