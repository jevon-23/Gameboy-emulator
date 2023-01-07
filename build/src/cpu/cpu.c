#include "../../include/instruction.h"
#include "../../include/mem.h"
#include "../../include/utils.h"
#include <float.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

cpu *new_cpu(memory *m) {
  cpu *out = (cpu *)malloc(sizeof(cpu));
  out->mem = m;
  out->regs = new_registers();
  out->stack = new_stack();
  out->state = _RUNNING;
  out->valid_daa = false;

  /* Set the regiseters */
  out->regs->pc = GAME_ROM_BANK_0_START;
  out->regs->sp = 0x00;
  return out;
}

/* Can simply index into out->regs, but can also use letter values
 * if that makes it a bit more easier */
registers *new_registers() {
  registers *out = (registers *)malloc(sizeof(registers));
  out->a = out->gp_regs;
  out->b = (out->gp_regs + 1);
  out->c = (out->gp_regs + 2);
  out->d = (out->gp_regs + 3);
  out->e = (out->gp_regs + 4);
  out->f = (out->gp_regs + 5);
  out->h = (out->gp_regs + 6);
  out->l = (out->gp_regs + 7);
  out->flag = 0x00;
  return out;
}

/**********************************/
/* CPU fetch / decode / exec loop */
/**********************************/

bool check_low_power(cpu *core, uint8_t opcode) {
  if (core->state == _STOP)
    /* TODO: Check for reset with opcode */
    return true;
  return false;
}

/* Used for testing and main b4 I add drawing */
void run_cpu_loop(cpu *core) {
  /* Program counter is increased based on the instruction being exec.
   * So get the opcode first, => decode instruction after
   * => update program counter, => execute instruction
   */

  /* fetch */
  uint8_t opcode = 1;

  /* 0x00 == noop, but will be used as quit for testing */
  while (opcode != 0x00) {
    opcode = mem_read8(core->mem, core->regs->pc);

    /* Inside of low power mode, do not exec next instruction
     * unless we have a reset or movement */
    if (check_low_power(core, opcode))
      continue;

    /* decode & exec */
    exec_next_instruction(core, opcode);
  }
}

void run_cpu(cpu *core) {
  /* Program counter is increased based on the instruction being exec.
   * So get the opcode first, => decode instruction after
   * => update program counter, => execute instruction
   */

  /* fetch */
  uint8_t opcode = mem_read8(core->mem, core->regs->pc);

  /* Inside of low power mode, do not exec next instruction
   * unless we have a reset or movement */
  if (check_low_power(core, opcode))
    return;

  /* decode & execute */
  exec_next_instruction(core, opcode);
  // instruction i = exec_next_instruction(core, opcode);
  // printf("Opcode executed: %x\n", i.opcode);
}

/**************************/
/* Register functionality */
/**************************/

uint16_t get_af(registers *regs) { return conv8_to16(*regs->a, *regs->f); }
uint16_t get_bc(registers *regs) { return conv8_to16(*regs->b, *regs->c); }
uint16_t get_de(registers *regs) { return conv8_to16(*regs->d, *regs->e); }
uint16_t get_hl(registers *regs) { return conv8_to16(*regs->h, *regs->l); }

/* Takes in pointers to the registers */
void set_regs(uint16_t val, uint8_t *hi, uint8_t *lo) {
  *hi = conv16_to8(val, true);
  *lo = conv16_to8(val, false);
}

void set_af(registers *regs, uint16_t val) { set_regs(val, regs->a, regs->f); }
void set_bc(registers *regs, uint16_t val) { set_regs(val, regs->b, regs->c); }
void set_de(registers *regs, uint16_t val) { set_regs(val, regs->d, regs->e); }
void set_hl(registers *regs, uint16_t val) { set_regs(val, regs->h, regs->l); }

void set_reg(registers *regs, enum reg_enum r, uint8_t v) {
  *(get_reg(regs, r)) = v;
}
uint8_t *get_reg(registers *regs, enum reg_enum r) {
  switch (r) {
  case _A:
    return regs->a;
  case _B:
    return regs->b;
  case _C:
    return regs->c;
  case _D:
    return regs->d;
  case _E:
    return regs->e;
  case _F:
    return regs->f;
  case _H:
    return regs->h;
  case _L:
    return regs->l;
  default:
    printf("Invalid register passed in\n");
    exit(-1);
  }
}

uint16_t get_reg_pair(registers *regs, enum reg_pairs pair) {
  switch (pair) {
  case _AF:
    return get_af(regs);
  case _BC:
    return get_bc(regs);
  case _DE:
    return get_de(regs);
  case _HL:
    return get_hl(regs);
  default:
    printf("invalid register passed in\n");
    exit(-1);
  }
}

void set_reg_pair(registers *regs, enum reg_pairs pair, uint16_t val) {
  switch (pair) {
  case _AF:
    return set_af(regs, val);
  case _BC:
    return set_bc(regs, val);
  case _DE:
    return set_de(regs, val);
  case _HL:
    return set_hl(regs, val);
  default:
    printf("invalid register passed in to pair\n");
    exit(-1);
  }
}

bool get_flag(registers *reg, uint8_t mask) { return reg->flag & mask; }

/* Each flag is only one bit. if set == true
 * => Our flag == our bit being on => (mask * set) = mask
 * if set == false => bit being off => (mask * set) = 0
 */
void set_flag(registers *reg, uint8_t mask, bool set) {
  if (set)
    reg->flag |= mask;
  else
    reg->flag &= ~mask;
}

void set_all_flags(registers *reg, int z, int n, int h, int cy) {
  /* 2 => unused */
  if (z < 2)
    set_flag(reg, Z_MASK, z);
  if (n < 2)
    set_flag(reg, N_MASK, n);
  if (h < 2)
    set_flag(reg, H_MASK, h);
  if (cy < 2)
    set_flag(reg, CY_MASK, cy);
}
