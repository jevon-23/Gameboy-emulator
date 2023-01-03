#include "../../include/cpu.h"
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

/* Used for testing and main b4 I add drawing */
void run_cpu_loop(cpu *core) {
  /* Program counter is increased based on the instruction being exec.
   * So get the opcode first, => decode instruction after
   * => update program counter, => execute instruction
   */

  /* fetch */
  uint8_t opcode = mem_read8(core->mem, core->regs->pc);

  /* decode */
  instruction next_instruction = new_instruction(core, opcode);
  core->regs->pc += next_instruction.len;

  /* 0x00 == noop, but will be used as quit for testing */
  while (opcode != 0x00) {
    /* Execute */
    next_instruction.fn(core, next_instruction);

    uint8_t opcode = mem_read8(core->mem, core->regs->pc);

    /* decode */
    instruction next_instruction = new_instruction(core, opcode);
    core->regs->pc += next_instruction.len;
  }
}

void run_cpu(cpu *core) {
  /* Program counter is increased based on the instruction being exec.
   * So get the opcode first, => decode instruction after
   * => update program counter, => execute instruction
   */

  /* fetch */
  uint8_t opcode = mem_read8(core->mem, core->regs->pc);

  /* decode */
  instruction next_instruction = new_instruction(core, opcode);
  core->regs->pc += next_instruction.len;

  /* Execute */
  next_instruction.fn(core, next_instruction);
}

/**************************/
/* Register functionality */
/**************************/

void set_regs(uint16_t val, uint8_t *hi, uint8_t *lo) {
  *hi = conv16_to8(val, true);
  *lo = conv16_to8(val, false);
}

uint16_t get_af(registers *regs) { return conv8_to16(*regs->a, *regs->f); }
uint16_t get_bc(registers *regs) { return conv8_to16(*regs->b, *regs->c); }
uint16_t get_de(registers *regs) { return conv8_to16(*regs->d, *regs->e); }
uint16_t get_hl(registers *regs) { return conv8_to16(*regs->h, *regs->l); }
void set_af(registers *regs, uint16_t val) { set_regs(val, regs->a, regs->f); }
void set_bc(registers *regs, uint16_t val) { set_regs(val, regs->b, regs->c); }
void set_de(registers *regs, uint16_t val) { set_regs(val, regs->d, regs->e); }
void set_hl(registers *regs, uint16_t val) { set_regs(val, regs->h, regs->l); }

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
