#include "../../include/cpu.h"
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
  return out;
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
