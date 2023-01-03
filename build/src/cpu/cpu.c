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
