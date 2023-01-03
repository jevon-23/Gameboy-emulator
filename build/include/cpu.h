#include "mem.h"
#include "utils.h"
#include <inttypes.h>
#ifndef CPU_H
#define CPU_H

/* Register struct */
typedef struct registers {
  uint16_t pc;        /* Progam counter */
  uint16_t sp;        /* Stack pointer */
  uint8_t gp_regs[8]; /* General purpose regs */
  /* Map gp_regs to a pointer, may remove */
  uint8_t *a;
  uint8_t *b;
  uint8_t *c;
  uint8_t *d;
  uint8_t *e;
  uint8_t *f;
  uint8_t *h;
  uint8_t *l;
  uint8_t flag; /* ZNHC.... */
} registers;

/* CPU struct */
typedef struct cpu {
  memory *mem;
  registers *regs;
  stack *stack;
} cpu;

/* CPU struct functions */
cpu *new_cpu(memory *m);

/************************/
/* Register definitions */
/************************/

/* Flag Masks */
#define Z_MASK 0x80  /* Zero flag */
#define N_MASK 0x40  /* Subtraction flag */
#define H_MASK 0x20  /* Half carry flag */
#define CY_MASK 0x10 /* Carry flag */

/* Register struct functions */
registers *new_registers();
uint16_t get_bc(registers *regs);
uint16_t get_de(registers *regs);
uint16_t get_hl(registers *regs);
uint16_t get_af(registers *regs);
void set_af(registers *regs, uint16_t val);
void set_bc(registers *regs, uint16_t val);
void set_de(registers *regs, uint16_t val);
void set_hl(registers *regs, uint16_t val);
void set_flag(registers *reg, uint8_t mask, bool set);

void run_cpu_loop(cpu *core);
void run_cpu(cpu *core);
#endif
