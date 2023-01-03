#include "../../include/instruction.h"
#include "../../include/cpu.h"
#include <float.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Noop instruction does nothing */
void noop(cpu *core, instruction i) { return; }

instruction new_instruction(cpu *core, uint8_t opcode) {
  instruction out;
  out.opcode = opcode;

  switch (opcode) {
  case 0x00:
    out.len = 1;
    out.fn = noop;
    break;
  }
  return out;
}
