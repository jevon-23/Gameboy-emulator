#include "../../include/mem.h"
#include "../../include/utils.h"
#include <float.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Allocates memory for a new instance of memory, and returns a pointer to it */
memory *new_memory() {
  memory *m = (memory *)malloc(sizeof(memory));
  m->length = MEMORY_LEN;
  return m;
}

bool mem_write8(memory *m, uint16_t address, uint8_t data) {
  *(m->data + address) = data;
  return true;
}

bool mem_write16(memory *m, uint16_t address, uint16_t data) {
  uint8_t hi = (uint8_t)((data & 0xFF00) >> 8);
  uint8_t lo = (uint8_t)((data & 0x00FF));
  mem_write8(m, address, hi);
  mem_write8(m, address + 1, lo);
  return true;
}

uint8_t mem_read8(memory *m, uint16_t address) { return *(m->data + address); }

uint16_t mem_read16(memory *m, uint16_t address) {
  uint8_t hi = *(m->data + address);
  uint8_t lo = *(m->data + address + 1);
  return ((uint16_t)hi) << 8 | lo;
}

/* write a game file into memory */
void write_game_file(memory *mem, aFile gameFile) {
  memcpy((mem->data + (GAME_CODE_BANK_0_START)), *gameFile.contents,
         gameFile.fileSize);
}
