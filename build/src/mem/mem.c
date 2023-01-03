#include "../../include/mem.h"
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

bool mem_write8(memory *m, uint16_t address, uint8_t data) { return true; }

bool mem_write16(memory *m, uint16_t address, uint8_t data) { return true; }

uint8_t mem_read8(memory *m, uint16_t address) { return 0x00; }

uint16_t mem_read16(memory *m, uint16_t address) { return 0x00; }
