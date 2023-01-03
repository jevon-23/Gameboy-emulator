#include <inttypes.h>
#include <stdbool.h>
#ifndef MEM_H
#define MEM_H

#define MEMORY_LEN 0x10000
typedef struct memory {
  uint8_t data[MEMORY_LEN]; // We want access from 0x0000 - 0xffff
  int length;
} memory;

/***************************/
/* Memory struct functions */
/***************************/
memory *new_memory();

bool mem_write8(memory *m, uint16_t address, uint8_t data);

bool mem_write16(memory *m, uint16_t address, uint8_t data);

uint8_t mem_read8(memory *m, uint16_t address);

uint16_t mem_read16(memory *m, uint16_t address);

/*********************/
/* Memory boundaries */
/*********************/

/* Boot rom boundaries. Boot rom is overwrtitten after
 * initial boot
 */
#define BOOT_ROM_START = 0x00
#define BOOT_ROM_END = 0xFF

/* Game ROM Bank 0. Split up into 3 places,
 *  1. Interrupt table
 *  2. Cartridge Header
 *  3. Game Code
 */

#define GAME_ROM_BANK_0_START 0x0000 // (0)
#define INTERRUPE_TABLE_START 0x0000 // (1)
#define INTERRUPE_TABLE_END 0x00FF
#define CARTDIGE_HEADER_START 0x0100 // (2)
#define CARTDIGE_HEADER_END 0x014F
#define GAME_CODE_BANK_0_START 0x0150 // (3)
#define GAME_CODE_BANK_0_END 0x03FFF
#define GAME_ROM_BANK_0_END 0x3FFF // (0)

/* Game ROM Bank n. Memory that can be swapped out that's
 * game code
 */
#define GAME_BANK_ROM_N_START 0x4000
#define GAME_BANK_ROM_N_END 0x7FFF

/* Tile RAM, holds data about graphics */
#define TILE_RAM_START 0x8000
#define TILE_RAM_END 0x97FF

/* Background map, used to map tiles to sections of the screen */
#define BACKGRUOND_MAP_START 0x9800
#define BACKGRUOND_MAP_END 0x9FFF

/* Cartridge Ram */
#define CARTRIDGE_RAM_START 0xA000
#define CARTRIDGE_RAM_END 0xBFFF

/* Working Ram, the ram that the gb has access to */
#define WORKING_RAM_START 0xC000
#define WORKING_RAM_END 0xDFFF

/* Echo Ram, redudancy for working ram */
#define ECHO_RAM_START 0xE000
#define ECHO_RAM_END 0xFDFF

/* Object attr mem, contains descriptions of graphical sprites */
#define OAM_START 0xFE00
#define OAM_END 0xFE9F

#define UNUSED_SECTION_START 0xFEA0
#define UNUSED_SECTION_END 0xFEFF

/* I/o Registers */
#define IO_REGS_START 0xFF00
#define IO_REGS_END 0xFF7F

/* High ram area */
#define HIGH_RAM_START 0xFF80
#define HIGH_RAM_END 0xFFFE

/* Interrupt reg */
#define INTERRUPT_REG 0xFFFF
#endif
