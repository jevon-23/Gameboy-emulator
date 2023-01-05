#include "../include/cpu.h"
#include "../include/mem.h"
#include "../include/r_utils.h"
#include <float.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int processCLI(int argc, char **argv, char **filename) {
  if (argc != 2) {
    printf("usage: ./gb <roms/filename>");
    exit(-1);
  }
  *filename = (char *)malloc(sizeof(char *) * strlen(*(argv + 1)));
  strcpy(*filename, *(argv + 1));
  return 0;
}

int main(int arg, char *argv[]) {
  printf("Successful build!\n");

  char *filename;

  /* Process the arguments that were passed in.
   * If valid, filename will be set to the path
   * of the rom to read */
  processCLI(arg, argv, &filename);

  /* Put the file in a bit more usable format */
  aFile gameFile = readFile(filename);

  /* Create a new memory unit */
  memory *mem = new_memory();
  write_game_file(mem, gameFile); /* Write the game file to memory */
  printf("Built a new memory unit\n");

  cpu *core = new_cpu(mem);
  printf("Built a new cpu. Running game loop\n");

  /* Begin loop */
  while (true) {
    run_cpu(core);
  }
  // Test rust interface
  // char out = test_fn(0x10);
  // printf("output of test_fn: %x\n", out);
  // RV16 out = add_overflow16(0x01, 0x02);
  // printf("out: %x\n", out.rv);
  return 0;
}
