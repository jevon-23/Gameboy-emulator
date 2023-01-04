#include "../include/mem.h"
#include "../include/r_utils.h"
#include <float.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int processCLI(int argc, char **argv, char **filename) {
  if (argc != 2) {
    printf("usage:");
    exit(-1);
  }
  return 0;
}
int main(int arg, char *argv[]) {

  memory *m = new_memory();
  printf("Successful build!\n");
  // Test rust interface
  // char out = test_fn(0x10);
  // printf("output of test_fn: %x\n", out);
  RV16 out = add_overflow16(0x01, 0x02);
  printf("out: %x\n", out.rv);
  return 0;
}
