#include <inttypes.h>
#include <stdbool.h>
/*
Just a mapping between the name of a file and the contents that file holds just
to make my life a little easier in the future Going to make all of these, the
head of the list will always be the the file that we are running search on
*/

#ifndef UTILS_H // header guard
#define UTILS_H

typedef struct aFile {
  char *name;      // Name of file
  unsigned char **contents; // Contents in file
  int fileSize;
} aFile;

aFile readFile(char *fileName);
void readContents(aFile theFile);
void printSpace();

#define MAX_STACK_LEN 50
/* 16 bit stack */
typedef struct stack {
    uint16_t data[MAX_STACK_LEN];
    uint8_t len;
    uint8_t max_len;
} stack;

stack *new_stack();
bool stack_is_empty(stack *s);
bool stack_push(stack *s, uint16_t value);
uint16_t stack_pop(stack *s);
uint16_t stack_peak(stack *s);
uint16_t *get_stack_pointer(stack *s);
void set_stack_pointer(stack *s, uint16_t val);



uint8_t conv16_to8(uint16_t u, bool want_hi);
uint16_t conv8_to16(uint8_t u1, uint8_t u2);

#endif // PARSER_H
