#include "../../include/utils.h"
#include <ctype.h>
#include <float.h>
#include <regex.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAX_PROGRAM_LEN 500000000
// If we run into an error, return a message and exit program.
void error(int code) {
  switch (code) {
  case 0: // Invalid .extension
    printf("Invalid file type. We want this to work for .py, .java, .c, and "
           ".golang sometime in the future, but a sof rn only .c files are "
           "available");
  default:
    printf("Something went wrong");
  }
  exit(-1);
}

// Print a seperator
void printSpace() {
  printf("\n~~~~~~~~~~~~~~~~~~~~~~~~");
  printf("\n");
}

/***********************/
/* AFILE functionality */
/***********************/
/*
Reads the contents of theFile.
*/
void readContents(aFile theFile) {
  printf("printing %d chars\n", theFile.fileSize);
  for (int i = 0; i < theFile.fileSize; i++) {
    printf("char %d = %x\n", i, *(*theFile.contents + i));
  }
}

/*
Reads the file, stores the name and its contents inside a AFILE struct
*/
aFile readFile(char *fileName) {
  printf("fileName = %s\n", fileName);
  // Opening the file that we are reading
  FILE *currFile = fopen(fileName, "rb");
  if (currFile == NULL) {
    error(1);
  }

  // Creating a new afile instance. Making room for the first line to be read
  // from file
  aFile theFile;
  theFile.name = fileName;
  theFile.contents = (unsigned char **)malloc(
      sizeof(unsigned char *)); // The content that fileName holds
  *theFile.contents =
      (unsigned char *)malloc((sizeof(unsigned char) * MAX_PROGRAM_LEN));
  // Read this line from the file, and storing it at a value inside of
  // theContents
  theFile.fileSize = fread(*theFile.contents, sizeof(unsigned char),
                           MAX_PROGRAM_LEN, currFile);
  fclose(currFile);
  printf("read %s\n", fileName);
  return theFile;
}

/***********************/
/* Stack Functionality */
/***********************/
stack *new_stack() {
  stack *out = (stack *)malloc(sizeof(stack));
  out->len = 0;
  out->max_len = MAX_STACK_LEN;
  return out;
}

bool stack_is_empty(stack *s) {
  if (s->len == 0)
    return true;
  else
    return false;
}

bool stack_push(stack *s, uint16_t value) {
  if (s->len >= s->max_len)
    return false;

  s->data[s->len] = value;
  s->len++;
  return true;
}

uint16_t stack_peak(stack *s) {
  if (stack_is_empty(s))
    return 0;
  return s->data[s->len - 1];
}

uint16_t stack_pop(stack *s) {
  if (stack_is_empty(s))
    return 0;

  uint16_t out = s->data[s->len - 1];
  s->len--;
  return out;
}

/**********************/
/* Misc Functionality */
/**********************/
uint8_t conv16_to8(uint16_t u, bool want_hi) {
  if (want_hi)
    return (uint8_t)((u & 0xFF00) >> 8);
  return (uint8_t)(u & 0x00FF);
}

uint16_t conv8_to16(uint8_t u1, uint8_t u2) {
  uint16_t out = (uint16_t)(u1) << 8 | u2;
  return out;
}
