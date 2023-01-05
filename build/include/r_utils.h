#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct RV8 {
  uint8_t rv;
  bool over_flow;
} RV8;

typedef struct RV16 {
  uint16_t rv;
  bool over_flow;
} RV16;

struct RV8 sub_overflow8(uint8_t arg1, uint8_t arg2);

struct RV16 sub_overflow16(uint16_t arg1, uint16_t arg2);

struct RV8 add_overflow8(uint8_t arg1, uint8_t arg2);

struct RV16 add_overflow16(uint16_t arg1, uint16_t arg2);

uint8_t test_fn(uint8_t arg);
