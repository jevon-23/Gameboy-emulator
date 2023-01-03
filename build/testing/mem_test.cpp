#include <gtest/gtest.h>
extern "C" {
#include "../include/mem.h"
// #include "../../../include/utils.h"
}

namespace {

TEST(buildMemTest, buildMem) {
  memory *out = new_memory();
  for (int i = 0; i < MEMORY_LEN; i++) {
    EXPECT_EQ(out->data[i], 0);
  }
}

TEST(rwTest, rw) {
  memory *m = new_memory();
  uint8_t b1 = 0xbe;
  uint8_t b2 = 0xef;

  uint16_t addy = 0xf123;
  mem_write8(m, addy, b1);
  mem_write8(m, addy + 1, b2);

  EXPECT_EQ(mem_read8(m, addy), b1);
  EXPECT_EQ(mem_read8(m, addy+1), b2);

  uint16_t s1 = 0xdead;
  uint16_t s2 = 0xbeef;
  uint16_t s = mem_read16(m, addy);
  EXPECT_EQ(s, s2);

  addy = addy + 10;
  mem_write16(m, addy, s1);
  mem_write16(m, addy + 2, s);

  EXPECT_EQ(addy, 0xf123 + 10);
  EXPECT_EQ(mem_read16(m, addy), s1);
  EXPECT_EQ(mem_read16(m, addy+2), s2);
}

TEST(writeGameFileTest, writeGameFile) {
  memory *testMem = new_memory();
  char fp[200];
  getcwd(fp, sizeof(fp));
  strcat(fp, "/build/testing/utils_help.txt");
  aFile theFile = readFile(fp);
  write_game_file(testMem, theFile);
  EXPECT_EQ('g', *(testMem->data + GAME_ROM_BANK_0_START));
}

} // namespace
