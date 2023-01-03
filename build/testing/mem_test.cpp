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

// TEST(writeGameFileTest, writeGameFile) {
//   mem *testMem = buildMem();
//   char fp[200];
//   getcwd(fp, sizeof(fp));
//   strcat(fp, "/src/memory/test/help.txt");
//   aFile theFile = readFile(fp);
//   writeGameFile(&testMem, theFile);
//   EXPECT_EQ('g', *(testMem->RAM + (interpEnding)));
// }

} // namespace
