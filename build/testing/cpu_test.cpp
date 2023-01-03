#include <gtest/gtest.h>
extern "C" {
#include "../include/cpu.h"
// #include "../../../include/utils.h"
}
namespace {

TEST(buildCPUTest, buildCpu) {
  memory *m = new_memory();
  cpu *c = new_cpu(m);
  for (int i = 0; i < MEMORY_LEN; i++) {
    EXPECT_EQ(c->mem->data[i], 0);
  }
}

TEST(regComboTest, regCombo) {
    registers *regs = new_registers();
    set_af(regs, 0xbeef);
    EXPECT_EQ(get_af(regs), 0xbeef);
    set_bc(regs, 0x00ff);
    EXPECT_EQ(get_bc(regs), 0x00ff);
    EXPECT_EQ(get_de(regs), 0x0000);
}

}
