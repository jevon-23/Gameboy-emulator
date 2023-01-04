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
    set_reg_pair(regs, _AF,  0xbeef);
    EXPECT_EQ(get_reg_pair(regs, _AF), 0xbeef);
    set_reg_pair(regs, _BC,  0x00ff);
    EXPECT_EQ(get_reg_pair(regs, _BC), 0x00ff);
    EXPECT_EQ(get_reg_pair(regs, _DE), 0x0000);
}

TEST(setFlagTest, setFlag) {
    registers *regs = new_registers();
    set_flag(regs, Z_MASK, true);
    EXPECT_EQ(regs->flag, Z_MASK);
    set_flag(regs, N_MASK, true);
    EXPECT_EQ(regs->flag, Z_MASK | N_MASK);
    set_flag(regs, Z_MASK, false);
    EXPECT_EQ(regs->flag, N_MASK);
}

}
