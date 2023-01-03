#include <gtest/gtest.h>
#include <string.h>
extern "C" {
#include "../include/instruction.h"
#include "../include/cpu.h"
#include "../include/mem.h"
#include <unistd.h>
}

namespace {
TEST(newInstructionTest, newInstruction) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    mem_write8(c->mem, GAME_ROM_BANK_0_START, 0x00);
    run_cpu_loop(c);
    // i.fn(c, i);
   
    EXPECT_EQ(c->regs->pc, GAME_ROM_BANK_0_START + 1);
}
}
