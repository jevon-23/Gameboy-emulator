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

TEST(inc16Test, inc16) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* ld BC, nnnn => 0x01nnnn */
    mem_write8(c->mem, c->regs->pc, 0x01); /* load BC nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0xbe);
    mem_write8(c->mem, c->regs->pc+2, 0xef);
    mem_write8(c->mem, c->regs->pc+3, 0x03); /* Increase BC + 1 */
    mem_write8(c->mem, c->regs->pc+4, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(c->regs->pc, GAME_ROM_BANK_0_START + 5);
    EXPECT_EQ(get_reg_pair(c->regs, _BC), 0xbeef+1);

    /* ld BC, nnnn => 0x01nnnn */
    mem_write8(c->mem, c->regs->pc, 0x01); /* load BC nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0xff);
    mem_write8(c->mem, c->regs->pc+2, 0xff);
    mem_write8(c->mem, c->regs->pc+3, 0x03); /* Increase BC + 1 */
    mem_write8(c->mem, c->regs->pc+4, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(c->regs->pc, GAME_ROM_BANK_0_START + 10);
    EXPECT_EQ(get_reg_pair(c->regs, _BC), 0x00);

}

TEST(loadBCTest, loadBC) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* ld BC, nnnn => 0x01nnnn */
    mem_write8(c->mem, c->regs->pc, 0x01); /* load BC nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0xbe);
    mem_write8(c->mem, c->regs->pc+2, 0xef);
    mem_write8(c->mem, c->regs->pc+3, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(c->regs->pc, GAME_ROM_BANK_0_START + 4);
    EXPECT_EQ(get_reg_pair(c->regs, _BC), 0xbeef);
    /* No ld A yet */
    *c->regs->a = 0xaf;
    /* Load A reg into address @ BC reg pair */
    mem_write8(c->mem, c->regs->pc, 0x02); /* Load *(BC), A */
    mem_write8(c->mem, c->regs->pc+1, 0x00);
    run_cpu_loop(c);
    EXPECT_EQ(c->regs->pc, GAME_ROM_BANK_0_START + 6);
    EXPECT_EQ(get_reg_pair(c->regs, _BC), 0xbeef);
    EXPECT_EQ(mem_read8(c->mem, get_reg_pair(c->regs, _BC)), 0xaf);
}

}
