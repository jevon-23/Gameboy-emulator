#include <gtest/gtest.h>
#include <string.h>
extern "C" {
#include "../include/instruction.h"
#include "../include/cpu.h"
#include "../include/mem.h"
#include "../include/utils.h"
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

TEST(daaTest, daa) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* Check add */
    /* Ld b = 0xbe */
    /* set_reg(c->regs, _B, 0xbe); */
    mem_write8(c->mem, c->regs->pc, 0x06); /* Load B = 0xde */
    mem_write8(c->mem, c->regs->pc+1, 0xde); 
    mem_write8(c->mem, c->regs->pc+2, 0x04); /* Increase B + 1 */
    mem_write8(c->mem, c->regs->pc+3, 0x27); /* daa */
    mem_write8(c->mem, c->regs->pc+4, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _B)), 0xde +1);
    EXPECT_EQ(c->regs->flag, CY_MASK);
    EXPECT_EQ(*c->regs->a, (uint8_t) (0xde + 1 + 0x60 + 0x6));
}

TEST(jumpRelativeTest, jumpRelative) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* jr nn => forward */
    mem_write8(c->mem, c->regs->pc, 0x18); /* jump relative */
    mem_write8(c->mem, c->regs->pc+1, 0x20); /* nn */
    mem_write8(c->mem, c->regs->pc+ 0x20, 0x00); /* end */
    run_cpu_loop(c);
    /* Have to read 2 bytes before the jump, and 1 for the close */
    EXPECT_EQ(c->regs->pc, GAME_ROM_BANK_0_START + 0x20 + 3); 

    /* jr nn => backward */
    mem_write8(c->mem, c->regs->pc, 0x18); /* jump relative */
    mem_write8(c->mem, c->regs->pc+1, -0x20); /* nn */
    mem_write8(c->mem, c->regs->pc+1 - 0x20, 0x00); /* end */
    run_cpu_loop(c);
    EXPECT_EQ(c->regs->pc, GAME_ROM_BANK_0_START + 6);

    uint16_t new_pc = c->regs->pc;
    /* jr cc nn => dont jump if cc is set */
    set_flag(c->regs, Z_MASK, true);
    mem_write8(c->mem, c->regs->pc, 0x20); /* jump relative */
    mem_write8(c->mem, c->regs->pc+1, 0xff); /* nn */
    mem_write8(c->mem, c->regs->pc+2, 0x00); /* end */
    run_cpu_loop(c);
    EXPECT_EQ(c->regs->pc, new_pc + 3);

    new_pc = c->regs->pc;
    /* jr cc nn => jump if cc is not set */
    set_flag(c->regs, Z_MASK, false);
    mem_write8(c->mem, c->regs->pc, 0x20); /* jump relative */
    mem_write8(c->mem, c->regs->pc+1, 0x30); /* nn */
    mem_write8(c->mem, c->regs->pc+2, 0x00); /* end */
    mem_write8(c->mem, c->regs->pc+1 + 0x30, 0x00); /* end */
    run_cpu_loop(c);
    EXPECT_EQ(c->regs->pc, new_pc + 0x30 + 3);

    new_pc = c->regs->pc;
    /* jr cc nn => dont jump if cc is not set */
    set_flag(c->regs, Z_MASK, false);
    mem_write8(c->mem, c->regs->pc, 0x28); /* jump relative */
    mem_write8(c->mem, c->regs->pc+1, 0xff); /* nn */
    mem_write8(c->mem, c->regs->pc+2, 0x00); /* end */
    run_cpu_loop(c);
    EXPECT_EQ(c->regs->pc, new_pc + 3);

    new_pc = c->regs->pc;
    /* jr cc nn => jump if cc is set */
    set_flag(c->regs, Z_MASK, true);
    mem_write8(c->mem, c->regs->pc, 0x28); /* jump relative */
    mem_write8(c->mem, c->regs->pc+1, 0x30); /* nn */
    mem_write8(c->mem, c->regs->pc+2, 0x00); /* end */
    mem_write8(c->mem, c->regs->pc+1 + 0x30, 0x00); /* end */
    run_cpu_loop(c);
    EXPECT_EQ(c->regs->pc, new_pc + 0x30 + 3);

}

TEST(stopTest, stop) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    mem_write8(c->mem, c->regs->pc, 0x10); /* stop */
    run_cpu(c);
    EXPECT_EQ(c->state, _STOP);
}

TEST(shiftRightTest, shiftRight) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* ld a, nn => 0x06nn */
    set_reg(c->regs, _A, 0x85);
    mem_write8(c->mem, c->regs->pc, 0x0f); /* shift A 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _A)), 0x42);
    EXPECT_EQ(c->regs->flag, CY_MASK);

    set_reg(c->regs, _A, 0x44);
    mem_write8(c->mem, c->regs->pc, 0x0f); /* shift A 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _A)), 0x22);
    EXPECT_EQ(c->regs->flag, 0);
}



TEST(dec16Test, dec16) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* ld BC, nnnn => 0x01nnnn */
    mem_write8(c->mem, c->regs->pc, 0x01); /* load BC nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0xbe);
    mem_write8(c->mem, c->regs->pc+2, 0xef);
    mem_write8(c->mem, c->regs->pc+3, 0x0B); /* Decrease BC - 1 */
    mem_write8(c->mem, c->regs->pc+4, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(get_reg_pair(c->regs, _BC), 0xbeef-1);

    /* ld BC, nnnn => 0x01nnnn */
    mem_write8(c->mem, c->regs->pc, 0x01); /* load BC nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0x00);
    mem_write8(c->mem, c->regs->pc+2, 0x00);
    mem_write8(c->mem, c->regs->pc+3, 0x0B); /* Increase BC + 1 */
    mem_write8(c->mem, c->regs->pc+4, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(get_reg_pair(c->regs, _BC), 0xffff);

    /* ld DE, nnnn => 0x01nnnn */
    mem_write8(c->mem, c->regs->pc, 0x11); /* load DE nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0xbe);
    mem_write8(c->mem, c->regs->pc+2, 0xaf);
    mem_write8(c->mem, c->regs->pc+3, 0x1B); /* Decrease BC - 1 */
    mem_write8(c->mem, c->regs->pc+4, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(get_reg_pair(c->regs, _DE), 0xbeaf-1);
}


TEST(loadA_addyTest, loadA_addy) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* Load A into memory */
    set_reg(c->regs, _A, 0xaf);
    set_reg_pair(c->regs, _BC, 0xbeef);
    mem_write8(c->mem, c->regs->pc, 0x2); /* load BC nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(get_reg_pair(c->regs, _BC), 0xbeef);
    EXPECT_EQ(mem_read8(c->mem, 0xbeef), 0xaf);
    EXPECT_EQ( *(get_reg(c->regs, _A)), 0xaf);

    /* Load A into memory */
    set_reg(c->regs, _A, 0xb3);
    set_reg_pair(c->regs, _DE, 0xbeaf);
    mem_write8(c->mem, c->regs->pc, 0x12); /* load DE nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ( *(get_reg(c->regs, _A)), 0xb3);
    EXPECT_EQ(get_reg_pair(c->regs, _DE), 0xbeaf);
    EXPECT_EQ(mem_read8(c->mem, 0xbeaf), 0xb3);

    /* Load A into memory  and increment HL */
    set_reg(c->regs, _A, 0x06);
    set_reg_pair(c->regs, _HL, 0x9999);
    mem_write8(c->mem, c->regs->pc, 0x22); /* load HL++ A */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ( *(get_reg(c->regs, _A)), 0x06);
    EXPECT_EQ(get_reg_pair(c->regs, _HL), 0x9999+1);
    EXPECT_EQ(mem_read8(c->mem, 0x9999), 0x06);
}

TEST(addRegPairTest, addRegPair) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* ld hl, 8a23 */
    set_reg_pair(c->regs, _HL, 0x8a23);

    /* ld BC, nnnn => 0x01nnnn */
    mem_write8(c->mem, c->regs->pc, 0x01); /* load BC 0605 */
    mem_write8(c->mem, c->regs->pc+1, 0x06);
    mem_write8(c->mem, c->regs->pc+2, 0x05);
    mem_write8(c->mem, c->regs->pc+3, 0x09); /* HL = HL + BC */
    mem_write8(c->mem, c->regs->pc+4, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(get_reg_pair(c->regs, _BC), 0x0605);
    EXPECT_EQ(get_reg_pair(c->regs, _HL), 0x9028);
    EXPECT_EQ(c->regs->flag, H_MASK);

    set_reg_pair(c->regs, _HL, 0x8a23);
    mem_write8(c->mem, c->regs->pc, 0x01); /* load BC 0605 */
    mem_write8(c->mem, c->regs->pc+1, 0x8a);
    mem_write8(c->mem, c->regs->pc+2, 0x23);
    mem_write8(c->mem, c->regs->pc+3, 0x09); /* HL = HL + BC */
    mem_write8(c->mem, c->regs->pc+4, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(get_reg_pair(c->regs, _BC), 0x8a23);
    EXPECT_EQ(get_reg_pair(c->regs, _HL), 0x1446);
    EXPECT_EQ(c->regs->flag, H_MASK | CY_MASK);

    /* ld hl, 8a23 */
    set_reg_pair(c->regs, _HL, 0x8a23);

    /* ld DE, nnnn => 0x11nnnn */
    mem_write8(c->mem, c->regs->pc, 0x11); /* load DE 0605 */
    mem_write8(c->mem, c->regs->pc+1, 0x06);
    mem_write8(c->mem, c->regs->pc+2, 0x05);
    mem_write8(c->mem, c->regs->pc+3, 0x19); /* HL = HL + DE */
    mem_write8(c->mem, c->regs->pc+4, 0x00); /* NOOP => quit */
    set_flag(c->regs, N_MASK, true); /* Set sub flag 4 a check */
    run_cpu_loop(c);
    EXPECT_EQ(get_reg_pair(c->regs, _DE), 0x0605);
    EXPECT_EQ(get_reg_pair(c->regs, _HL), 0x9028);
    EXPECT_EQ(c->regs->flag, H_MASK);
}

TEST(loadStack16Test, loadStack16) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    stack_push(c->stack, 0xbeef);
    /* ld BC, nnnn => 0x01nnnn */
    mem_write8(c->mem, c->regs->pc, 0x08); /* load BC nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0xde);
    mem_write8(c->mem, c->regs->pc+2, 0xad);
    mem_write8(c->mem, c->regs->pc+3, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(mem_read16(c->mem, 0xdead), 0xefbe);
    EXPECT_EQ(stack_peak(c->stack), 0xbeef);
}

TEST(shiftLeftTest, shiftLeft) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* ld a, nn => 0x06nn */
    set_reg(c->regs, _A, 0x85);
    mem_write8(c->mem, c->regs->pc, 0x07); /* shift A 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _A)), 0x0A);
    EXPECT_EQ(c->regs->flag, CY_MASK);

    set_reg(c->regs, _A, 0x45);
    mem_write8(c->mem, c->regs->pc, 0x07); /* shift A 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _A)), 0x8A);
    EXPECT_EQ(c->regs->flag, 0);
}

TEST(load8Test, load8) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* ld B, nn => 0x06nn */
    mem_write8(c->mem, c->regs->pc, 0x06); /* load B nn */
    mem_write8(c->mem, c->regs->pc+1, 0xbe);
    mem_write8(c->mem, c->regs->pc+2, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _B)), 0xbe);

    /* ld C, nn => 0x06nn */
    mem_write8(c->mem, c->regs->pc, 0x0e); /* load B nn */
    mem_write8(c->mem, c->regs->pc+1, 0xef);
    mem_write8(c->mem, c->regs->pc+2, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _C)), 0xef);
}

TEST(dec8Test, dec8) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);

    /* Check sub */
    set_reg(c->regs, _B, 0xbe);
    mem_write8(c->mem, c->regs->pc, 0x05); /* Increase B + 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _B)), 0xbe -1);
    EXPECT_EQ(c->regs->flag, N_MASK);

    /* Check H flag */
    set_reg(c->regs, _B, 0x10);
    mem_write8(c->mem, c->regs->pc, 0x05); /* Increase B + 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _B)), 0x0f);
    EXPECT_EQ(c->regs->flag, N_MASK | H_MASK);

    /* Check Z flag */
    set_reg(c->regs, _B, 0x01);
    mem_write8(c->mem, c->regs->pc, 0x05); /* Increase B + 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _B)), 0x00);
    EXPECT_EQ(c->regs->flag, N_MASK | Z_MASK);

    set_reg(c->regs, _C, 0x21);
    mem_write8(c->mem, c->regs->pc, 0x0d); /* Decrease B + 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _C)), 0x21-1);
    EXPECT_EQ(c->regs->flag, N_MASK);
}

TEST(inc8Test, inc8) {
    memory *m = new_memory();
    cpu *c = new_cpu(m);
    /* Check add */
    /* Ld b = 0xbe */
    set_reg(c->regs, _B, 0xbe);
    mem_write8(c->mem, c->regs->pc, 0x04); /* Increase B + 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _B)), 0xbe +1);
    EXPECT_EQ(c->regs->flag, 0x00);

    /* Check H flag */
    set_reg(c->regs, _B, 0x0f);
    mem_write8(c->mem, c->regs->pc, 0x04); /* Increase B + 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _B)), 0x10);
    EXPECT_EQ(c->regs->flag, H_MASK);

    /* Check 0 flag */
    set_reg(c->regs, _B, 0xff);
    mem_write8(c->mem, c->regs->pc, 0x04); /* Increase B + 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _B)), 0x00);
    EXPECT_EQ(c->regs->flag, H_MASK | Z_MASK);

    /* Check _C */
    set_reg(c->regs, _C, 0x20);
    mem_write8(c->mem, c->regs->pc, 0x0c); /* Increase B + 1 */
    mem_write8(c->mem, c->regs->pc+1, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(*(get_reg(c->regs, _C)), 0x20+1);
    EXPECT_EQ(c->regs->flag, 0x00);
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

    /* ld DE, nnnn => 0x01nnnn */
    mem_write8(c->mem, c->regs->pc, 0x11); /* load BC nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0xef);
    mem_write8(c->mem, c->regs->pc+2, 0xbe);
    mem_write8(c->mem, c->regs->pc+3, 0x13); /* Increase BC + 1 */
    mem_write8(c->mem, c->regs->pc+4, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(get_reg_pair(c->regs, _DE), 0xefbe +1);
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

    /* Load from address @ reg pair register into A reg */
    mem_write8(c->mem, 0xbeef, 0xaf);
    mem_write8(c->mem, c->regs->pc, 0x0A); /* Load *(BC), A */
    mem_write8(c->mem, c->regs->pc+1, 0x00);
    run_cpu_loop(c);
    EXPECT_EQ(c->regs->pc, GAME_ROM_BANK_0_START + 6);
    EXPECT_EQ(get_reg_pair(c->regs, _BC), 0xbeef);
    EXPECT_EQ(mem_read8(c->mem, get_reg_pair(c->regs, _BC)), 0xaf);
    EXPECT_EQ( *(get_reg(c->regs, _A)), 0xaf);

    /* DE */
    mem_write8(c->mem, c->regs->pc, 0x11); /* load BC nnnn */
    mem_write8(c->mem, c->regs->pc+1, 0xef);
    mem_write8(c->mem, c->regs->pc+2, 0xbe);
    mem_write8(c->mem, c->regs->pc+3, 0x00); /* NOOP => quit */
    run_cpu_loop(c);
    EXPECT_EQ(get_reg_pair(c->regs, _DE), 0xefbe);

    /* Load from address @ reg pair register into A reg */
    mem_write8(c->mem, 0xefbe, 0xad);
    mem_write8(c->mem, c->regs->pc, 0x1A); /* Load *(DE), A */
    mem_write8(c->mem, c->regs->pc+1, 0x00);
    run_cpu_loop(c);
    EXPECT_EQ(get_reg_pair(c->regs, _DE), 0xefbe);
    EXPECT_EQ(mem_read8(c->mem, get_reg_pair(c->regs, _DE)), 0xad);
    EXPECT_EQ( *(get_reg(c->regs, _A)), 0xad);
}
}
