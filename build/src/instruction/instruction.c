#include "../../include/instruction.h"
#include "../../include/cpu.h"
#include "../../include/r_utils.h"
#include <float.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Private fn header */
void set_instruction_vars(cpu *core, instruction *i, uint8_t len,
                          uint8_t num_cycles, arguments args);
arguments new_args(enum reg_enum src_reg, enum reg_enum dst_reg,
                   enum reg_pairs src_pair, enum reg_pairs dst_pair);

/**********************************/
/* Checking for the flag register */
/**********************************/

/* Checks for overflow / underflow in bit 3 */
bool check_half_carry16(uint16_t v1, uint16_t v2, bool is_add) {
  /* Add the lower nibbles, and check to see if they turn on 4th bit,
   * if so => overflow bit 7 */
  if (is_add)
    return ((v1 & 0xfff) + (v2 & 0xfff) & 0x1000) == 0x1000;
  return ((v1 & 0xfff) - (v2 & 0xfff) & 0x1000) == 0x1000;
}

/* Checks for overflow / underflow in bit 3 */
bool check_half_carry8(uint8_t v1, uint8_t v2, bool is_add) {
  /* Add the lower nibbles, and check to see if they turn on 4th bit,
   * if so => overflow bit 3 */
  if (is_add)
    return ((v1 & 0xf) + (v2 & 0xf) & 0x10) == 0x10;
  return ((v1 & 0xf) - (v2 & 0xf) & 0x10) == 0x10;
}

bool check_zero(uint16_t v) { return v == 0; }

bool check_carry8_shift(uint8_t v1, bool left_shift) {
  if (left_shift)
    return 0x80 & v1;
  return 0x01 & v1;
}

/************************************/
/* Helper functionality for opcodes */
/************************************/

void check_daa(cpu *core, uint8_t rv) {
  /* https://forums.nesdev.org/viewtopic.php?t=15944
   * From what I understand, we are trying to change the output of
   * an arithmetic instruction to a decimal coded version.
   * If the carry flags (HC || C), the number may be an invalid
   * binary coded num, or it may just be the incorrect number, hence
   * the updates to RV that take place throughout the fn */
  if (mem_read8(core->mem, core->regs->pc) == 0x27) {
    core->regs->pc++;
    /* Binary decode the output */
    bool n_flag = get_flag(core->regs, N_MASK);
    bool c_flag = get_flag(core->regs, CY_MASK);
    bool h_flag = get_flag(core->regs, H_MASK);

    // note: assumes a is a uint8_t and wraps from 0xff to 0
    if (!n_flag) { // after an addition, adjust if (half-)carry occurred or if
                   // result is out of bounds
      if (c_flag || rv > 0x99) {
        rv += 0x60;
        set_flag(core->regs, CY_MASK, true);
      }
      if (h_flag || (rv & 0x0f) > 0x09)
        rv += 0x6;

    } else { // after rv subtraction, only adjust if (half-)carry occurred
      if (c_flag)
        rv -= 0x60;

      if (h_flag)
        rv -= 0x6;
    }
    set_all_flags(core->regs, check_zero(rv), n_flag, false, 3);
    set_reg(core->regs, _A, rv);
  }
}

/* Jumps to a relative address, between -128 & 127 */
void jump_relative(cpu *core, instruction i, int8_t offset) {
  core->regs->pc += offset;
}

void cc_jump_relative(cpu *core, instruction i, uint8_t mask, bool set) {
  arguments args = new_args(_, _, __, __);
  /* if ~SET and mask is not on or SET and mask is on, jump*/
  if ((!set & !get_flag(core->regs, mask) ||
       set & get_flag(core->regs, mask))) {
    set_instruction_vars(core, &i, 2, 12, args);
    jump_relative(core, i, (int8_t)i.full_opcode[1]);
  } else
    set_instruction_vars(core, &i, 2, 8, args);
}

/* Puts the cpu into low power mode
 * TODO: stop the system clock? */
void stop_cpu(cpu *core, instruction i) { core->state = _STOP; }

void halt(cpu *core, instruction i) {
  core->state = _HALTED;
  return;
}

/* Pop from the stack and store it into an address */
void stack_pop_i(cpu *core, instruction i) {
  enum reg_pairs pair = i.args.src_pair;
  /* (1) Popping from stack into a pair reg */
  if (pair != __)
    /* Not implemented */
    return;

  /* (2) Popping from stack into an address */

  /* Get the corresponding address & value */
  uint16_t address = conv8_to16(i.full_opcode[1], i.full_opcode[2]);

  uint16_t value = stack_peak(core->stack);

  /* Store in big endian => lower byte first */
  uint8_t hi = conv16_to8(value, false);
  uint8_t lo = conv16_to8(value, true);

  uint16_t new_value = conv8_to16(hi, lo);
  mem_write16(core->mem, address, new_value);
}

/* Shifting register left or right by 1 */
void shift_reg(cpu *core, instruction i, bool left_shift) {
  enum reg_enum reg = i.args.src_reg;
  uint8_t *r = get_reg(core->regs, reg);

  /* New value of the register */
  uint8_t new_r = left_shift ? (*r) << 1 : (*r) >> 1;

  /* We don't set the Z flag if we are doing one of the
   * RLCA, RRCA, RLA, RRA */
  if (i.opcode == 0x07 || i.opcode == 0x17 || i.opcode == 0x0f ||
      i.opcode == 0x1f)
    set_all_flags(core->regs, false, false, false,
                  check_carry8_shift(*r, left_shift));
  else
    set_all_flags(core->regs, check_zero(new_r), false, false,
                  check_carry8_shift(*r, left_shift));

  set_reg(core->regs, reg, new_r);
}

/* Subsitute functionality */
void sub_reg(cpu *core, instruction i) {

  enum reg_enum src_reg = i.args.src_reg;
  enum reg_enum dst_reg = i.args.dst_reg;
  enum reg_pairs src_pair = i.args.src_pair;
  enum reg_pairs dst_pair = i.args.dst_pair;

  /* (1) 16-bit sub instructions */
  if (src_pair == _SP && src_reg == _1) {
    set_stack_pointer(core->stack, stack_peak(core->stack) - 1);
    return;
  }
  uint16_t r_src_pair = __;
  if (src_pair != __) {
    r_src_pair = get_reg_pair(core->regs, src_pair);
    if (dst_pair == _ADDY && src_reg == _1) {
      /* Read the address */
      uint8_t val = mem_read8(core->mem, r_src_pair);
      RV8 sub_result = sub_overflow8(val, 1);
      set_all_flags(core->regs, check_zero(sub_result.rv), true,
                    check_half_carry8(val, 1, true), 2);
      mem_write8(core->mem, r_src_pair, sub_result.rv);
    }
    /* Inc src_pair by 1 */
    else if (src_reg == _1) {
      RV16 sub_result = sub_overflow16(r_src_pair, 1);
      set_reg_pair(core->regs, src_pair, sub_result.rv);
    }
  }
  /* (2) 8 Bit sub instructions */
  if (dst_reg == _1) {
    uint8_t *r1 = get_reg(core->regs, src_reg);
    RV8 sub_result = sub_overflow8(*r1, 1);
    set_all_flags(core->regs, check_zero(sub_result.rv), true,
                  check_half_carry8(*r1, 1, false), 2);
    set_reg(core->regs, src_reg, sub_result.rv);
    check_daa(core, sub_result.rv);
  }
}

void add_reg(cpu *core, instruction i) {
  enum reg_enum src_reg = i.args.src_reg;
  enum reg_enum dst_reg = i.args.dst_reg;
  enum reg_pairs src_pair = i.args.src_pair;
  enum reg_pairs dst_pair = i.args.dst_pair;

  core->valid_daa = true;
  /* (1) 16-bit add instructions */
  uint16_t r_src_pair = __;
  if (src_pair != __) {
    /* If we are dealing with the stack regiester */
    if (src_pair == _SP) {
      // To make this cleaner, should makse core->sp => *(core->sp);
      if (src_reg == _1)
        set_stack_pointer(core->stack, stack_peak(core->stack) + 1);
      else if (dst_pair != __) {
        uint16_t r_dst_pair = get_reg_pair(core->regs, dst_pair);
        RV16 add_result = add_overflow16(stack_peak(core->stack), r_dst_pair);
        set_reg_pair(core->regs, dst_pair, add_result.rv);
        set_all_flags(core->regs, 2, false,
                      check_half_carry16(r_src_pair, r_dst_pair, true),
                      add_result.over_flow);
      }

      return;
    }

    r_src_pair = get_reg_pair(core->regs, src_pair);

    /* (1.1) Inc value stored in address @ src_pair by 1 */
    if (dst_pair == _ADDY && src_reg == _1) {
      /* Read the address */
      uint8_t val = mem_read8(core->mem, r_src_pair);
      RV8 add_result = add_overflow8(val, 1);
      set_all_flags(core->regs, check_zero(add_result.rv), false,
                    check_half_carry8(val, 1, true), 2);
      mem_write8(core->mem, r_src_pair, add_result.rv);
    }

    /* (1.2) Inc src_pair by 1 */
    else if (src_reg == _1) {
      RV16 add_result = add_overflow16(r_src_pair, 1);
      set_reg_pair(core->regs, src_pair, add_result.rv);
    } else {
      /* (1.3) Adding 2 reg pairs */
      uint16_t r_dst_pair = get_reg_pair(core->regs, dst_pair);
      RV16 add_result = add_overflow16(r_src_pair, r_dst_pair);
      set_reg_pair(core->regs, src_pair, add_result.rv);
      set_all_flags(core->regs, 2, false,
                    check_half_carry16(r_src_pair, r_dst_pair, true),
                    add_result.over_flow);
    }
  }

  /* (2) 8 Bit add instructions */
  if (dst_reg == _1) {
    uint8_t *r1 = get_reg(core->regs, src_reg);
    RV8 add_result = add_overflow8(*r1, 1);
    set_all_flags(core->regs, check_zero(add_result.rv), false,
                  check_half_carry8(*r1, 1, true), 2);
    set_reg(core->regs, src_reg, add_result.rv);
    check_daa(core, add_result.rv);
  }
}

/* Loads in a value into the register pair based on i */
void load_reg(cpu *core, instruction i) {

  enum reg_enum src_reg = i.args.src_reg;
  enum reg_enum dst_reg = i.args.dst_reg;
  enum reg_pairs src_pair = i.args.src_pair;
  enum reg_pairs dst_pair = i.args.dst_pair;

  /* (1) Doing a load with a src_pair */
  if (src_pair != __) {
    if (src_pair == _SP) {
      /* Loading nn into stack pointer */
      uint16_t nn = conv8_to16(i.full_opcode[1], i.full_opcode[2]);
      if (!stack_is_empty(core->stack)) {
        set_stack_pointer(core->stack, nn);
        return;
      }
      /* Allow for push if stack is not allocated */
      if (!stack_push(core->stack, nn)) {
        printf("Stack overflow!\n");
        exit(-1);
      }
    }

    if (src_reg == _ && dst_reg == _) {
      if (dst_pair == _ADDY) {
        /* Storing nn in *(dst_pair) */
        uint16_t address = get_reg_pair(core->regs, src_pair);
        mem_write8(core->mem, address, i.full_opcode[1]);
        return;
      }
      /* (1.1) Loading nn into src_pair */
      uint16_t nn = conv8_to16(i.full_opcode[1], i.full_opcode[2]);
      set_reg_pair(core->regs, src_pair, nn);
    } else if (src_reg != _ && dst_reg == _) {
      /* (1.2) Loading a value from address into a register */
      uint16_t address = get_reg_pair(core->regs, src_pair);
      set_reg(core->regs, src_reg, mem_read8(core->mem, address));
    } else {
      /* (1.3) Loading a value a register into an address */
      uint16_t address = get_reg_pair(core->regs, src_pair);
      mem_write8(core->mem, address, *(get_reg(core->regs, dst_reg)));
    }
    return;
  }
  /* Load from src_reg => dst_reg */
  if (src_reg != _ && dst_reg != _) {
    set_reg(core->regs, dst_reg, *(get_reg(core->regs, src_reg)));
  }
  /* (2) Doing a load into variable register */
  else if (src_reg == _) {
    /* Loading immediate */
    set_reg(core->regs, dst_reg, i.full_opcode[1]);
  }
}

/* Noop instruction does nothing */
void noop(cpu *core, instruction i) { return; }

/************************************/
/* Instruction set up and interface */
/************************************/

arguments new_args(enum reg_enum src_reg, enum reg_enum dst_reg,
                   enum reg_pairs src_pair, enum reg_pairs dst_pair) {
  arguments out;
  out.src_reg = src_reg;
  out.dst_reg = dst_reg;
  out.src_pair = src_pair;
  out.dst_pair = dst_pair;
  return out;
}
/* Read the necessary bytes from memory for this instruction */
void set_instruction_vars(cpu *core, instruction *i, uint8_t len,
                          uint8_t num_cycles, arguments args) {
  i->len = len;
  i->num_cycles = num_cycles;
  i->full_opcode[0] = i->opcode;
  for (int j = 1; j < i->len; j++) {
    i->full_opcode[j] = mem_read8(core->mem, (core->regs->pc + j));
  }
  core->regs->pc += len;
  i->args = args;
}

/* Given an opcode, decodes the instruction and executes it on the cpu */
instruction exec_next_instruction(cpu *core, uint8_t opcode) {
  instruction out;
  out.opcode = opcode;
  arguments args;

  switch (opcode) {
    /***************/
    /* 0x00 - 0x0f */
    /***************/
  case 0x00: /* noop */
    args = new_args(_, _, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    /* noop(core, out); */
    break;
  case 0x01: /* LD BC, d16 => 0x01nnnn */
    args = new_args(_, _, _BC, __);
    set_instruction_vars(core, &out, 3, 12, args);
    load_reg(core, out);
    break;
  case 0x02: /* LD A, (BC) */
    args = new_args(_, _A, _BC, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x03: /* INC BC */
    args = new_args(_1, _, _BC, __);
    set_instruction_vars(core, &out, 1, 8, args);
    add_reg(core, out);
    break;
  case 0x04: /* INC B */
    args = new_args(_B, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    add_reg(core, out);
    break;
  case 0x05: /* DEC B */
    args = new_args(_B, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    sub_reg(core, out);
    break;
  case 0x06: /* LD B, d8 => 0x06nn */
    args = new_args(_, _B, __, __);
    set_instruction_vars(core, &out, 2, 8, args);
    load_reg(core, out);
    break;
  case 0x07: /* RLCA */
    args = new_args(_A, _, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    shift_reg(core, out, true);
    break;
  case 0x08: /* LD (a16), SP */
    args = new_args(_, _, __, __);
    set_instruction_vars(core, &out, 3, 4, args);
    stack_pop_i(core, out);
    break;
  case 0x09: /* ADD HL, BC */
    args = new_args(_, _, _HL, _BC);
    set_instruction_vars(core, &out, 1, 4, args);
    add_reg(core, out);
    break;
  case 0x0a: /* LD BC, A => 0x02 */
    args = new_args(_A, _, _BC, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x0b: /* DEC BC */
    args = new_args(_1, _, _BC, __);
    set_instruction_vars(core, &out, 1, 8, args);
    sub_reg(core, out);
    break;
  case 0x0c: /* INC C */
    args = new_args(_C, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    add_reg(core, out);
    break;
  case 0x0d: /* DEC C */
    args = new_args(_C, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    sub_reg(core, out);
    break;
  case 0x0e: /* LD C, d8 => 0x06nn */
    args = new_args(_, _C, __, __);
    set_instruction_vars(core, &out, 2, 8, args);
    load_reg(core, out);
    break;
  case 0x0f: /* RRCA */
    args = new_args(_A, _, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    shift_reg(core, out, false);
    break;

    /***************/
    /* 0x10 - 0x1f */
    /***************/
  case 0x10: /* STOP */
    args = new_args(_, _, __, __);
    set_instruction_vars(core, &out, 2, 4, args);
    stop_cpu(core, out);
    break;
  case 0x11: /* LD DE, 16 => 0x11nnnn */
    args = new_args(_, _, _DE, __);
    set_instruction_vars(core, &out, 3, 12, args);
    load_reg(core, out);
    break;
  case 0x12: /* LD A, (DE) */
    args = new_args(_, _A, _DE, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x13: /* INC DE */
    args = new_args(_1, _, _DE, __);
    set_instruction_vars(core, &out, 1, 8, args);
    add_reg(core, out);
    break;
  case 0x14: /* INC D */
    args = new_args(_D, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    add_reg(core, out);
    break;
  case 0x15: /* DEC D */
    args = new_args(_D, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    sub_reg(core, out);
    break;
  case 0x16: /* LD D, d8 => 0x06nn */
    args = new_args(_, _D, __, __);
    set_instruction_vars(core, &out, 2, 8, args);
    load_reg(core, out);
    break;
  case 0x17: /* RLA */
    args = new_args(_A, _, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    shift_reg(core, out, true);
    break;
  case 0x18: /* JR nn: Jump relative on signed offset */
    args = new_args(_, _, __, __);
    set_instruction_vars(core, &out, 2, 12, args);
    jump_relative(core, out, (int8_t)out.full_opcode[1]);
    break;
  case 0x19: /* ADD HL, BC */
    args = new_args(_, _, _HL, _DE);
    set_instruction_vars(core, &out, 1, 4, args);
    add_reg(core, out);
    break;
  case 0x1a: /* LD A, DE => 0x1a */
    args = new_args(_A, _, _DE, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x1b: /* DEC BC */
    args = new_args(_1, _, _DE, __);
    set_instruction_vars(core, &out, 1, 8, args);
    sub_reg(core, out);
    break;
  case 0x1c: /* INC E */
    args = new_args(_E, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    add_reg(core, out);
    break;
  case 0x1d: /* DEC E */
    args = new_args(_E, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    sub_reg(core, out);
    break;
  case 0x1e: /* LD E, d8 => 0x06nn */
    args = new_args(_, _E, __, __);
    set_instruction_vars(core, &out, 2, 8, args);
    load_reg(core, out);
    break;
  case 0x1f: /* RRA */
    args = new_args(_A, _, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    shift_reg(core, out, false);
    break;

  /***************/
  /* 0x20 - 0x2f */
  /***************/
  case 0x20: /* JR NZ nn */
    cc_jump_relative(core, out, Z_MASK, false);
    break;
  case 0x21: /* LD HL, 16 => 0x21nnnn */
    args = new_args(_, _, _HL, __);
    set_instruction_vars(core, &out, 3, 12, args);
    load_reg(core, out);
    break;
  case 0x22: /* LD HL, A && HL++ => 0x22 */
    args = new_args(_, _A, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    out.args = new_args(_1, _, _HL, __);
    add_reg(core, out);
    break;
  case 0x23: /* INC HL */
    args = new_args(_1, _, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    add_reg(core, out);
    break;
  case 0x24: /* INC H */
    args = new_args(_H, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    add_reg(core, out);
    break;
  case 0x25: /* DEC H */
    args = new_args(_H, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    sub_reg(core, out);
  case 0x26: /* LD H, d8 => 0x06nn */
    args = new_args(_, _H, __, __);
    set_instruction_vars(core, &out, 2, 8, args);
    load_reg(core, out);
    break;
  case 0x27: /* DAA */
    args = new_args(_, _H, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x28: /* JR Z nn */
    cc_jump_relative(core, out, Z_MASK, true);
    break;
  case 0x29: /* ADD HL, HL */
    args = new_args(_, _, _HL, _HL);
    set_instruction_vars(core, &out, 1, 4, args);
    add_reg(core, out);
    break;
  case 0x2a: /* LD A, HL++ => 0x2a */
    args = new_args(_A, _, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    out.args = new_args(_1, _, _HL, __);
    add_reg(core, out);
    break;
  case 0x2b: /* DEC HL */
    args = new_args(_1, _, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    sub_reg(core, out);
    break;
  case 0x2c: /* INC L */
    args = new_args(_L, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    add_reg(core, out);
    break;
  case 0x2d: /* DEC L */
    args = new_args(_L, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    sub_reg(core, out);
    break;
  case 0x2e: /* LD L, d8 => 0x06nn */
    args = new_args(_, _L, __, __);
    set_instruction_vars(core, &out, 2, 8, args);
    load_reg(core, out);
    break;
  case 0x2f: /* CPL */
    args = new_args(_, _, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    /* Get 1s complement of A register */
    uint8_t a_comp = ~(*(get_reg(core->regs, _A)));
    set_reg(core->regs, _A, a_comp);
    set_all_flags(core->regs, 2, true, true, 2);
    break;

    /***************/
    /* 0x30 - 0x3f */
    /***************/
  case 0x30: /* JR NC nn */
    cc_jump_relative(core, out, CY_MASK, false);
    break;
  case 0x31: /* LD SP nn */
    args = new_args(_, _, _SP, __);
    set_instruction_vars(core, &out, 3, 12, args);
    load_reg(core, out);
    break;
  case 0x32: /* LD HL, A && HL-- => 0x22 */
    args = new_args(_, _A, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    out.args = new_args(_1, _, _HL, __);
    sub_reg(core, out);
    break;
  case 0x33: /* INC SP */
    args = new_args(_1, _, _SP, __);
    set_instruction_vars(core, &out, 1, 8, args);
    add_reg(core, out);
    break;
  case 0x34: /* INC *(HL) */
    args = new_args(_1, _, _HL, _ADDY);
    set_instruction_vars(core, &out, 1, 8, args);
    add_reg(core, out);
    break;
  case 0x35: /* INC *(HL) */
    args = new_args(_1, _, _HL, _ADDY);
    set_instruction_vars(core, &out, 1, 8, args);
    sub_reg(core, out);
    break;
  case 0x36: /* LD *(HL), d8 => 0x36nn */
    args = new_args(_, _, _HL, _ADDY);
    set_instruction_vars(core, &out, 2, 8, args);
    load_reg(core, out);
    break;
  case 0x37: /* SCF */
    args = new_args(_, _, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    set_all_flags(core->regs, 3, false, false, CY_MASK);
    break;
  case 0x38: /* JR C nn */
    cc_jump_relative(core, out, CY_MASK, true);
    break;
  case 0x39: /* INC HL += SP */
    args = new_args(_, _, _SP, _HL);
    set_instruction_vars(core, &out, 1, 8, args);
    add_reg(core, out);
    break;
  case 0x3a: /* LD A, HL-- => 0x2a */
    args = new_args(_A, _, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    out.args = new_args(_1, _, _HL, __);
    sub_reg(core, out);
    break;
  case 0x3b: /* DEC SP */
    args = new_args(_1, _, _SP, __);
    set_instruction_vars(core, &out, 1, 8, args);
    sub_reg(core, out);
    break;
  case 0x3c: /* INC A */
    args = new_args(_A, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    add_reg(core, out);
    break;
  case 0x3d: /* DEC A */
    args = new_args(_A, _1, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    sub_reg(core, out);
    break;
  case 0x3e: /* LD A, d8 => 0x06nn */
    args = new_args(_, _A, __, __);
    set_instruction_vars(core, &out, 2, 8, args);
    load_reg(core, out);
    break;
  case 0x3f: /* CCF */
    args = new_args(_, _, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    /* Flip the carry flag */
    bool ccf = !(get_flag(core->regs, CY_MASK));
    set_all_flags(core->regs, 2, false, false, ccf);
    break;

    /***************/
    /* 0x40 - 0x4f */
    /***************/
  case 0x40: /* LD B, B */
    args = new_args(_B, _B, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x41: /* LD B, C */
    args = new_args(_C, _B, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x42: /* LD B, D */
    args = new_args(_D, _B, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x43: /* LD B, E */
    args = new_args(_E, _B, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x44: /* LD B, H */
    args = new_args(_H, _B, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x45: /* LD B, L */
    args = new_args(_L, _B, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x46: /* LD B, HL */
    args = new_args(_B, _, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x47: /* LD B, A */
    args = new_args(_A, _B, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x48: /* LD C, B */
    args = new_args(_B, _C, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x49: /* LD C, C */
    args = new_args(_C, _C, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x4a: /* LD C, D */
    args = new_args(_D, _C, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x4b: /* LD C, E */
    args = new_args(_E, _C, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x4c: /* LD C, H */
    args = new_args(_H, _C, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x4d: /* LD C, L */
    args = new_args(_L, _C, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x4e: /* LD C, HL */
    args = new_args(_C, _, _HL, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x4f: /* LD C, A */
    args = new_args(_A, _C, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;

    /***************/
    /* 0x50 - 0x5f */
    /***************/
  case 0x50: /* LD B, B */
    args = new_args(_B, _D, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x51: /* LD D, C */
    args = new_args(_C, _D, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x52: /* LD D, D */
    args = new_args(_D, _D, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x53: /* LD D, E */
    args = new_args(_E, _D, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x54: /* LD D, H */
    args = new_args(_H, _D, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x55: /* LD D, L */
    args = new_args(_L, _D, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x56: /* LD D, HL */
    args = new_args(_D, _, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x57: /* LD D, A */
    args = new_args(_A, _D, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x58: /* LD E, B */
    args = new_args(_B, _E, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x59: /* LD E, E */
    args = new_args(_E, _E, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x5a: /* LD E, D */
    args = new_args(_D, _E, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x5b: /* LD E, E */
    args = new_args(_C, _E, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x5c: /* LD E, H */
    args = new_args(_H, _E, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x5d: /* LD E, L */
    args = new_args(_L, _E, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x5e: /* LD E, HL */
    args = new_args(_E, _, _HL, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x5f: /* LD E, A */
    args = new_args(_A, _E, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;

    /***************/
    /* 0x60 - 0x6f */
    /***************/
  case 0x60: /* LD H, B */
    args = new_args(_B, _H, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x61: /* LD H, C */
    args = new_args(_C, _H, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x62: /* LD H, D */
    args = new_args(_D, _H, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x63: /* LD H, E */
    args = new_args(_E, _H, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x64: /* LD H, H */
    args = new_args(_H, _H, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x65: /* LD H, L */
    args = new_args(_L, _H, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x66: /* LD H, HL */
    args = new_args(_H, _, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x67: /* LD H, A */
    args = new_args(_A, _H, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x68: /* LD L, B */
    args = new_args(_B, _L, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x69: /* LD L, C */
    args = new_args(_C, _L, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x6a: /* LD L, D */
    args = new_args(_D, _L, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x6b: /* LD L, E */
    args = new_args(_E, _L, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x6c: /* LD L, H */
    args = new_args(_H, _L, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x6d: /* LD L, L */
    args = new_args(_L, _L, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x6e: /* LD L, HL */
    args = new_args(_L, _, _HL, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
  case 0x6f: /* LD L, A */
    args = new_args(_A, _L, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    load_reg(core, out);
    break;
    /***************/
    /* 0x70 - 0x7f */
    /***************/
  case 0x70: /* LD HL, B && HL-- => 0x22 */
    args = new_args(_, _B, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x71: /* LD HL, C && HL-- => 0x22 */
    args = new_args(_, _C, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x72: /* LD HL, D && HL-- => 0x22 */
    args = new_args(_, _D, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x73: /* LD HL, E && HL-- => 0x22 */
    args = new_args(_, _E, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x74: /* LD HL, H && HL-- => 0x22 */
    args = new_args(_, _H, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x75: /* LD HL, L && HL-- => 0x22 */
    args = new_args(_, _L, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  case 0x76: /* HALT */
    args = new_args(_, _, __, __);
    set_instruction_vars(core, &out, 1, 4, args);
    halt(core, out);
    break;
  case 0x77: /* LD HL, A && HL-- => 0x22 */
    args = new_args(_, _A, _HL, __);
    set_instruction_vars(core, &out, 1, 8, args);
    load_reg(core, out);
    break;
  default:
    printf("Invalid opcode: %x\n", opcode);
    exit(-1);
  }

  return out;
}
