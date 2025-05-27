/*
 *  Copyright (C) 2025 Nicolai Brand (lytix.dev)
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include "types.h"


/* 
 * Notes on the instruction formats:
 * - The first 7 bits is always the opcode
 * - Four main variants (R, I, S, U)
 * R-type:
 *   31      25 24   20 19   15 14   12 11   7 6     0
 *  [ funct7 ][ rs2 ][ rs1 ][funct3][  rd  ][opcode]
 * I-type:
 *   31              20 19   15 14   12 11   7 6     0
 *  [      imm       ][ rs1 ][funct3][  rd ][opcode]
 * S-type:
 *   31      25 24   20 19   15 14   12 11   7 6     0
 *  [ imm[11:5] ][ rs2 ][ rs1 ][funct3][ imm][opcode]
 * U-type:
 *   31                              12 11   7 6     0
 *  [      imm                       ][ rd  ][opcode]
 *
 *  Further variants:
 *  - SB, UJ. Changes how immediates are encoded.
 */

#define XLEN 64

typedef enum {
    REG_ZERO = 0, // x0, always zero
    REG_RA, // x1, etc.
    REG_SP,
    REG_GP,
    REG_TP,
    REG_T0,
    REG_T1,
    REG_T2,
    REG_S0,
    REG_S1,
    REG_A0,
    REG_A1,
    REG_A2,
    REG_A3,
    REG_A4,
    REG_A5,
    REG_A6,
    REG_A7,
    REG_S2,
    REG_S4,
    REG_S5,
    REG_S6,
    REG_S7,
    REG_S8,
    REG_S9,
    REG_S10,
    REG_S11,
    REG_T3,
    REG_T4,
    REG_T5,
    REG_T6,
} RegNames;


typedef enum {
    OPCODE_OP        = 0b0110011, // R-type
    OPCODE_OP_32     = 0b0111011, // R-type, 32-bit

    OPCODE_OP_IMM    = 0b0010011, // I-type: Immediate Arithmetic
    OPCODE_OP_IMM_32 = 0b0011011, // I-type: 32-bit Immediate Arithmetic 
    OPCODE_JALR      = 0b1100111, // I-type, Jump and Link Register
    OPCODE_LOAD      = 0b0000011, // I-type, Loads
                                  
    OPCODE_LUI       = 0b0110111, // U-type: Load Upper Immediate
    OPCODE_AUIPC     = 0b0010111, // U-type: Add Upper Immediate to PC
                                  
    OPCODE_JAL       = 0b1101111, // J-type: Jump and Link
                                  
    OPCODE_BRANCH    = 0b1100011, // B-type: Conditional Branches (beq, bne, etc.)
                                  
    OPCODE_STORE     = 0b0100011, // S-type: Stores (sb, sh, sw, sd, etc.)
    //OPCODE_MISC_MEM  = 0b0001111, // I-type: FENCE and related memory operations
    //OPCODE_SYSTEM    = 0b1110011  // I-type: CSR access, ecall, ebreak
} Opcode;

typedef enum {
    /* OPCODE_BRANCH */
    FUNCT3_BEQ  = 0b000,
    FUNCT3_BNE  = 0b001,
    FUNCT3_BLT  = 0b100,
    FUNCT3_BGE  = 0b101,
    FUNCT3_BLTU = 0b110,
    FUNCT3_BGEU = 0b111,

    /* OPCODE_LOAD and OPCODE_STORE */
    FUNCT3_LB   = 0b000,
    FUNCT3_LH   = 0b001,
    FUNCT3_LW   = 0b010,
    FUNCT3_LD   = 0b011,
    FUNCT3_LBU  = 0b100,
    FUNCT3_LHU  = 0b101,
    FUNCT3_LWU  = 0b110,

    FUNCT3_SB   = 0b000,
    FUNCT3_SH   = 0b001,
    FUNCT3_SW   = 0b010,
    FUNCT3_SD   = 0b011,

    /* OPCODE_OP and OPCODE_OP_IMM */
    FUNCT3_ADD_SUB  = 0b000,
    FUNCT3_SLL      = 0b001,
    FUNCT3_SLT      = 0b010,
    FUNCT3_SLTU     = 0b011,
    FUNCT3_XOR      = 0b100,
    FUNCT3_SRL_SRA  = 0b101,
    FUNCT3_OR       = 0b110,
    FUNCT3_AND      = 0b111,

    /* JALR */
    FUNCT3_JALR     = 0b000,

    // /* SYSTEM */
    // FUNCT3_PRIV     = 0b000,
    // FUNCT3_CSRRW    = 0b001,
    // FUNCT3_CSRRS    = 0b010,
    // FUNCT3_CSRRC    = 0b011,
    // FUNCT3_CSRRWI   = 0b101,
    // FUNCT3_CSRRSI   = 0b110,
    // FUNCT3_CSRRCI   = 0b111,

    // /* FENCE */
    // FUNCT3_FENCE    = 0b000,
    // FUNCT3_FENCE_I  = 0b001
} Funct3;

typedef enum {
    /* OPCODE_OP and OPCODE_OP_32 */
    FUNCT7_ADD      = 0b0000000,
    FUNCT7_SUB      = 0b0100000,
    FUNCT7_SLL      = 0b0000000,
    FUNCT7_SLT      = 0b0000000,
    FUNCT7_SLTU     = 0b0000000,
    FUNCT7_XOR      = 0b0000000,
    FUNCT7_SRL      = 0b0000000,
    FUNCT7_SRA      = 0b0100000,
    FUNCT7_OR       = 0b0000000,
    FUNCT7_AND      = 0b0000000,

    // NOTE: Word in RV64 is 32 bits!
    FUNCT7_ADDW     = 0b0000000,
    FUNCT7_SUBW     = 0b0100000,
    FUNCT7_SLLW     = 0b0000000,
    FUNCT7_SRLW     = 0b0000000,
    FUNCT7_SRAW     = 0b0100000,

    /* TODO: SYSTEM instructions. May just implement it as a single instruction. */
    //FUNCT7_SYSTEM   = 0b0000000
} Funct7;

#define MEM_SIZE (1 << 20) // 1 MiB

typedef struct {
    s64 regs[32];
    u64 pc;
    u64 steps;
    u8 memory[MEM_SIZE];
} RiscVM;


/* Converts a signed number from a smaller bit-width to a larger one without changing its value */
static s32 sign_extend(s32 imm, s32 bit_width) 
{
    int shift = 32 - bit_width;
    return (imm << shift) >> shift;
}

// TODO: Ensure correct alignment, also, bounds check
static u8 mem_load8(RiscVM *vm, u64 addr) 
{
    return vm->memory[addr];
}

static u16 mem_load16(RiscVM *vm, u64 addr)
{
    return *(u16 *)&vm->memory[addr];
}

static u32 mem_load32(RiscVM *vm, u64 addr) 
{
    return *(u32 *)&vm->memory[addr];
}

static u64 mem_load64(RiscVM *vm, u64 addr) 
{
    return *(u64 *)&vm->memory[addr];
}

static void mem_store8(RiscVM *vm, u64 addr, u8 val) 
{
    vm->memory[addr] = val;
}

static void mem_store16(RiscVM *vm, u64 addr, u16 val)
{
    *(u16 *)&vm->memory[addr] = val;
}

static void mem_store32(RiscVM *vm, u64 addr, u32 val)
{
    *(u32 *)&vm->memory[addr] = val;
}

static void mem_store64(RiscVM *vm, u64 addr, u64 val) 
{
    *(u64 *)&vm->memory[addr] = val;
}

static u32 encode_rtype(u8 rd, u8 rs1, u8 rs2, u8 opcode, u8 funct7, u8 funct3)
{
    return ((funct7 & 0x7F) << 25) |
           ((rs2 & 0x1F) << 20) |
           ((rs1 & 0x1F) << 15) |
           ((funct3 & 0x07) << 12) |
           ((rd & 0x1F) << 7) |
           (opcode & 0x7F);
}

static u32 encode_itype_op(u8 rd, u8 rs1, s32 imm, u8 opcode, u8 funct3)
{
    return ((imm & 0xFFF) << 20) |
           ((rs1 & 0x1F) << 15) |
           ((funct3 & 0x07) << 12) |
           ((rd & 0x1F) << 7) |
           (opcode & 0x7F);
}

static u32 encode_itype_load(u8 rd, u8 rs1, s32 imm, u8 funct3)
{
    return ((imm & 0xFFF) << 20) |
           ((rs1 & 0x1F) << 15) |
           ((funct3 & 0x7) << 12) |
           ((rd & 0x1F) << 7) |
           OPCODE_LOAD;
}

static u32 encode_stype(u8 rs1, u8 rs2, s32 imm, u8 opcode, u8 funct3)
{
    u32 imm11_5 = (imm >> 5) & 0x7F;
    u32 imm4_0  = imm & 0x1F;
    return (imm11_5 << 25) |
           ((rs2 & 0x1F) << 20) |
           ((rs1 & 0x1F) << 15) |
           ((funct3 & 0x7) << 12) |
           ((imm4_0 & 0x1F) << 7) |
           (opcode & 0x7F);
}


void execute_instruction(RiscVM *vm, u32 inst)
{
    vm->steps += 1;
    u8 opcode = inst & 0x7F;

    switch (opcode) {
    default:
        printf("Unknown opcode: 0x%02X\n", opcode);
        break;
    /* 
     * R-type:
     *   31      25 24   20 19   15 14   12 11   7 6     0
     *  [ funct7 ][ rs2 ][ rs1 ][funct3][  rd  ][opcode]
     *
     * Handles:
     * ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND.
     */
    case OPCODE_OP: {
        vm->pc += 4;
        u8 rd = (inst >> 7) & 0x1F;
        u8 funct3 = (inst >> 12) & 0x07;
        u8 rs1 = (inst >> 15) & 0x1F;
        u8 rs2 = (inst >> 20) & 0x1F;
        u8 funct7 = (inst >> 25) & 0x7F;

        switch (funct3) {
        case FUNCT3_ADD_SUB:
            if (funct7 == FUNCT7_ADD) {
                vm->regs[rd] = vm->regs[rs1] + vm->regs[rs2];
            } else if (funct7 == FUNCT7_SUB) {
                vm->regs[rd] = vm->regs[rs1] - vm->regs[rs2];
            }
            break;
        case FUNCT3_SLL:
            // TODO: redundant to check for funct7? Ommited for the rest of the cases
            if (funct7 == FUNCT7_SLL) {
                vm->regs[rd] = vm->regs[rs1] << (vm->regs[rs2] & 0x3F);
            }
            break;
        case FUNCT3_SLT:
            vm->regs[rd] = (s64)vm->regs[rs1] < (s64)vm->regs[rs2];
            break;
        case FUNCT3_SLTU:
            vm->regs[rd] = vm->regs[rs1] < vm->regs[rs2];
            break;
        case FUNCT3_XOR:
            vm->regs[rd] = vm->regs[rs1] ^ vm->regs[rs2];
            break;
        case FUNCT3_SRL_SRA:
            if (funct7 == FUNCT7_SRL) {
                vm->regs[rd] = vm->regs[rs1] >> (vm->regs[rs2] & 0x3F);
            } else if (funct7 == FUNCT7_SRA) {
                vm->regs[rd] = (s64)vm->regs[rs1] >> (vm->regs[rs2] & 0x3F);
            }
            break;
        case FUNCT3_OR:
            vm->regs[rd] = vm->regs[rs1] | vm->regs[rs2];
            break;
        case FUNCT3_AND:
            vm->regs[rd] = vm->regs[rs1] & vm->regs[rs2];
            break;
        default:
            // TODO: invalid instruction
            break;
        }
    }; break;
    /*
     * I-type:
     *   31              20 19   15 14   12 11   7 6     0
     *  [      imm       ][ rs1 ][funct3][  rd ][opcode]
     *
     * Handles:
     * ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI.
     */
    case OPCODE_OP_IMM: {
        vm->pc += 4;
        u8 rd = (inst >> 7) & 0x1F;
        u8 funct3 = (inst >> 12) & 0x07;
        u8 rs1 = (inst >> 15) & 0x1F;
        s32 imm = ((s32)inst) >> 20; // TODO: Use sign extend function

        switch (funct3) {
        case FUNCT3_ADD_SUB:
            vm->regs[rd] = vm->regs[rs1] + imm;
            break;
        case FUNCT3_SLT:
            vm->regs[rd] = (s64)vm->regs[rs1] < (s64)imm;
            break;
        case FUNCT3_SLTU:
            vm->regs[rd] = vm->regs[rs1] < (u64)imm;
            break;
        case FUNCT3_XOR:
            vm->regs[rd] = vm->regs[rs1] ^ imm;
            break;
        case FUNCT3_OR:
            vm->regs[rd] = vm->regs[rs1] | imm;
            break;
        case FUNCT3_AND:
            vm->regs[rd] = vm->regs[rs1] & imm;
            break;
        case FUNCT3_SLL:
            vm->regs[rd] = vm->regs[rs1] << (imm & 0x3F);
            break;
        case FUNCT3_SRL_SRA: {
            u8 funct7 = (inst >> 25) & 0x7F;
            if (funct7 == FUNCT7_SRL) {
                vm->regs[rd] = vm->regs[rs1] >> (imm & 0x3F);
            } else if (funct7 == FUNCT7_SRA) {
                vm->regs[rd] = (s64)vm->regs[rs1] >> (imm & 0x3F);
            }
            break;
        }
        default:
            // TODO: invalid instruction
            break;
        }
    }; break;
    /* 
     * I-type load 
     *
     * Handles:
     * LB, LH, LW, LD, LBU, LHU, LWU
     */
    case OPCODE_LOAD: {
        vm->pc += 4;
        u8 rd = (inst >> 7) & 0x1F;
        u8 funct3 = (inst >> 12) & 0x07;
        u8 rs1 = (inst >> 15) & 0x1F;
        s32 imm = ((s32)inst) >> 20;

        u64 addr = vm->regs[rs1] + imm;

        switch (funct3) {
        case FUNCT3_LB:
            vm->regs[rd] = (s8)mem_load8(vm, addr);
            break;
        case FUNCT3_LH:
            vm->regs[rd] = (s16)mem_load16(vm, addr);
            break;
        case FUNCT3_LW:
            vm->regs[rd] = (s32)mem_load32(vm, addr);
            break;
        case FUNCT3_LD:
            vm->regs[rd] = (s64)mem_load64(vm, addr);
            break;
        case FUNCT3_LBU:
            vm->regs[rd] = mem_load8(vm, addr);
            break;
        case FUNCT3_LHU:
            vm->regs[rd] = mem_load16(vm, addr);
            break;
        case FUNCT3_LWU:
            vm->regs[rd] = mem_load32(vm, addr);
            break;
        default:
            // TODO: invalid load funct3
            break;
        }
    }; break;
   /*
    * S-type:
    *   31      25 24   20 19   15 14   12 11   7 6     0
    *  [ imm[11:5] ][ rs2 ][ rs1 ][funct3][ imm][opcode]
    *
    * Handles:
    * SB, SH, SW, SD.
    */
    case OPCODE_STORE: {
        vm->pc += 4;
        u8 imm4_0 = (inst >> 7) & 0x1F;
        u8 funct3 = (inst >> 12) & 0x07;
        u8 rs1 = (inst >> 15) & 0x1F;
        u8 rs2 = (inst >> 20) & 0x1F;
        u8 imm11_5 = (inst >> 25) & 0x7F;

        s32 imm = sign_extend((imm11_5 << 5) | imm4_0, 12);
        u64 addr = vm->regs[rs1] + imm;
        u64 value = vm->regs[rs2];

        switch (funct3) {
        case FUNCT3_SB:
            mem_store8(vm, addr, (u8)value);
            break;
        case FUNCT3_SH:
            mem_store16(vm, addr, (u16)value);
            break;
        case FUNCT3_SW:
            mem_store32(vm, addr, (u32)value);
            break;
        case FUNCT3_SD:
            mem_store64(vm, addr, value);
            break;
        default:
            // TODO: invalid store funct3
            break;
        }
    }; break;

    /* 
     * B-type
     *
     * Handles:
     * BEQ, BNE, BLT, BGE, BLTU, BGEU
     */
    /*
    case OPCODE_BRANCH: {
    }; break;
    */

    // TODO: U and J types

    }
}


int test(void)
{
    RiscVM vm = {0};

    vm.regs[2] = 10;
    vm.regs[3] = 20;
    vm.regs[4] = 1;
    vm.regs[5] = 3;
    vm.regs[6] = 7;
    vm.regs[9] = 2;

    u32 insts[] = {
        encode_rtype(7, 2, 3, OPCODE_OP, FUNCT7_ADD, FUNCT3_ADD_SUB),        // x7 = x2 + x3 = 30
        encode_rtype(8, 4, 9, OPCODE_OP, FUNCT7_SLL, FUNCT3_SLL),            // x8 = x4 << x9 = 4
        encode_rtype(9, 5, 8, OPCODE_OP, FUNCT7_OR, FUNCT3_OR),              // x9 = x5 | x8 = 7
        encode_rtype(10, 7, 9, OPCODE_OP, FUNCT7_XOR, FUNCT3_XOR),           // x10 = 30 ^ 7 = 25
        encode_rtype(1, 10, 6, OPCODE_OP, FUNCT7_SUB, FUNCT3_ADD_SUB),       // x1 = 25 - 7 = 18
        encode_itype_op(11, 2, 42, OPCODE_OP_IMM, FUNCT3_ADD_SUB),           // x11 = x2 + 42 = 52
        encode_itype_op(12, 2, 0x0F0, OPCODE_OP_IMM, FUNCT3_AND),            // x12 = x2 & 0xF0 = 0
        encode_itype_op(13, 2, 3, OPCODE_OP_IMM, FUNCT3_OR),                 // x13 = x2 | 3 = 11
        encode_itype_op(14, 2, 1, OPCODE_OP_IMM, FUNCT3_SLL),                // x14 = x2 << 1 = 20
        encode_itype_op(15, 2, 1, OPCODE_OP_IMM, FUNCT3_SRL_SRA),            // x15 = x2 >> 1 (logical) = 5
        encode_itype_op(16, 2, -1, OPCODE_OP_IMM, FUNCT3_XOR),               // x16 = x2 ^ -1 = ~x2 = -11
        encode_itype_op(17, 2, -5, OPCODE_OP_IMM, FUNCT3_SLT),               // x17 = (10 < -5)? 0 : 0
        encode_itype_op(18, 2, 20, OPCODE_OP_IMM, FUNCT3_SLT),               // x18 = (10 < 20)? 1 : 0
    };

    for (int i = 0; i < sizeof(insts)/sizeof(insts[0]); i++) {
        execute_instruction(&vm, insts[i]);
    }

    printf("x1  = %zu\n", vm.regs[1]);  // 18
    printf("x11 = %zu\n", vm.regs[11]); // 52
    printf("x12 = %zu\n", vm.regs[12]); // 0
    printf("x13 = %zu\n", vm.regs[13]); // 11
    printf("x14 = %zu\n", vm.regs[14]); // 20
    printf("x15 = %zu\n", vm.regs[15]); // 5
    printf("x16 = %zd\n", vm.regs[16]); // -11
    printf("x17 = %zu\n", vm.regs[17]); // 0
    printf("x18 = %zu\n", vm.regs[18]); // 1
}

int test2(void) 
{
    RiscVM vm = {0};

    vm.regs[REG_SP] = 100;
    vm.regs[REG_GP] = 1234;
    vm.regs[REG_TP] = -1;

    u32 insts[] = {
        // sw gp, 0(sp)
        encode_stype(REG_SP, REG_GP, 0, OPCODE_STORE, FUNCT3_SW),
        // sb tp, 4(sp)
        encode_stype(REG_SP, REG_TP, 4, OPCODE_STORE, FUNCT3_SB),
        // lw t0, 0(sp)
        encode_itype_load(REG_T0, REG_SP, 0, FUNCT3_LW),
        // lb t1, 4(sp)
        encode_itype_load(REG_T1, REG_SP, 4, FUNCT3_LB),
        // lbu t2, 4(sp)
        encode_itype_load(REG_T2, REG_SP, 4, FUNCT3_LBU),
        // add s0, t0, t1
        encode_rtype(REG_S0, REG_T0, REG_T1, OPCODE_OP, FUNCT7_ADD, FUNCT3_ADD_SUB),
        // addi s1, s0, 1000
        encode_itype_op(REG_S1, REG_S0, 1000, OPCODE_OP_IMM, FUNCT3_ADD_SUB),
    };

    for (int i = 0; i < sizeof(insts) / sizeof(insts[0]); i++) {
        execute_instruction(&vm, insts[i]);
    }

    printf("x5  (t0) = %lld (expected 1234)\n", vm.regs[REG_T0]);
    printf("x6  (t1) = %lld (expected -1)\n", vm.regs[REG_T1]);
    printf("x7  (t2) = %lld (expected 255)\n", vm.regs[REG_T2]);
    printf("x8  (s0) = %lld (expected 1233)\n", vm.regs[REG_S0]);
    printf("x9  (s1) = %lld (expected 2233)\n", vm.regs[REG_S1]);
}

int main(void)
{
    /*
     * List of TODOS:
     * - 32-bit arithmetic
     * - Handle invalid instruction encoding
     * - Memory
     * - Traps, exceptions, interrupts
     * - Some way to explictily halt. Other SYSTEM stuff.
     * - Make sure x0 cannot be written to.
     * - Raise exception if traget address for a jump is not aligned
     * - Nice way to debug state of VM.
     * - Single-step execution.
     *
     * Testing etc:
     * - Bunch of regression tests
     * - Some kind of API encode instructions.
     * - Simple assembler. Useful for testing.
     */

    test2();
}
