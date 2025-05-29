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
#include "riscvm.h"


/* Converts a signed number from a smaller bit-width to a larger one without changing its value */
// TODO: Figure out if this is actually correct.
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

bool execute_instruction(RiscVM *vm, u32 inst)
{
    vm->steps += 1;
    u8 opcode = inst & 0x7F;

    switch (opcode) {
    default:
        printf("Unknown opcode: 0x%02X\n", opcode);
        break;

    case OPCODE_HALT:
        return true;

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
     * B-type:
     * 31   30-25   24-20   19-15   14-12   11-8   7   6-0
     * [imm12][imm10:5][ rs2 ][ rs1 ][funct3][imm4:1][imm11][opcode]
     *
     * Handles:
     * BEQ, BNE, BLT, BGE, BLTU, BGEU
     *
     * NOTE:
     *  - The immediate is always signed, and PC-relative, so must be sign-extended and added to pc.
     *  - The LSB of the immediate is always 0.
     */
    case OPCODE_BRANCH: {
        u8 funct3 = (inst >> 12) & 0x7;
        u8 rs1 = (inst >> 15) & 0x1F;
        u8 rs2 = (inst >> 20) & 0x1F;

        u32 imm11 = (inst >> 7) & 0x1;
        u32 imm4_1 = (inst >> 8) & 0xF;
        u32 imm10_5 = (inst >> 25) & 0x3F;
        u32 imm12 = (inst >> 31) & 0x1;

        s32 imm = (imm12 << 12) | (imm11 << 11) | (imm10_5 << 5) | (imm4_1 << 1);
        imm = sign_extend(imm, 13);

        bool do_branch = false;
        switch (funct3) {
        case FUNCT3_BEQ:
            do_branch = (vm->regs[rs1] == vm->regs[rs2]);
            break;
        case FUNCT3_BNE:
            do_branch = (vm->regs[rs1] != vm->regs[rs2]);
            break;
        case FUNCT3_BLT:
            do_branch = ((s64)vm->regs[rs1] < (s64)vm->regs[rs2]);
            break;
        case FUNCT3_BGE:
            do_branch = ((s64)vm->regs[rs1] >= (s64)vm->regs[rs2]);
            break;
        case FUNCT3_BLTU:
            do_branch = (vm->regs[rs1] < vm->regs[rs2]);
            break;
        case FUNCT3_BGEU:
            do_branch = (vm->regs[rs1] >= vm->regs[rs2]);
            break;
        default:
            // TODO: Handle invalid funct3
            break;
        }

        if (do_branch) {
            vm->pc += imm;
        } else {
            vm->pc += 4;
        }
    }; break;

    /* U-type  */
    case OPCODE_LUI: {
        u8 rd = (inst >> 7) & 0x1F;
        s32 imm = inst & 0xFFFFF000;
        vm->regs[rd] = imm;
        vm->pc += 4;
    }; break;
    case OPCODE_AUIPC: {
        u8 rd = (inst >> 7) & 0x1F;
        s32 imm = inst & 0xFFFFF000;
        vm->regs[rd] = vm->pc + imm;
        vm->pc += 4;
    }; break;

    case OPCODE_JAL: {
        u8 rd = (inst >> 7) & 0x1F;
        s32 imm =
            ((inst >> 21) & 0x3FF) << 1 |  // imm[10:1]
            ((inst >> 20) & 0x1) << 11 |   // imm[11]
            ((inst >> 12) & 0xFF) << 12 |  // imm[19:12]
            ((inst >> 31) & 0x1) << 20;    // imm[20]

        imm = sign_extend(imm, 21);
        vm->regs[rd] = vm->pc + 4;
        vm->pc += imm;
    }; break;
    case OPCODE_JALR: {
        u8 rd = (inst >> 7) & 0x1F;
        u8 funct3 = (inst >> 12) & 0x07;
        u8 rs1 = (inst >> 15) & 0x1F;
        s32 imm = ((s32)inst) >> 20; // full sign-extend

        u64 target = (vm->regs[rs1] + imm) & ~1ULL;
        vm->regs[rd] = vm->pc + 4;
        vm->pc = target;
    }; break;

    }

    return false;
}


void execute_until_halt(RiscVM *vm, u32 instructions[1024])
{
    while (!execute_instruction(vm, instructions[vm->pc >> 2]));
}

/*
 * List of TODOS:
 * - 32-bit arithmetic
 * - Handle invalid instruction encoding
 * - Proper memory
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
