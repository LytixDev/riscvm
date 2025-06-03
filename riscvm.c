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
#include "base.h"
#include "riscvm.h"


static s32 sign_extend_32(s32 imm, s32 bit_width) 
{
    int shift = 32 - bit_width;
    return (imm << shift) >> shift;
}

static s64 sign_extend_64(s64 imm, s32 bit_width) 
{
    int shift = 64 - bit_width;
    return (imm << shift) >> shift;
}

// TODO: Ensure correct alignment, bounds check, and more
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
    u32 word = *(u32 *)&vm->memory[addr];
    //printf("%lx - %ld\n", addr, (s64)word);
    return word;
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

static inline void reg_write(RiscVM *vm, u32 rd, u64 value)
{
    /* x0 always hardwired to be zero */
    if (rd == 0) {
        return;
    }
    // printf("write to %d :: %lu (s64 %ld)\n", rd, value, (s64)value);
    vm->regs[rd] = value;
}

// #define DEBUG

static void debug_print(char *fmt, ...)
{
#ifdef DEBUG
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
#endif
}

bool execute_instruction(RiscVM *vm, u32 inst)
{
    vm->steps += 1;
    u8 opcode = inst & 0b1111111; // First 7 bits are the opcode

    switch (opcode) {
    default:
        printf("Unknown opcode: 0x%02X\n", opcode);
        debug_print("Unknown instruction 0x%08X\n", inst);
        break;

    case OPCODE_HALT:
        debug_print("HALT\n");
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
        u8 rd = (inst >> 7) & 0b11111; // 5 bits for rd
        u8 funct3 = (inst >> 12) & 0b111;
        u8 rs1 = (inst >> 15) & 0b11111; // 5 bits for rs1
        u8 rs2 = (inst >> 20) & 0b11111; // 5 bits for rs2
        u8 funct7 = (inst >> 25) & 0b1111111;

        if (funct7 == FUNCT7_MEXTENSION) {
            switch (funct3) {
            case FUNCT3_MUL:
                reg_write(vm, rd, vm->regs[rs1] * vm->regs[rs2]);
                debug_print("MUL x%d, x%d, x%d\n", rd, rs1, rs2);
                break;
            case FUNCT3_DIV: {
                s64 dividend = vm->regs[rs1];
                s64 divisor = vm->regs[rs2];
                if (divisor == 0) {
                    // TODO: Exception?
                    reg_write(vm, rd, -1);
                } else {
                    reg_write(vm, rd, dividend / divisor);
                }
                debug_print("DIV x%d, x%d, x%d\n", rd, rs1, rs2);
            }; break;
            case FUNCT3_DIVU: {
                u64 dividend = (u64)vm->regs[rs1];
                u64 divisor = (u64)vm->regs[rs2];
                if (divisor == 0) {
                    // TODO: Exception?
                    reg_write(vm, rd, -1);
                } else {
                    reg_write(vm, rd, dividend / divisor);
                }
                debug_print("DIVU x%d, x%d, x%d\n", rd, rs1, rs2);
            }; break;
            case FUNCT3_REM: {
                s64 dividend = (s64)vm->regs[rs1];
                s64 divisor = (s64)vm->regs[rs2];
                if (divisor == 0) {
                    // TODO: Exception?
                    reg_write(vm, rd, -1);
                } else {
                    reg_write(vm, rd, dividend % divisor);
                }
                debug_print("REM x%d, x%d, x%d\n", rd, rs1, rs2);
            }; break;
            case FUNCT3_REMU: {
                u64 dividend = (u64)vm->regs[rs1];
                u64 divisor = (u64)vm->regs[rs2];
                if (divisor == 0) {
                    // TODO: Exception?
                    reg_write(vm, rd, -1);
                } else {
                    reg_write(vm, rd, dividend % divisor);
                }
                debug_print("REM x%d, x%d, x%d\n", rd, rs1, rs2);
            }; break;
            default:
                // TODO: invalid instruction
                debug_print("Unknown M-extension 32-bit funct3=0x%X\n", funct3);
                break;
            }
        } else {
            switch (funct3) {
            case FUNCT3_ADD_SUB:
                if (funct7 == FUNCT7_ADD) {
                    reg_write(vm, rd, vm->regs[rs1] + vm->regs[rs2]);
                    debug_print("ADD x%d, x%d, x%d\n", rd, rs1, rs2);
                } else if (funct7 == FUNCT7_SUB) {
                    reg_write(vm, rd, vm->regs[rs1] - vm->regs[rs2]);
                    debug_print("SUB x%d, x%d, x%d\n", rd, rs1, rs2);
                }
                break;
            case FUNCT3_SLL:
                // TODO: redundant to check for funct7? Ommited for the rest of the cases
                if (funct7 == FUNCT7_SLL) {
                    reg_write(vm, rd, vm->regs[rs1] << (vm->regs[rs2] & 63));
                    debug_print("SLL x%d, x%d, x%d\n", rd, rs1, rs2);
                }
                break;
            case FUNCT3_SLT:
                reg_write(vm, rd, (s64)vm->regs[rs1] < (s64)vm->regs[rs2]);
                debug_print("SLT x%d, x%d, x%d\n", rd, rs1, rs2);
                break;
            case FUNCT3_SLTU:
                reg_write(vm, rd, vm->regs[rs1] < vm->regs[rs2]);
                debug_print("SLTU x%d, x%d, x%d\n", rd, rs1, rs2);
                break;
            case FUNCT3_XOR:
                reg_write(vm, rd, vm->regs[rs1] ^ vm->regs[rs2]);
                debug_print("XOR x%d, x%d, x%d\n", rd, rs1, rs2);
                break;
            case FUNCT3_SRL_SRA:
                if (funct7 == FUNCT7_SRL) {
                    reg_write(vm, rd, vm->regs[rs1] >> (vm->regs[rs2] & 63));
                    debug_print("SRL x%d, x%d, x%d\n", rd, rs1, rs2);
                } else if (funct7 == FUNCT7_SRA) {
                    reg_write(vm, rd, (s64)vm->regs[rs1] >> (vm->regs[rs2] & 63));
                    debug_print("SRA x%d, x%d, x%d\n", rd, rs1, rs2);
                }
                break;
            case FUNCT3_OR:
                reg_write(vm, rd, vm->regs[rs1] | vm->regs[rs2]);
                debug_print("OR x%d, x%d, x%d\n", rd, rs1, rs2);
                break;
            case FUNCT3_AND:
                reg_write(vm, rd, vm->regs[rs1] & vm->regs[rs2]);
                debug_print("AND x%d, x%d, x%d\n", rd, rs1, rs2);
                break;
            default:
                // TODO: invalid instruction
                debug_print("Unknown R-type funct3=0x%X\n", funct3);
                break;
            }
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
        u8 rd = (inst >> 7) & 0b11111; // 5 bits for rd
        u8 funct3 = (inst >> 12) & 0b111;
        u8 rs1 = (inst >> 15) & 0b11111; // 5 bits for rs1
        s32 imm = ((s32)inst) >> 20;  // Arithmetic shift already sign-extends correctly

        switch (funct3) {
        case FUNCT3_ADD_SUB:
            reg_write(vm, rd, vm->regs[rs1] + imm);
            debug_print("ADDI x%d, x%d, %d\n", rd, rs1, imm);
            break;
        case FUNCT3_SLT:
            reg_write(vm, rd, (s64)vm->regs[rs1] < (s64)imm);
            debug_print("SLTI x%d, x%d, %d\n", rd, rs1, imm);
            break;
        case FUNCT3_SLTU:
            reg_write(vm, rd, vm->regs[rs1] < (s64)imm);
            debug_print("SLTIU x%d, x%d, %d\n", rd, rs1, imm);
            break;
        case FUNCT3_XOR:
            reg_write(vm, rd, vm->regs[rs1] ^ imm);
            debug_print("XORI x%d, x%d, %d\n", rd, rs1, imm);
            break;
        case FUNCT3_OR:
            reg_write(vm, rd, vm->regs[rs1] | imm);
            debug_print("ORI x%d, x%d, %d\n", rd, rs1, imm);
            break;
        case FUNCT3_AND:
            reg_write(vm, rd, vm->regs[rs1] & imm);
            debug_print("ANDI x%d, x%d, %d\n", rd, rs1, imm);
            break;
        case FUNCT3_SLL:
            reg_write(vm, rd, vm->regs[rs1] << (imm & 0x63));
            debug_print("SLLI x%d, x%d, %d\n", rd, rs1, (imm & 0x3F));
            break;
        case FUNCT3_SRL_SRA: {
            s32 shamt_max_6_bits = imm & 0x3F;
            u8 funct7 = (inst >> 25) & 0x7F;
            if (funct7 == FUNCT7_SRL) {
                reg_write(vm, rd, vm->regs[rs1] >> shamt_max_6_bits); // Logical right shift
                debug_print("SRLI x%d, x%d, %d\n", rd, rs1, shamt_max_6_bits);
            } else if (funct7 == FUNCT7_SRA) {
                reg_write(vm, rd, (s64)vm->regs[rs1] >> shamt_max_6_bits);  // Arithmetic right shift
                debug_print("SRAI x%d, x%d, %d\n", rd, rs1, shamt_max_6_bits);
            }
            break;
        }
        default:
            // TODO: invalid instruction
            debug_print("Unknown I-type funct3=0x%X\n", funct3);
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
        u8 rd = (inst >> 7) & 0b11111; // 5 bits for rd
        u8 funct3 = (inst >> 12) & 0b111;
        u8 rs1 = (inst >> 15) & 0b11111; // 5 bits for rs1
        s32 imm = ((s32)inst) >> 20;  // Arithmetic shift already sign-extends correctly

        u64 addr = vm->regs[rs1] + imm;

        switch (funct3) {
        case FUNCT3_LB:
            reg_write(vm, rd, (s8)mem_load8(vm, addr));
            debug_print("LB x%d, %d(x%d)\n", rd, imm, rs1);
            break;
        case FUNCT3_LH:
            reg_write(vm, rd, (s16)mem_load16(vm, addr));
            debug_print("LH x%d, %d(x%d)\n", rd, imm, rs1);
            break;
        case FUNCT3_LW:
            reg_write(vm, rd, (s32)mem_load32(vm, addr));
            debug_print("LW x%d, %d(x%d)\n", rd, imm, rs1);
            break;
        case FUNCT3_LD:
            reg_write(vm, rd, (s64)mem_load64(vm, addr));
            debug_print("LD x%d, %d(x%d)\n", rd, imm, rs1);
            break;
        case FUNCT3_LBU:
            reg_write(vm, rd, mem_load8(vm, addr));
            debug_print("LBU x%d, %d(x%d)\n", rd, imm, rs1);
            break;
        case FUNCT3_LHU:
            reg_write(vm, rd, mem_load16(vm, addr));
            debug_print("LHU x%d, %d(x%d)\n", rd, imm, rs1);
            break;
        case FUNCT3_LWU:
            reg_write(vm, rd, mem_load32(vm, addr));
            debug_print("LWU x%d, %d(x%d)\n", rd, imm, rs1);
            break;
        default:
            // TODO: invalid load funct3
            debug_print("Unknown LOAD funct3=0x%X\n", funct3);
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
        u8 imm4_0 = (inst >> 7) & 0b11111; // first 5 immediate bits
        u8 funct3 = (inst >> 12) & 0b111;
        u8 rs1 = (inst >> 15) & 0b11111; // 5 bits for rs1
        u8 rs2 = (inst >> 20) & 0b11111; // 5 bits for rs2
        u8 imm11_5 = (inst >> 25) & 0b1111111; // final 7 immediate bits
        // Splice together immediate and sign extend
        s32 imm = sign_extend_32((imm11_5 << 5) | imm4_0, 12);
        u64 addr = vm->regs[rs1] + imm;
        u64 value = vm->regs[rs2];

        switch (funct3) {
        case FUNCT3_SB:
            mem_store8(vm, addr, (u8)value);
            debug_print("SB x%d, %d(x%d)\n", rs2, imm, rs1);
            break;
        case FUNCT3_SH:
            mem_store16(vm, addr, (u16)value);
            debug_print("SH x%d, %d(x%d)\n", rs2, imm, rs1);
            break;
        case FUNCT3_SW:
            mem_store32(vm, addr, (u32)value);
            debug_print("SW x%d, %d(x%d)\n", rs2, imm, rs1);
            break;
        case FUNCT3_SD:
            mem_store64(vm, addr, value);
            debug_print("SD x%d, %d(x%d)\n", rs2, imm, rs1);
            break;
        default:
            // TODO: invalid store funct3
            debug_print("Unknown STORE funct3=0x%X\n", funct3);
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
        u8 funct3 = (inst >> 12) & 0b111;
        u8 rs1 = (inst >> 15) & 0b11111;
        u8 rs2 = (inst >> 20) & 0b11111;

        /* The immediate is funnily encoded */
        u32 imm11 = (inst >> 7) & 0x1; // First bit
        u32 imm4_1 = (inst >> 8) & 0xF; // Next five
        u32 imm10_5 = (inst >> 25) & 0x3F; // Next 6
        u32 imm12 = (inst >> 31) & 0x1; // Final
        s32 imm = (imm12 << 12) | (imm11 << 11) | (imm10_5 << 5) | (imm4_1 << 1);
        imm = sign_extend_32(imm, 13);

        bool do_branch = false;
        switch (funct3) {
        case FUNCT3_BEQ:
            do_branch = (vm->regs[rs1] == vm->regs[rs2]);
            debug_print("BEQ x%d, x%d, %d (taken=%s)\n", rs1, rs2, imm, do_branch ? "yes" : "no");
            break;
        case FUNCT3_BNE:
            do_branch = (vm->regs[rs1] != vm->regs[rs2]);
            debug_print("BNE x%d, x%d, %d (taken=%s)\n", rs1, rs2, imm, do_branch ? "yes" : "no");
            break;
        case FUNCT3_BLT:
            do_branch = ((s64)vm->regs[rs1] < (s64)vm->regs[rs2]);
            debug_print("BLT x%d, x%d, %d (taken=%s)\n", rs1, rs2, imm, do_branch ? "yes" : "no");
            break;
        case FUNCT3_BGE:
            do_branch = ((s64)vm->regs[rs1] >= (s64)vm->regs[rs2]);
            debug_print("BGE x%d, x%d, %d (taken=%s)\n", rs1, rs2, imm, do_branch ? "yes" : "no");
            break;
        case FUNCT3_BLTU:
            do_branch = (vm->regs[rs1] < vm->regs[rs2]);
            debug_print("BLTU x%d, x%d, %d (taken=%s)\n", rs1, rs2, imm, do_branch ? "yes" : "no");
            break;
        case FUNCT3_BGEU:
            do_branch = (vm->regs[rs1] >= vm->regs[rs2]);
            debug_print("BGEU x%d, x%d, %d (taken=%s)\n", rs1, rs2, imm, do_branch ? "yes" : "no");
            break;
        default:
            // TODO: Handle invalid funct3
            debug_print("Unknown BRANCH funct3=0x%X\n", funct3);
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
        u8 rd = (inst >> 7) & 0b11111;
        s32 imm = inst & 0xFFFFF000; // Everything but the first 3 bytes
        reg_write(vm, rd, imm);
        vm->pc += 4;
        debug_print("LUI x%d, 0x%X\n", rd, imm >> 12);
    }; break;
    case OPCODE_AUIPC: {
        u8 rd = (inst >> 7) & 0b11111;
        s32 imm = inst & 0xFFFFF000; // Everything but the first 3 bytes
        reg_write(vm, rd, vm->pc + imm);
        vm->pc += 4;
        debug_print("AUIPC x%d, 0x%X\n", rd, imm >> 12);
    }; break;

    case OPCODE_JAL: {
        u8 rd = (inst >> 7) & 0b11111; // 5 bits for rd
        u32 imm20 = (inst >> 31) & 0x1;     // imm[20] from bit 31
        u32 imm10_1 = (inst >> 21) & 0x3FF; // imm[10:1] from bits 30:21  
        u32 imm11 = (inst >> 20) & 0x1;     // imm[11] from bit 20
        u32 imm19_12 = (inst >> 12) & 0xFF; // imm[19:12] from bits 19:12
        /* Reconstruct the 21-bit immediate (bit 0 is always 0 for alignment) */
        s32 imm = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
        imm = sign_extend_32(imm, 21);
        
        reg_write(vm, rd, vm->pc + 4);
        vm->pc += imm;
        debug_print("JAL x%d, %d\n", rd, imm);
    }; break;
    case OPCODE_JALR: {
        u8 rd = (inst >> 7) & 0b11111;
        u8 funct3 = (inst >> 12) & 0b111;
        u8 rs1 = (inst >> 15) & 0b11111;
        s32 imm = ((s32)inst) >> 20; // Arithmetic shift already sign extends

        /* Clears the least significant bit (for alignment) */
        u64 target = (vm->regs[rs1] + imm) & ~1ULL;
        reg_write(vm, rd, vm->pc + 4);
        vm->pc = target;
        debug_print("JALR x%d, x%d, %d\n", rd, rs1, imm);
    }; break;

    }

    return false;
}

void dump_regs(RiscVM *vm, bool ignore_zero)
{
    for (u32 i = 0; i < 32; i++) {
        s64 value = vm->regs[i];
        if ((ignore_zero && value == 0) || i == 2) {
            continue;
        }
        printf("x%d = %ld\n", i, value);
    }
}

void dump_regs_to_buffer(RiscVM *vm, u8 *buf, size_t buf_size, bool ignore_zero)
{
    size_t offset = 0;
    for (u32 i = 0; i < 32; i++) {
        s64 value = vm->regs[i];
        /* Ignore stack pointer for now */
        if ((ignore_zero && value == 0) || i == 2) {
            continue;
        }
        int written = snprintf(buf + offset, buf_size - offset, "x%d = %ld\n", i, value);
        if (written < 0 || (size_t)written >= buf_size - offset) {
            // Prevent overflow
            break;
        }
        offset += written;
    }
}


void execute_until_halt(RiscVM *vm, u32 *instructions)
{
    vm->regs[REG_SP] = 0x10000;
    bool halt = false;
    while (!halt) {
        halt = execute_instruction(vm, instructions[vm->pc >> 2]);
    }
}

