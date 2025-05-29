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
#ifndef RISCVM_H
#define RISCVM_H

/* Types */
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
    OPCODE_HALT      = 0b0000000, // Custom special Opcode.

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


/* Functions */
bool execute_instruction(RiscVM *vm, u32 inst);
void execute_until_halt(RiscVM *vm, u32 instructions[1024]);



#endif // RISCVM_H
