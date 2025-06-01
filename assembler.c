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
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "base.h"
#include "riscvm.h"
#include "assembler.h"


char *regs[] = {
    "x0",  "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",
    "x8",  "x9",  "x10", "x11", "x12", "x13", "x14", "x15",
    "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
    "x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31"
};

char *reg_names[] = {
    "zero", "ra",  "sp",  "gp",  "tp",  "t0",  "t1",  "t2",
    "s0",   "s1",  "a0",  "a1",  "a2",  "a3",  "a4",  "a5",
    "a6",   "a7",  "s2",  "s3",  "s4",  "s5",  "s6",  "s7",
    "s8",   "s9",  "s10", "s11", "t3",  "t4",  "t5",  "t6"
};

char *mnemonics[] = {
    "lui", "auipc", "jal", "jalr",
    "beq", "bne", "blt", "bge", "bltu", "bgeu",
    "lb", "lh", "lw", "lbu", "lhu", "lwu", "ld",
    "sb", "sh", "sw", "sd",
    "addi", "slti", "sltiu", "xori", "ori", "andi", "slli", "srli", "srai",
    "add", "sub", "sll", "slt", "sltu", "xor", "srl", "sra", "or", "and",
    "addiw", "slliw", "srliw", "sraiw", "addw", "subw", "sllw", "srlw", "sraw",

    "halt",
    /* Pseudo instructions */ 
    "call", "mv"
};

typedef enum {
    MNEMONIC_LUI = 0, MNEMONIC_AUIPC, MNEMONIC_JAL, MNEMONIC_JALR,

    MNEMONIC_BEQ, MNEMONIC_BNE, MNEMONIC_BLT, MNEMONIC_BGE, MNEMONIC_BLTU, MNEMONIC_BGEU,

    MNEMONIC_LB, MNEMONIC_LH, MNEMONIC_LW, MNEMONIC_LBU, MNEMONIC_LHU, MNEMONIC_LWU, MNEMONIC_LD,

    MNEMONIC_SB, MNEMONIC_SH, MNEMONIC_SW, MNEMONIC_SD,

    MNEMONIC_ADDI, MNEMONIC_SLTI, MNEMONIC_SLTIU, MNEMONIC_XORI, MNEMONIC_ORI, MNEMONIC_ANDI,
    MNEMONIC_SLLI, MNEMONIC_SRLI, MNEMONIC_SRAI,

    MNEMONIC_ADD, MNEMONIC_SUB, MNEMONIC_SLL, MNEMONIC_SLT, MNEMONIC_SLTU, MNEMONIC_XOR, 
    MNEMONIC_SRL, MNEMONIC_SRA, MNEMONIC_OR, MNEMONIC_AND,

    MNEMONIC_ADDIW, MNEMONIC_SLLIW, MNEMONIC_SRLIW, MNEMONIC_SRAIW,

    MNEMONIC_ADDW, MNEMONIC_SUBW, MNEMONIC_SLLW, MNEMONIC_SRLW, MNEMONIC_SRAW,

    MNEMONIC_PSEUDO_HALT, MNEMONIC_PSEUDO_CALL, MNEMONIC_PSEUDO_MV,

    MNEMONIC_COUNT
} Mnemonic;

// TODO: Goofy
u8 mnemonic_operand_count[MNEMONIC_COUNT] = {
    [MNEMONIC_LUI]    = 2,
    [MNEMONIC_AUIPC]  = 2,
    [MNEMONIC_JAL]    = 2,
    [MNEMONIC_JALR]   = 3,

    [MNEMONIC_BEQ]    = 3,
    [MNEMONIC_BNE]    = 3,
    [MNEMONIC_BLT]    = 3,
    [MNEMONIC_BGE]    = 3,
    [MNEMONIC_BLTU]   = 3,
    [MNEMONIC_BGEU]   = 3,

    [MNEMONIC_LB]     = 3,
    [MNEMONIC_LH]     = 3,
    [MNEMONIC_LW]     = 2,
    [MNEMONIC_LBU]    = 3,
    [MNEMONIC_LHU]    = 3,
    [MNEMONIC_LWU]    = 3,
    [MNEMONIC_LD]     = 3,

    [MNEMONIC_SB]     = 3,
    [MNEMONIC_SH]     = 3,
    [MNEMONIC_SW]     = 2,
    [MNEMONIC_SD]     = 3,

    [MNEMONIC_ADDI]   = 3,
    [MNEMONIC_SLTI]   = 3,
    [MNEMONIC_SLTIU]  = 3,
    [MNEMONIC_XORI]   = 3,
    [MNEMONIC_ORI]    = 3,
    [MNEMONIC_ANDI]   = 3,
    [MNEMONIC_SLLI]   = 3,
    [MNEMONIC_SRLI]   = 3,
    [MNEMONIC_SRAI]   = 3,

    [MNEMONIC_ADD]    = 3,
    [MNEMONIC_SUB]    = 3,
    [MNEMONIC_SLL]    = 3,
    [MNEMONIC_SLT]    = 3,
    [MNEMONIC_SLTU]   = 3,
    [MNEMONIC_XOR]    = 3,
    [MNEMONIC_SRL]    = 3,
    [MNEMONIC_SRA]    = 3,
    [MNEMONIC_OR]     = 3,
    [MNEMONIC_AND]    = 3,

    [MNEMONIC_ADDIW]  = 3,
    [MNEMONIC_SLLIW]  = 3,
    [MNEMONIC_SRLIW]  = 3,
    [MNEMONIC_SRAIW]  = 3,

    [MNEMONIC_ADDW]   = 3,
    [MNEMONIC_SUBW]   = 3,
    [MNEMONIC_SLLW]   = 3,
    [MNEMONIC_SRLW]   = 3,
    [MNEMONIC_SRAW]   = 3,

    [MNEMONIC_PSEUDO_HALT] = 0,
    [MNEMONIC_PSEUDO_CALL] = 1,
    [MNEMONIC_PSEUDO_MV]   = 2,
};

typedef struct {
    u8 *data;
    u32 data_len;
    u32 pos; // Used during parsing
    bool had_error;

    s32 inst_count;
    s32 instruction_stream_byte_offset;
    /* 
     * key: str8, 
     * value: u32: byte offset in instruction stream + 1. +1 so 0 is not treated as NULL
     */
    HashMap labels;

    size_t instructions_allocated;
    u32 *instructions;
} Assembler;

typedef enum {
    OP_REG,
    OP_IMM,
    OP_INDIRECT, // Has both reg and imm
    OP_LABEL,
} OperandKind;

typedef struct {
    OperandKind kind;
    s32 reg_id;
    u32 imm;
    u8 label[64];
} Operand;


static u8 next_u8(Assembler *ass)
{
    u8 c = ass->data[ass->pos];
    ass->pos++;
    return c;
}

static u32 parse_u32(u8 *s)
{
    return strtoul((char*)s, NULL, 0);
}

static void skip_spaces(Assembler *ass)
{
    while (ass->data[ass->pos] == ' ' || ass->data[ass->pos] == '\t') {
        ass->pos++;
    }
}

static void skip_until_newline(Assembler *ass)
{
    while (ass->pos < ass->data_len && ass->data[ass->pos] != '\n') {
        ass->pos++;
    }
    if (ass->pos < ass->data_len) {
        ass->pos++; // Skips the newline itself
    }
}

static s32 match_mnemonic(u8 mnemonic[8])
{
    for (u32 i = 0; i < MNEMONIC_COUNT; i++) {
        if (strcmp(mnemonic, mnemonics[i]) == 0) {
            return i;
        }
    }
    return -1;
}

static s32 match_reg(u8 *name)
{
    for (s32 i = 0; i < 32; i++) {
        if (strcmp(name, regs[i]) == 0 || strcmp(name, reg_names[i]) == 0) {
            return i;
        }
    }
    return -1;
}

static Operand read_token(Assembler *ass)
{
    /*
     * if digit -> parse imm. if open paren, parse reg, match reg
     * else     -> parse reg, math reg
     */
    Operand operand = { .kind = OP_REG, .reg_id = -1, .imm = 0 };
    skip_spaces(ass);
    u8 c = next_u8(ass);
    ass->pos--; // Not ideal, but we're only peeking c here
    u32 i = 0;  

    /* Parse immediate */
    u8 imm[32];
    if ((c >= '0' && c <= '9') || c == '-') {
        while (ass->pos < ass->data_len) {
            c = next_u8(ass);
            if (c == ',' || c == '\n' || c == ' ' || c == '\t' || c == '(') break;
            imm[i++] = c;
            if (i == 31) break;
        }
        ass->pos--;
        imm[i] = 0;
        operand.imm = parse_u32(imm);
        if (c != '(') {
            operand.kind = OP_IMM;
        } else {
            ass->pos++; // skip over '('
            operand.kind = OP_INDIRECT;
        }
    }

    skip_spaces(ass);

    /* Parse register */
    i = 0;
    if (operand.kind != OP_IMM) {
        while (ass->pos < ass->data_len) {
            c = next_u8(ass);
            if (c == ',' || c == '\n' || c == ' ' || c == '\t' || c == ')') break;
            operand.label[i++] = c;
            if (i == 63) break;
        }
        ass->pos--;
        operand.label[i] = 0;
        operand.reg_id = match_reg(operand.label);
        if (operand.reg_id == -1) {
            operand.kind = OP_LABEL;
        }
    }

    skip_spaces(ass);
    if (ass->data[ass->pos] == ',') {
        ass->pos++;
    }
    return operand;
}

/*
 * funct7[31:25] | rs2[24:20] | rs1[19:15] | funct3[14:12] | rd[11:7] | opcode[6:0]
 */
static u32 encode_rtype(u8 rd, u8 rs1, u8 rs2, u8 opcode, u8 funct7, u8 funct3)
{
    return (funct7 << 25) |
           (rs2 << 20) |
           (rs1 << 15) |
           (funct3 << 12) |
           (rd << 7) |
           opcode;
}

/* 
 * imm[31:20] | rs1[19:15] | funct3[14:12] | rd[11:7] | opcode[6:0]
 */
static u32 encode_itype_op(u8 rd, u8 rs1, s32 imm, u8 opcode, u8 funct3)
{
    return ((imm & 0xFFF) << 20) |
           (rs1 << 15) |
           (funct3 << 12) |
           (rd << 7) |
           opcode;
}

static u32 encode_itype_load(u8 rd, u8 rs1, s32 imm, u8 funct3)
{
    return ((imm & 0xFFF) << 20) |
           (rs1 << 15) |
           (funct3 << 12) |
           (rd << 7) |
           OPCODE_LOAD;
}

/*
 * imm[31:25] | rs2[24:20] | rs1[19:15] | funct3[14:12] | imm[11:7] | opcode[6:0]
 */
static u32 encode_stype(u8 rs1, u8 rs2, s32 imm, u8 opcode, u8 funct3)
{
    u32 imm11_5 = (imm >> 5) & 0x7F;
    u32 imm4_0  = imm & 0x1F;
    return (imm11_5 << 25) |
           (rs2 << 20) |
           (rs1 << 15) |
           (funct3 << 12) |
           (imm4_0 << 7) |
           opcode;
}

/* 
 * imm[12|10:5] | rs2[24:20] | rs1[19:15] | funct3[14:12] | imm[4:1|11] | opcode[6:0] 
 * Note: 13-bit immediate is scrambled across multiple fields, bit 0 is always 0 (2-byte aligned)
 */
static u32 encode_btype(u8 rs1, u8 rs2, s32 imm, u8 opcode, u8 funct3)
{
    /* The immediate is funnily encoded */
    u32 imm12 = (imm >> 12) & 0x1;
    u32 imm11 = (imm >> 11) & 0x1;
    u32 imm10_5 = (imm >> 5) & 0x3F;
    u32 imm4_1 = (imm >> 1) & 0xF;
    
    return (imm12 << 31) |
           (imm10_5 << 25) |
           (rs2 << 20) |
           (rs1 << 15) |
           (funct3 << 12) |
           (imm4_1 << 8) |
           (imm11 << 7) |
           opcode;
}
/* 
 * imm[31:12] | rd[11:7] | opcode[6:0]
 * Note: LUI, AUIPC
 */
static u32 encode_utype(u8 rd, s32 imm, u8 opcode)
{
    return ((imm & 0xFFFFF) << 12) |
           (rd << 7) |
           opcode;
}

/*
 * imm[20|10:1|11|19:12] | rd[11:7] | opcode[6:0]
 */
static u32 encode_jtype(u8 rd, s32 imm, u8 opcode)
{
    u32 imm20 = (imm >> 20) & 0x1;
    u32 imm19_12 = (imm >> 12) & 0xFF;
    u32 imm11 = (imm >> 11) & 0x1;
    u32 imm10_1 = (imm >> 1) & 0x3FF;
    
    return (imm20 << 31) |
           (imm10_1 << 21) |
           (imm11 << 20) |
           (imm19_12 << 12) |
           (rd << 7) |
           opcode;
}


static u32 encode_inst(Assembler *ass, u32 mnemonic_id, s32 rd, s32 rs1, s32 rs2, u32 imm)
{
    switch (mnemonic_id) {
    default:
        printf("Encoding not implemented for mnemonic '%s'\n", mnemonics[mnemonic_id]);
        ass->had_error = true;
        return 0;
    case MNEMONIC_PSEUDO_HALT:
        return 0;

    /* R-type */
    case MNEMONIC_ADD:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_ADD, FUNCT3_ADD_SUB);
    case MNEMONIC_SUB:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SUB, FUNCT3_ADD_SUB);
    case MNEMONIC_SLL:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SLL, FUNCT3_SLL);
    case MNEMONIC_SLT:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SLT, FUNCT3_SLT);
    case MNEMONIC_SLTU:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SLTU, FUNCT3_SLTU);
    case MNEMONIC_XOR:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_XOR, FUNCT3_XOR);
    case MNEMONIC_SRL:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SRL, FUNCT3_SRL_SRA);
    case MNEMONIC_SRA:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SRA, FUNCT3_SRL_SRA);
    case MNEMONIC_OR:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_OR, FUNCT3_OR);
    case MNEMONIC_AND:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_AND, FUNCT3_AND);
    /* Immediate here becomes the shift amount which has to be clamped to less than 63. */
    case MNEMONIC_SLLI:
        return encode_rtype(rd, rs1, imm & 63, OPCODE_OP_IMM, FUNCT7_SLL, FUNCT3_SLL);
    case MNEMONIC_SRLI:
        return encode_rtype(rd, rs1, imm & 63, OPCODE_OP_IMM, FUNCT7_SRL, FUNCT3_SRL_SRA);
    case MNEMONIC_SRAI:
        return encode_rtype(rd, rs1, imm & 63, OPCODE_OP_IMM, FUNCT7_SRA, FUNCT3_SRL_SRA);

    /* I-type */
    case MNEMONIC_ADDI:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_ADD_SUB);
    case MNEMONIC_SLTI:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_SLT);
    case MNEMONIC_SLTIU:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_SLTU);
    case MNEMONIC_XORI:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_XOR);
    case MNEMONIC_ORI:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_OR);
    case MNEMONIC_ANDI:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_AND);
    case MNEMONIC_JALR:
        return encode_itype_op(rd, rs1, imm, OPCODE_JALR, FUNCT3_JALR);
    /* I-type load */
    case MNEMONIC_LB:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LB);
    case MNEMONIC_LH:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LH);
    case MNEMONIC_LW:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LW);
    case MNEMONIC_LD:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LD);
    case MNEMONIC_LBU:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LBU);
    case MNEMONIC_LHU:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LHU);
    case MNEMONIC_LWU:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LWU);

    /* S-type */
    case MNEMONIC_SB:
        return encode_stype(rs1, rd, imm, OPCODE_STORE, FUNCT3_SB);
    case MNEMONIC_SH:
        return encode_stype(rs1, rd, imm, OPCODE_STORE, FUNCT3_SH);
    case MNEMONIC_SW:
        return encode_stype(rs1, rd, imm, OPCODE_STORE, FUNCT3_SW);
    case MNEMONIC_SD:
        return encode_stype(rs1, rd, imm, OPCODE_STORE, FUNCT3_SD);

    /* B-type */
    case MNEMONIC_BEQ:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BEQ);
    case MNEMONIC_BNE:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BNE);
    case MNEMONIC_BLT:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BLT);
    case MNEMONIC_BGE:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BGE);
    case MNEMONIC_BLTU:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BLTU);
    case MNEMONIC_BGEU:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BGEU);

    /* U-type */
    case MNEMONIC_LUI:
        return encode_utype(rd, imm, OPCODE_LUI);
    case MNEMONIC_AUIPC:
        return encode_utype(rd, imm, OPCODE_AUIPC);
        
    /* J-type */
    case MNEMONIC_JAL:
        return encode_jtype(rd, imm, OPCODE_JAL);
    }
}

static void resolve_labels(Assembler *ass)
{
    u8 label_name[255];
    u8 c;
    u32 i;

    while (!ass->had_error && ass->pos < ass->data_len) {
        i = 0;
        skip_spaces(ass);
        c = next_u8(ass);
        if (c == '#') {
            skip_until_newline(ass);
            continue;
        }

        /* Get label name */
        do {
            label_name[i++] = c;
            if (i == 255) {
                printf("Label name too large (max 255 chars).\n");
                ass->had_error = true;
                return;
            }
        } while ((c = next_u8(ass)) != ' ' && c != '\n');
        /* Empty label */
        if (i == 0) {
            printf("Empty label name.\n");
            ass->had_error = true;
            return;
        }
        label_name[i] = 0;
        /* Not a label. So an instruction. */
        if (label_name[i - 1] != ':') {
            // TODO: Pseudo instruction 'call' resolves into two instructions.
            //       I need to revisit this code and make it cleaner and not hardcoded for call.
            if (strcmp(label_name, "call") == 0) {
                ass->instruction_stream_byte_offset += 8;
            } else {
                ass->instruction_stream_byte_offset += 4;
            }
            skip_until_newline(ass);
            continue;
        }
        label_name[i - 1] = 0; // Replace ':' with null terminator

        // NOTE: Kind of stupid and hacky use of the hasmpap. Fix later.
        void *v = hashmap_sget(&ass->labels, label_name);
        if (v != NULL) {
            printf("Found duplicate labels '%s'\n", label_name);
            ass->had_error = true;
            return;
        }

        hashmap_sput(&ass->labels, label_name, 
                     (u32 *)(size_t)(ass->instruction_stream_byte_offset + 1), sizeof(u32 *), false);

        if (c != '\n') {
            skip_until_newline(ass);
        }
    }

    /* Reset pos before the next pass */
    ass->pos = 0;
    ass->instruction_stream_byte_offset = 0;
}


static void emit_instruction(Assembler *ass, u32 inst)
{
    if (ass->inst_count > ass->instructions_allocated) {
        ass->instructions_allocated *= 2;
        ass->instructions = realloc(ass->instructions, sizeof(u32) * ass->instructions_allocated);
    }
    ass->instructions[ass->inst_count] = inst;
    ass->inst_count++;
    ass->instruction_stream_byte_offset += 4;
}

static void assemble_next_inst(Assembler *ass)
{
    skip_spaces(ass);
    u8 c = next_u8(ass);

    /* If comment, skip entire line */
    if (c == '#') {
        skip_until_newline(ass);
        return assemble_next_inst(ass);
    }

    /* Parse mnemonic */
    u8 mnemonic[255] = {0};
    u32 i = 0;
    do {
        mnemonic[i++] = c;
        if (i == 255) {
            printf("Mnemonic or label too large (max 255).\n");
            ass->had_error = true;
            return;
        }
    } while ((c = next_u8(ass)) != ' ' && c != '\n');

    /* This is a label, not an instruction mnemonic */
    if (mnemonic[i - 1] == ':') {
        mnemonic[i - 1] = 0; // Repalce ':' with null terminator
        void *v = hashmap_sget(&ass->labels, mnemonic);
        if (v == NULL) {
            printf("Unknown label '%s'\n", mnemonic);
            ass->had_error = true;
            return;
        }
        /* Happy path */
        if (c != '\n') {
            skip_until_newline(ass);
        }
        //return assemble_next_inst(ass);
        return;
    }
    /* Only progress here if mnemonic is not a label */
    s32 mnemonic_id = match_mnemonic(mnemonic);
    if (mnemonic_id == -1) {
        printf("Unknown mnemonic '%s'\n", mnemonic);
        ass->had_error = true;
        return;
    }
#if 0
        printf("'%s'\n", mnemonic);
#endif

    /* If halt pseudo op then early return */
    if (mnemonic_id == MNEMONIC_PSEUDO_HALT) {
        emit_instruction(ass, 0);
        return;
    }

    /* Parse operands */
    u32 n_ops = mnemonic_operand_count[mnemonic_id];

    Operand ops[n_ops];
    for (u32 i = 0; i < n_ops; i++) {
        ops[i] = read_token(ass);
        if (ops[i].kind == OP_LABEL) {
            void *v = hashmap_sget(&ass->labels, ops[i].label);
            if (v != NULL) {
                /* PC-relative */
                ops[i].imm = (((u32)(size_t)v) - 1) - ass->instruction_stream_byte_offset;
            } else {
                printf("Could not resolve label '%s'\n", mnemonic);
                ass->had_error = true;
                return;
            }
        }
    }

#if 0
    printf("mnemonic: %s\n", mnemonic);
    printf("rd: %d\n", ops[0].reg_id);
    printf("rs1: %d\n", ops[1].reg_id);
    if (ops[2].reg_id != -1) {
        printf("rs2: %d\n", ops[2].reg_id);
    } else {
        printf("imm: %d\n", ops[2].imm);
    }
    printf("\n");
#endif

    skip_spaces(ass);
    skip_until_newline(ass);

    if (mnemonic_id == MNEMONIC_PSEUDO_CALL) {
        /* 
         * Transalte pseudo operation call into 
         * auipc ra, offser[31:12]
         * jalr ra, ra, offset[11:0]
         */
        if (ops[0].kind != OP_LABEL) {
            fprintf(stderr, "Pseudo op 'call' requires one operand: a label\n");
        }
        u32 target_offset = ops[0].imm + 4;
        u32 offset_high = (target_offset + 0x800) >> 12;
        u32 offset_low  = target_offset & 0xFFF;

        emit_instruction(ass, encode_inst(ass, MNEMONIC_AUIPC, REG_RA, -1, -1, offset_high));
        emit_instruction(ass, encode_inst(ass, MNEMONIC_JALR, REG_RA, REG_RA, -1, offset_low));
    } else if (mnemonic_id == MNEMONIC_PSEUDO_MV) {
        emit_instruction(ass, encode_inst(ass, MNEMONIC_ADDI, ops[0].reg_id, ops[1].reg_id, -1, 0));
    } else {
        u32 imm = ops[2].imm;
        if (n_ops == 2) {
            imm = ops[1].imm;
        }
        emit_instruction(ass, encode_inst(ass, mnemonic_id, ops[0].reg_id, ops[1].reg_id, ops[2].reg_id, imm));
    }
}

BinaryBuf read_file(u8 *filename)
{
    BinaryBuf buf = { .data = NULL };
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        fprintf(stderr, "Could not open file '%s'\n", filename);
        return buf;
    }
    struct stat st;
    if (stat(filename, &st) != 0) {
        fprintf(stderr, "Could not find file '%s'\n", filename);
        fclose(fp);
        return buf;
    }

    buf.size = st.st_size;
    buf.data = malloc(sizeof(u8) * (buf.size + 1));
    buf.data[buf.size] = 0;
    if (fread(buf.data, sizeof(u8), st.st_size, fp) != buf.size) {
        LOG_FATAL("Could not read file '%s'", filename);
        free(buf.data);
        fclose(fp);
        return buf;
    }
    fclose(fp);
    return buf;
}

BinaryBuf assemble_file(u8 *input_file)
{
    /* Read input file in its entirety */
    BinaryBuf input_data = read_file(input_file);
    if (input_data.data == NULL) {
        return (BinaryBuf){ .data = NULL };
    }

    Assembler ass = {0};
    ass.data = input_data.data;
    ass.data_len = input_data.size;
    hashmap_init(&ass.labels);
    ass.instructions_allocated = 128;
    ass.instructions = malloc(sizeof(u32) * ass.instructions_allocated);

    /* First pass. Label resolution. */
    resolve_labels(&ass);

    /* Second pass. Code gen. */
    size_t instructions_allocated = 128;
    while (!ass.had_error && ass.pos < ass.data_len) {
        assemble_next_inst(&ass);
    }

    /* Cleanup */
    free(input_data.data);
    hashmap_free(&ass.labels);

    if (ass.had_error) {
        return (BinaryBuf){ .data = NULL };
    }

    return (BinaryBuf){ .size = ass.inst_count, .data = (u8 *)ass.instructions };
}
