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
    /* Pseudo instructions */ 
    "halt", "call", "mv"
};

typedef enum {
    M_LUI = 0, M_AUIPC, M_JAL, M_JALR,
    M_BEQ, M_BNE, M_BLT, M_BGE, M_BLTU, M_BGEU,
    M_LB, M_LH, M_LW, M_LBU, M_LHU, M_LWU, M_LD,
    M_SB, M_SH, M_SW, M_SD,
    M_ADDI, M_SLTI, M_SLTIU, M_XORI, M_ORI, M_ANDI,
    M_SLLI, M_SRLI, M_SRAI,
    M_ADD, M_SUB, M_SLL, M_SLT, M_SLTU, M_XOR, 
    M_SRL, M_SRA, M_OR, M_AND,
    M_ADDIW, M_SLLIW, M_SRLIW, M_SRAIW,
    M_ADDW, M_SUBW, M_SLLW, M_SRLW, M_SRAW,
    M_PSEUDO_HALT, M_PSEUDO_CALL, M_PSEUDO_MV,

    M_COUNT
} Mnemonic;

typedef struct {
    u8 *data;
    u32 data_len;
    u32 pos; // Holds the current position during parsing
    bool had_error;

    s32 inst_count;
    s32 inst_byte_offset;
    /* 
     * key: str8, 
     * value: u32: byte offset in instruction stream + 1. +1 so 0 is not treated as NULL
     */
    HashMap labels;

    size_t insts_allocated;
    u32 *insts;
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
    s32 imm;
    u8 label[64];
} Operand;


static u8 next_u8(Assembler *ass)
{
    u8 c = ass->data[ass->pos];
    ass->pos++;
    return c;
}

static s32 parse_s32(u8 *s)
{
    return strtoul((char*)s, NULL, 0);
}

static void skip_spaces(Assembler *ass)
{
    while (ass->data[ass->pos] == ' ' || ass->data[ass->pos] == '\t') {
        ass->pos++;
    }
}

static void skip_until_next_line(Assembler *ass)
{
    while (ass->pos < ass->data_len && next_u8(ass) != '\n');
    skip_spaces(ass);
    u8 c = next_u8(ass);
    ass->pos--;
    if (c == '\n') {
        skip_until_next_line(ass);
    }
}

static s32 match_mnemonic(u8 mnemonic[8])
{
    for (u32 i = 0; i < M_COUNT; i++) {
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

static u32 mnemonic_operand_count(u8 mnemonic)
{
    switch (mnemonic) {
    case M_LUI:
    case M_AUIPC:
    case M_JAL:
    case M_LW:
    case M_SB:
    case M_SH:
    case M_SW:
    case M_SD:
    case M_PSEUDO_MV:
        return 2;
    case M_PSEUDO_CALL:
        return 1;
    case M_PSEUDO_HALT:
        return 0;

    default:
        return 3;
    }
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
    case M_PSEUDO_HALT:
        return 0;

    /* R-type */
    case M_ADD:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_ADD, FUNCT3_ADD_SUB);
    case M_SUB:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SUB, FUNCT3_ADD_SUB);
    case M_SLL:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SLL, FUNCT3_SLL);
    case M_SLT:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SLT, FUNCT3_SLT);
    case M_SLTU:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SLTU, FUNCT3_SLTU);
    case M_XOR:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_XOR, FUNCT3_XOR);
    case M_SRL:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SRL, FUNCT3_SRL_SRA);
    case M_SRA:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_SRA, FUNCT3_SRL_SRA);
    case M_OR:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_OR, FUNCT3_OR);
    case M_AND:
        return encode_rtype(rd, rs1, rs2, OPCODE_OP, FUNCT7_AND, FUNCT3_AND);
    /* Immediate here becomes the shift amount which has to be clamped to less than 63. */
    case M_SLLI:
        return encode_rtype(rd, rs1, imm & 63, OPCODE_OP_IMM, FUNCT7_SLL, FUNCT3_SLL);
    case M_SRLI:
        return encode_rtype(rd, rs1, imm & 63, OPCODE_OP_IMM, FUNCT7_SRL, FUNCT3_SRL_SRA);
    case M_SRAI:
        return encode_rtype(rd, rs1, imm & 63, OPCODE_OP_IMM, FUNCT7_SRA, FUNCT3_SRL_SRA);

    /* I-type */
    case M_ADDI:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_ADD_SUB);
    case M_SLTI:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_SLT);
    case M_SLTIU:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_SLTU);
    case M_XORI:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_XOR);
    case M_ORI:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_OR);
    case M_ANDI:
        return encode_itype_op(rd, rs1, imm, OPCODE_OP_IMM, FUNCT3_AND);
    case M_JALR:
        return encode_itype_op(rd, rs1, imm, OPCODE_JALR, FUNCT3_JALR);
    /* I-type load */
    case M_LB:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LB);
    case M_LH:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LH);
    case M_LW:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LW);
    case M_LD:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LD);
    case M_LBU:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LBU);
    case M_LHU:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LHU);
    case M_LWU:
        return encode_itype_load(rd, rs1, imm, FUNCT3_LWU);

    /* S-type */
    case M_SB:
        return encode_stype(rs1, rd, imm, OPCODE_STORE, FUNCT3_SB);
    case M_SH:
        return encode_stype(rs1, rd, imm, OPCODE_STORE, FUNCT3_SH);
    case M_SW:
        return encode_stype(rs1, rd, imm, OPCODE_STORE, FUNCT3_SW);
    case M_SD:
        return encode_stype(rs1, rd, imm, OPCODE_STORE, FUNCT3_SD);

    /* B-type */
    case M_BEQ:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BEQ);
    case M_BNE:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BNE);
    case M_BLT:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BLT);
    case M_BGE:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BGE);
    case M_BLTU:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BLTU);
    case M_BGEU:
        return encode_btype(rd, rs1, imm, OPCODE_BRANCH, FUNCT3_BGEU);

    /* U-type */
    case M_LUI:
        return encode_utype(rd, imm, OPCODE_LUI);
    case M_AUIPC:
        return encode_utype(rd, imm, OPCODE_AUIPC);
        
    /* J-type */
    case M_JAL:
        return encode_jtype(rd, imm, OPCODE_JAL);
    }
}

static Operand next_operand(Assembler *ass)
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
        operand.imm = parse_s32(imm);
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
            skip_until_next_line(ass);
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
        ass->pos--;
        label_name[i] = 0;

        /* Empty label */
        if (i == 0) {
            printf("Empty label name.\n");
            ass->had_error = true;
            return;
        }
        /* Not a label. So an instruction. */
        if (label_name[i - 1] != ':') {
            /* Pseudo instruction call resolves into two instructions */
            if (strcmp(label_name, "call") == 0) {
                ass->inst_byte_offset += 8;
            } else {
                ass->inst_byte_offset += 4;
            }
            skip_until_next_line(ass);
            continue;
        }
        /* Execution enters here on encountering a label */
        label_name[i - 1] = 0; // Replace ':' with null terminator
        void *v = hashmap_sget(&ass->labels, label_name);
        if (v != NULL) {
            printf("Found duplicate labels '%s'\n", label_name);
            ass->had_error = true;
            return;
        }
        hashmap_sput(&ass->labels, label_name, 
                     (u32 *)(size_t)(ass->inst_byte_offset + 1), sizeof(u32 *), false);
        if (c != '\n') {
            skip_until_next_line(ass);
        }
    }

    /* Reset pos before the next pass */
    ass->pos = 0;
    ass->inst_byte_offset = 0;
}

static void emit_instruction(Assembler *ass, u32 inst)
{
    if (ass->inst_count > ass->insts_allocated) {
        ass->insts_allocated *= 2;
        ass->insts = realloc(ass->insts, sizeof(u32) * ass->insts_allocated);
    }
    ass->insts[ass->inst_count] = inst;
    ass->inst_count++;
    ass->inst_byte_offset += 4;
}

static void assemble_next_inst(Assembler *ass)
{
    skip_spaces(ass);
    u8 c = next_u8(ass);

    /* If comment, skip entire line */
    if (c == '#') {
        skip_until_next_line(ass);
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
            skip_until_next_line(ass);
        }
        return;
    }

    /* Only progress here if mnemonic is not a label */
    s32 mnemonic_id = match_mnemonic(mnemonic);
    if (mnemonic_id == -1) {
        printf("Unknown mnemonic '%s'\n", mnemonic);
        ass->had_error = true;
        return;
    }

    /* Parse operands */
    u32 n_ops = mnemonic_operand_count(mnemonic_id);
    Operand ops[n_ops];
    for (u32 i = 0; i < n_ops; i++) {
        ops[i] = next_operand(ass);
        if (ops[i].kind == OP_LABEL) {
            void *v = hashmap_sget(&ass->labels, ops[i].label);
            if (v == NULL) {
                printf("Could not resolve label '%s'\n", ops[i].label);
                ass->had_error = true;
                return;
            }
            /* Immediate becomes the PC-relative offset from the label */
            ops[i].imm = (((u32)(size_t)v) - 1) - ass->inst_byte_offset;
        }
    }

    skip_until_next_line(ass);

#if 0
    /* Debug instruction */
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

    /* Encode instruction */
    switch (mnemonic_id) {
    /* Normal instructions */
    default: {
        u32 imm = ops[2].imm;
        if (n_ops == 2) {
            imm = ops[1].imm;
        }
        emit_instruction(ass, encode_inst(ass, mnemonic_id, ops[0].reg_id, ops[1].reg_id, ops[2].reg_id, imm));
    }; break;
    /* Pseudo instructions */
    case M_PSEUDO_HALT:
        emit_instruction(ass, 0);
        break;
    case M_PSEUDO_MV:
        emit_instruction(ass, encode_inst(ass, M_ADDI, ops[0].reg_id, ops[1].reg_id, -1, 0));
        break;
    case M_PSEUDO_CALL: {
        /* 
         * Transalte pseudo operation call into 
         * auipc ra, offser[31:12]
         * jalr ra, ra, offset[11:0]
         */
        if (ops[0].kind != OP_LABEL) {
            fprintf(stderr, "Pseudo op 'call' requires one operand: a label\n");
        }
        u32 target_offset = ops[0].imm;
        u32 offset_high = (target_offset + 0x800) >> 12;
        u32 offset_low  = target_offset & 0xFFF;

        emit_instruction(ass, encode_inst(ass, M_AUIPC, REG_RA, -1, -1, offset_high));
        emit_instruction(ass, encode_inst(ass, M_JALR, REG_RA, REG_RA, -1, offset_low));
    }; break;
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
        fprintf(stderr, "Could not read file '%s'", filename);
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

    /* Initialize assembler */
    Assembler ass = {0};
    ass.data = input_data.data;
    ass.data_len = input_data.size;
    hashmap_init(&ass.labels);
    ass.insts_allocated = 128;
    ass.insts = malloc(sizeof(u32) * ass.insts_allocated);

    /* First pass. Label resolution. */
    resolve_labels(&ass);

    /* Second pass. Code gen. */
    while (!ass.had_error && ass.pos < ass.data_len) {
        assemble_next_inst(&ass);
    }

    /* Cleanup */
    free(input_data.data);
    hashmap_free(&ass.labels);

    if (ass.had_error) {
        return (BinaryBuf){ .data = NULL };
    }
    return (BinaryBuf){ .size = ass.inst_count, .data = (u8 *)ass.insts };
}
