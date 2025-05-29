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
#define NICC_IMPLEMENTATION
#include "nicc.h"
#include "types.h"
#include "riscvm.h"


u8 *regs[] = {
    "x0",  "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",
    "x8",  "x9",  "x10", "x11", "x12", "x13", "x14", "x15",
    "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
    "x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31"
};

u8 *reg_names[] = {
    "zero", "ra",  "sp",  "gp",  "tp",  "t0",  "t1",  "t2",
    "s0",   "s1",  "a0",  "a1",  "a2",  "a3",  "a4",  "a5",
    "a6",   "a7",  "s2",  "s3",  "s4",  "s5",  "s6",  "s7",
    "s8",   "s9",  "s10", "s11", "t3",  "t4",  "t5",  "t6"
};

const char *mnemonics[] = {
    "lui", "auipc", "jal", "jalr",
    "beq", "bne", "bge", "bltu", "bgeu",
    "lb", "lh", "lw", "lbu", "lhu", "lwu", "ld",
    "sb", "sh", "sw", "sd",
    "addi", "slti", "sltiu", "xori", "ori", "andi", "slli", "srli", "srai",
    "add", "sub", "sll", "slt", "sltu", "xor", "srl", "sra", "or", "and",
    "addiw", "slliw", "srliw", "sraiw", "addw", "subw", "sllw", "srlw", "sraw",

    "halt",
};

typedef enum {
    MNEMONIC_LUI, MNEMONIC_AUIPC, MNEMONIC_JAL, MNEMONIC_JALR,

    MNEMONIC_BEQ, MNEMONIC_BNE, MNEMONIC_BGE, MNEMONIC_BLTU, MNEMONIC_BGEU,

    MNEMONIC_LB, MNEMONIC_LH, MNEMONIC_LW, MNEMONIC_LBU, MNEMONIC_LHU, MNEMONIC_LWU, MNEMONIC_LD,

    MNEMONIC_SB, MNEMONIC_SH, MNEMONIC_SW, MNEMONIC_SD,

    MNEMONIC_ADDI, MNEMONIC_SLTI, MNEMONIC_SLTIU, MNEMONIC_XORI, MNEMONIC_ORI, MNEMONIC_ANDI,
    MNEMONIC_SLLI, MNEMONIC_SRLI, MNEMONIC_SRAI,

    MNEMONIC_ADD, MNEMONIC_SUB, MNEMONIC_SLL, MNEMONIC_SLT, MNEMONIC_SLTU, MNEMONIC_XOR, 
    MNEMONIC_SRL, MNEMONIC_SRA, MNEMONIC_OR, MNEMONIC_AND,

    MNEMONIC_ADDIW, MNEMONIC_SLLIW, MNEMONIC_SRLIW, MNEMONIC_SRAIW,

    MNEMONIC_ADDW, MNEMONIC_SUBW, MNEMONIC_SLLW, MNEMONIC_SRLW, MNEMONIC_SRAW,

    MNEMONIC_HALT,

    MNEMONIC_COUNT
} Mnemonic;

typedef struct {
    // Lexing stuff
    u8 *data;
    u32 data_len;
    u32 pos;
    bool had_error;

    s32 inst_count;
    s32 instruction_stream_byte_offset;
    /* 
     * key: str8, 
     * value: u32: byte offset in instruction stream + 1. +1 so 0 is not treated as NULL
     */
    HashMap labels;
} Assembler;

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
    if (ass->pos < ass->data_len) ass->pos++; // Skips the newline itself
}


static void read_token(Assembler *ass, u8 token[16])
{
    skip_spaces(ass);
    u32 i = 0;
    while (ass->pos < ass->data_len) {
        u8 c = ass->data[ass->pos];
        if (c == ',' || c == '\n' || c == ' ' || c == '\t') break;
        token[i++] = c;
        ass->pos++;
        if (i == 15) break;
    }
    token[i] = 0;
    skip_spaces(ass);
    if (ass->data[ass->pos] == ',') ass->pos++;
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

static u32 encode_btype(u8 rs1, u8 rs2, u8 imm, u8 opcode, u8 funct3)
{
    /* Immediate is 13-bit signed, but bit 0 is always 0 (2-byte aligned) */
    
    u32 imm_12   = (imm >> 12) & 0x1;    // bit 12
    u32 imm_11   = (imm >> 11) & 0x1;    // bit 11  
    u32 imm_10_5 = (imm >> 5) & 0x3F;    // bits 10:5
    u32 imm_4_1  = (imm >> 1) & 0xF;     // bits 4:1
    
    u32 instruction = 0;
    instruction |= (opcode & 0x7F);           // bits 6:0
    instruction |= (imm_11 & 0x1) << 7;       // bit 7
    instruction |= (imm_4_1 & 0xF) << 8;      // bits 11:8
    instruction |= (funct3 & 0x7) << 12;      // bits 14:12
    instruction |= (rs1 & 0x1F) << 15;        // bits 19:15
    instruction |= (rs2 & 0x1F) << 20;        // bits 24:20
    instruction |= (imm_10_5 & 0x3F) << 25;   // bits 30:25
    instruction |= (imm_12 & 0x1) << 31;      // bit 31
    
    return instruction;
}

static u32 encode_inst(Assembler *ass, u32 mnemonic_id, s32 rd, s32 rs1, s32 rs2, u32 imm)
{
    switch (mnemonic_id) {
    default:
        printf("Encoding not implemented for mnemonic id %d\n", mnemonic_id);
        ass->had_error = true;
        return 0;
    case MNEMONIC_HALT:
        return 0;
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
    case MNEMONIC_SLLI:
        return encode_rtype(rd, rs1, imm & 0x1F, OPCODE_OP_IMM, FUNCT7_SLL, FUNCT3_SLL);
    case MNEMONIC_SRLI:
        return encode_rtype(rd, rs1, imm & 0x1F, OPCODE_OP_IMM, FUNCT7_SRL, FUNCT3_SRL_SRA);
    case MNEMONIC_SRAI:
        return encode_rtype(rd, rs1, imm & 0x1F, OPCODE_OP_IMM, FUNCT7_SRA, FUNCT3_SRL_SRA);

    case MNEMONIC_BNE:
        return encode_btype(rs1, rs2, imm, OPCODE_BRANCH, FUNCT3_BNE);
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
        if (i == 1) {
            printf("Empty label name.\n");
            ass->had_error = true;
            return;
        }
        /* Not a label. So an instruction. */
        if (label_name[i - 1] != ':') {
            ass->instruction_stream_byte_offset += 4;
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


static u32 assemble_next_inst(Assembler *ass)
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
            return 0;
        }
    } while ((c = next_u8(ass)) != ' ' && c != '\n');

    /* This is a label, not an instruction mnemonic */
    if (mnemonic[i - 1] == ':') {
        mnemonic[i - 1] = 0; // Repalce ':' with null terminator
        void *v = hashmap_sget(&ass->labels, mnemonic);
        if (v == NULL) {
            printf("Unknown label '%s'\n", mnemonic);
            ass->had_error = true;
            return 0;
        }
        /* Happy path */
        if (c != '\n') {
            skip_until_newline(ass);
        }
        return assemble_next_inst(ass);
    }
    /* Only progress here if mnemonic is not a label */
    s32 mnemonic_id = match_mnemonic(mnemonic);
    if (mnemonic_id == -1) {
        printf("Unknown mnemonic\n");
        ass->had_error = true;
        return 0;
    }

    /* Parse args (regs, immediate, label) */
    // NOTE: assume either regs or immediate and always 3 args
    u8 tok0[16] = {0};
    u8 tok1[16] = {0};
    u8 tok2[16] = {0};
    read_token(ass, tok0);
    read_token(ass, tok1);
    read_token(ass, tok2);

    s32 rd = match_reg(tok0);
    s32 rs1 = match_reg(tok1);
    s32 rs2 = match_reg(tok2);
    u32 imm = 0;
    /* rs2 is label or immediate */
    if (rs2 == -1) {
        void *v = hashmap_sget(&ass->labels, tok2);
        if (v != NULL) {
            /* PC-relative */
            imm = (((u32)(size_t)v) - 1) - ass->instruction_stream_byte_offset;
        } else {
            imm = parse_u32(tok2);
        }
    }

#if 0
    printf("mnemonic: %s\n", mnemonic);
    printf("rd: %s (%d)\n", tok0, rd);
    printf("rs1: %s (%d)\n", tok1, rs1);
    if (rs2 != -1) {
        printf("rs2: %s (%d)\n", tok2, rs2);
    } else {
        printf("imm: %s (%d)\n", tok2, imm);
    }
    printf("\n\n");
#endif

    skip_spaces(ass);
    skip_until_newline(ass);

    u32 encoded_inst = encode_inst(ass, mnemonic_id, rd, rs1, rs2, imm);
    return encoded_inst;
}

s32 assemble_file(u8 *input_file, u32 instructions[1024])
{
    /* Read input file in its entirety */
    struct stat st;
    if (stat(input_file, &st) != 0) {
        printf("Could not find file '%s'", input_file);
        return -1;
    }
    size_t input_size = st.st_size;
    char *input = malloc(sizeof(char) * (input_size + 1));
    input[input_size] = 0;
    FILE *fp = fopen(input_file, "r");
    if (fp == NULL) {
        printf("Could not open file '%s'", input_file);
        free(input);
        fclose(fp);
        return -1;
    }
    if (fread(input, sizeof(char), st.st_size, fp) != input_size) {
        printf("Could not read file '%s'", input_file);
        free(input);
        fclose(fp);
        return -1;
    }
    fclose(fp);

    Assembler ass = {0};
    ass.data = input;
    ass.data_len = input_size;
    hashmap_init(&ass.labels);

    /* First pass. Label resolution. */
    resolve_labels(&ass);

    /* Second pass. Code gen. */
    while (!ass.had_error && ass.pos < ass.data_len) {
        u32 inst = assemble_next_inst(&ass);
        instructions[ass.inst_count] = inst;
        ass.inst_count++;
        ass.instruction_stream_byte_offset += 4;
    }

    /* Cleanup */
    free(input);
    hashmap_free(&ass.labels);

    if (ass.had_error) {
        return -1;
    }
    return ass.inst_count;
}

int main(void)
{
    u32 inst[1024];
    s32 inst_count = assemble_file("tests/simp.s", inst);

    RiscVM vm = {0};
    execute_until_halt(&vm, inst);
    printf("x2 = %zu\n", vm.regs[2]);
    printf("x3 = %zu\n", vm.regs[3]);
    printf("x4 = %zu\n", vm.regs[4]);
    printf("x5 = %zu\n", vm.regs[5]);

    // TODOS:
    // - Resolve labels (pre-pass)
    // - Parse and use labels
    // - Register indirect addressing mode `lw x1, 8(x2)`
}
