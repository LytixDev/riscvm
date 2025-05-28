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

#define MNEMONIC_ADD  0  // R-type
#define MNEMONIC_ADDI 1  // I-type

#define MNEMONIC_COUNT 2
u8 *mnemonics[] = {
    "add", "addi"
};

typedef struct {
    // Lexing stuff
    u8 *data;
    u32 data_len;
    u32 pos;
    bool had_error;

    s32 inst_count;
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
    u8 mnemonic[8] = {0};
    u32 i = 0;
    do {
        mnemonic[i++] = c;
        if (i == 8) {
            printf("Mnemonic too large.\n");
            ass->had_error = true;
            return 0;
        }
    } while ((c = next_u8(ass)) != ' ');

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
    if (rs2 == -1) {
        imm = parse_u32(tok2);
    }

    printf("mnemonic: %s\n", mnemonic);
    printf("rd: %s (%d)\n", tok0, rd);
    printf("rs1: %s (%d)\n", tok1, rs1);
    if (rs2 != -1)
        printf("rs2: %s (%d)\n", tok2, rs2);
    else
        printf("imm: %s (%d)\n", tok2, imm);
    printf("\n\n");

    u32 encoded_inst;
    switch (mnemonic_id) {
    default:
        printf("Encoding not implemented for mnemonic id %d\n", mnemonic_id);
        ass->had_error = true;
        return 0;
    case MNEMONIC_ADD:
        encoded_inst = encode_rtype(rd, rs1, rs2, 0x33, 0x00, 0x0);
        break;

    case MNEMONIC_ADDI:
        encoded_inst = encode_itype_op(rd, rs1, imm, 0x13, 0x0);
        break;
    }

    skip_spaces(ass);
    skip_until_newline(ass);
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

    while (!ass.had_error && ass.pos < ass.data_len) {
        u32 inst = assemble_next_inst(&ass);
        instructions[ass.inst_count] = inst;
        ass.inst_count++;
    }

    free(input);
    if (ass.had_error) {
        return -1;
    }
    return ass.inst_count;
}

int main(void)
{
    u32 inst[1024];
    s32 inst_count = assemble_file("tests/arith.s", inst);
    printf("%d\n", inst_count);

    RiscVM vm = {0};
    for (s32 i = 0; i < inst_count; i++) {
        execute_instruction(&vm, inst[i]);
    }

    printf("x3  = %zu\n", vm.regs[3]);
}
