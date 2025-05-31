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
#include <string.h>
#include <getopt.h>
#include <sys/stat.h>
#define BASE_IMPLEMENTATION
#include "base.h"

#include "assembler.h"
#include "riscvm.h"

bool run_test(u8 *source_file, u8 *expected_reg_state_file)
{
    BinaryBuf instructions = assemble_file(source_file);
    if (instructions.data == NULL) {
        fprintf(stderr, "FAIL :: could not assemble '%s'\n", source_file);
        return false;
    }

    RiscVM vm = {0};
    execute_until_halt(&vm, (u32 *)instructions.data);
    u8 reg_state[4096];
    dump_regs_to_buffer(&vm, reg_state, 4096, true);

    BinaryBuf expected_reg_state = read_file(expected_reg_state_file);
    if (expected_reg_state.data == NULL) {
        fprintf(stderr, "FAIL :: during reading of expected_reg_state_file '%s'\n", expected_reg_state_file);
        free(instructions.data);
        return false;
    }

    bool fail = false;
    if (strcmp(reg_state, expected_reg_state.data) == 0) {
        printf("PASS :: %s\n", source_file);
    } else {
        printf("FAIL :: %s\nExpected:\n%s\nActual:\n%s\n", source_file, expected_reg_state.data, reg_state);
        fail = true;
    }

    free(instructions.data);
    free(expected_reg_state.data);
    return fail;
}

void run_all_tests(void)
{
    run_test("tests/simp.s", "tests/simp.regs");
    run_test("tests/arith.s", "tests/arith.regs");
    run_test("tests/addi.s", "tests/addi.regs");
    run_test("tests/arith_imm.s", "tests/arith_imm.regs");
}

void assemble_and_save(u8 *input_file)
{
    BinaryBuf instructions = assemble_file(input_file);
    if (instructions.data == NULL) {
        return;
    }
    FILE *out_file = fopen("out.bin", "wb");
    if (!out_file) {
        fprintf(stderr, "Could not open file 'out.bin'\n");
        return;
    }

    size_t written = fwrite((u32 *)instructions.data, sizeof(u32), instructions.size, out_file);
    if (written != instructions.size) {
        fprintf(stderr, "Failed to write all instructions.\n");
        fclose(out_file);
        return;
    }

    fclose(out_file);
}

void run_binary(u8 *input_file)
{
    BinaryBuf buf = read_file(input_file);
    if (buf.data == NULL) {
        return;
    }

    RiscVM vm = {0};
    execute_until_halt(&vm, (u32 *)buf.data);
    dump_regs(&vm, true);
    free(buf.data);
}

void run_asm(u8 *input_file)
{
    RiscVM vm = {0};
    BinaryBuf instructions = assemble_file(input_file);
    if (instructions.data == NULL) {
        fprintf(stderr, "Error occured during assembly. Quitting ...\n");
        return;
    }
    execute_until_halt(&vm, (u32 *)instructions.data);
    printf("--- Finished in %d steps ---\n", vm.steps);
    dump_regs(&vm, true);
    free(instructions.data);
}

int main(int argc, char *argv[])
{
    // TODO: debug levels

    int opt;
    char *mode = NULL;
    char *input_file = NULL;
    while ((opt = getopt(argc, argv, "m:f:")) != -1) {
        switch (opt) {
        case 'm': // mode: test, assemble, runasm, runbin
            mode = optarg;
            break;
        case 'f': // input file
            input_file = optarg;
            break;
        default:
            fprintf(stderr, "Usage: %s -m [test|assemble|runasm|runbin] [-f filename]\n", argv[0]);
            return 1;
        }
    }

    if (mode == NULL) {
        fprintf(stderr, "Usage: %s -m [test|assemble|runasm|runbin] [-f filename]\n", argv[0]);
        return 1;
    }

    if (strcmp(mode, "test") == 0) {
        run_all_tests();
        return 0;
    } 

    /* All the other modes need a valid input file */
    if (input_file == NULL) {
        fprintf(stderr, "No input file given.\n");
        fprintf(stderr, "Usage: %s -m [test|assemble|runasm|runbin] [-f filename]\n", argv[0]);
        return 1;
    }
    if (strcmp(mode, "assemble") == 0) {
        assemble_and_save((u8 *)input_file);
    } else if (strcmp(mode, "runasm") == 0) {
        run_asm((u8 *)input_file);
    } else if (strcmp(mode, "runbin") == 0) {
        run_binary((u8 *)input_file);
    }

    return 0;
}
