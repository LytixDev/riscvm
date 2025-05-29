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
#include "types.h"
#include "assembler.h"
#include "riscvm.h"


bool read_file_to_buffer(u8 *filename, u8 *buffer)
{
    FILE *f = fopen(filename, "r");
    if (f == NULL) {
        return false;
    }
    size_t read = fread(buffer, 1, 4096 - 1, f);
    buffer[read] = 0;
    fclose(f);
    return true;
}

bool run_test(u8 *source_file, u8 *expected_reg_state_file)
{
    u32 inst[1024];
    assemble_file(source_file, inst);

    RiscVM vm = {0};
    execute_until_halt(&vm, inst);

    u8 reg_state[4096];
    dump_regs_to_buffer(&vm, reg_state, 4096, true);

    u8 expected_reg_state[4096];
    if (!read_file_to_buffer(expected_reg_state_file, expected_reg_state)) {
        fprintf(stderr, "Failed to open expected reg file: %s\n", expected_reg_state_file);
        return false;
    }

    if (strcmp(reg_state, expected_reg_state) == 0) {
        printf("PASS :: %s\n", source_file);
        return true;
    } else {
        printf("FAIL :: %s\nExpected:\n%s\nActual:\n%s\n", source_file, expected_reg_state, reg_state);
        return false;
    }
}

void run_all_tests(void)
{
    run_test("tests/simp.s", "tests/simp.regs");
    run_test("tests/artih.s", "tests/arith.regs");
}

int main(void)
{
#if 1
    u32 inst[1024];
    s32 inst_count = assemble_file("tests/arith.s", inst);

    RiscVM vm = {0};
    execute_until_halt(&vm, inst);
    dump_regs(&vm, true);
#endif

    // run_all_tests();

}
