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
#ifndef RISCVM_ASSEMBLER_H
#define RISCVM_ASSEMBLER_H

#include "base.h"

typedef struct {
    s64 size; // Number of bytes
    u8 *data; // Heap allocated. NULLABLE (no data).
} BinaryBuf;


BinaryBuf read_file(u8 *filename);
BinaryBuf assemble_file(u8 *input_file);


#endif /* RISCVM_ASSEMBLER_H */
