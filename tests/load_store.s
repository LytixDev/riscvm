main:
 addi t1, zero, 1
 addi t2, zero, 2
 addi t3, zero, 3
 addi t4, zero, 4
 sw t1, 0(x0)
 sw t2, 4(x0)
 sw t3, 8(x0)
 sw t4, 12(x0)
 lw t1 12(x0)
 lw t2 8(x0)
 lw t3 4(x0)
 lw t4 0(x0)
 halt
