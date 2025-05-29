# x5 = 1, if x5 == 1 then x5 == 9, x5 += 1
main:
    addi x5, zero, 2
    addi x6, zero, 1
    bne x5, x6, .L2
.L1:
    addi x5, zero, 9
.L2:
    addi x5, x5, 1
    halt
