main:
        addi    sp, sp, -16
        sd      ra, 8(sp)
        sd      s0, 0(sp)
        addi    s0, sp, 16
        li      a0, 16 # fib of a0
        call    fib
        # clear other regs, only interested in result of fib(n)
        addi    s0, zero, 0
        addi    ra, zero, 0
        addi    a1, zero, 0
        halt
fib:
        addi    sp, sp, -32
        sd      ra, 24(sp)
        sd      s0, 16(sp)
        addi    s0, sp, 32
        sw      a0, -24(s0)
        lw      a1, -24(s0)
        li      a0, 1
        blt     a0, a1, .LBB0_2
        j       .LBB0_1
.LBB0_1:
        lw      a0, -24(s0)
        sw      a0, -20(s0)
        j       .LBB0_3
.LBB0_2:
        lw      a0, -24(s0)
        addi   a0, a0, -1
        call    fib
        sd      a0, -32(s0)
        lw      a0, -24(s0)
        addi   a0, a0, -2
        call    fib
        mv      a1, a0
        ld      a0, -32(s0)
        add    a0, a0, a1
        sw      a0, -20(s0)
        j       .LBB0_3
.LBB0_3:
        lw      a0, -20(s0)
        ld      ra, 24(sp)
        ld      s0, 16(sp)
        addi    sp, sp, 32
        ret
