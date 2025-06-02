main:
        addi    sp, sp, -32
        sd      ra, 24(sp)
        sd      s0, 16(sp)
        addi    s0, sp, 32
        li      a0, 5
        sw      a0, -20(s0)
        li      a1, 10
        sw      a1, -24(s0)
        call    add
        sw      a0, -28(s0)
        li      a0, 0
        ld      ra, 24(sp)
        ld      s0, 16(sp)
        addi    sp, sp, 32
        ret
add:
        addi    sp, sp, -32
        sd      ra, 24(sp)
        sd      s0, 16(sp)
        addi    s0, sp, 32
        sw      a0, -20(s0)
        sw      a1, -24(s0)
        lw      a0, -20(s0)
        lw      a1, -24(s0)
        add     a0, a0, a1
        ld      ra, 24(sp)
        ld      s0, 16(sp)
        addi    sp, sp, 32
        ret
