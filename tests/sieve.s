main:
        addi    sp, sp, -32
        sd      ra, 24(sp)
        sd      s0, 16(sp)
        addi    s0, sp, 32
        li      a0, 0
        sd      a0, -32(s0)
        sw      a0, -20(s0)
        li      a0, 50
        sw      a0, -24(s0)
        lw      a0, -24(s0)
        call    sieve
        halt
sieve:
        addi    sp, sp, -64
        sd      ra, 56(sp)
        sd      s0, 48(sp)
        addi    s0, sp, 64
        sw      a0, -20(s0)
        lw      a1, -20(s0)
        li      a0, 1
        blt     a0, a1, .LBB0_2
        j       .LBB0_1
.LBB0_1:
        j       .LBB0_17
.LBB0_2:
        lw      a0, -20(s0)
        addiw   a0, a0, 1
        slli    a0, a0, 32
        srli    a0, a0, 32
        mv      a1, sp
        sd      a1, -32(s0)
        addi    a1, a0, 15
        andi    a2, a1, -16
        mv      a1, sp
        sub     a1, a1, a2
        sd      a1, -64(s0)
        mv      sp, a1
        sd      a0, -40(s0)
        li      a0, 0
        sw      a0, -44(s0)
        j       .LBB0_3
.LBB0_3:
        lw      a1, -44(s0)
        lw      a0, -20(s0)
        blt     a0, a1, .LBB0_6
        j       .LBB0_4
.LBB0_4:
        ld      a0, -64(s0)
        lw      a1, -44(s0)
        add     a1, a1, a0
        li      a0, 1
        sb      a0, 0(a1)
        j       .LBB0_5
.LBB0_5:
        lw      a0, -44(s0)
        addiw   a0, a0, 1
        sw      a0, -44(s0)
        j       .LBB0_3
.LBB0_6:
        ld      a1, -64(s0)
        li      a0, 0
        sb      a0, 1(a1)
        sb      a0, 0(a1)
        li      a0, 2
        sw      a0, -48(s0)
        j       .LBB0_7
.LBB0_7:
        lw      a0, -48(s0)
        mulw    a1, a0, a0
        lw      a0, -20(s0)
        blt     a0, a1, .LBB0_16
        j       .LBB0_8
.LBB0_8:
        ld      a0, -64(s0)
        lw      a1, -48(s0)
        add     a0, a0, a1
        lbu     a0, 0(a0)
        beqz    a0, .LBB0_14
        j       .LBB0_9
.LBB0_9:
        lw      a0, -48(s0)
        mulw    a0, a0, a0
        sw      a0, -52(s0)
        j       .LBB0_10
.LBB0_10:
        lw      a1, -52(s0)
        lw      a0, -20(s0)
        blt     a0, a1, .LBB0_13
        j       .LBB0_11
.LBB0_11:
        ld      a0, -64(s0)
        lw      a1, -52(s0)
        add     a1, a1, a0
        li      a0, 1
        sb      a0, 0(a1)
        j       .LBB0_12
.LBB0_12:
        lw      a1, -48(s0)
        lw      a0, -52(s0)
        addw    a0, a0, a1
        sw      a0, -52(s0)
        j       .LBB0_10
.LBB0_13:
        j       .LBB0_14
.LBB0_14:
        j       .LBB0_15
.LBB0_15:
        lw      a0, -48(s0)
        addiw   a0, a0, 1
        sw      a0, -48(s0)
        j       .LBB0_7
.LBB0_16:
        ld      a0, -32(s0)
        mv      sp, a0
        j       .LBB0_17
.LBB0_17:
        addi    sp, s0, -64
        ld      ra, 56(sp)
        ld      s0, 48(sp)
        addi    sp, sp, 64
        ret
