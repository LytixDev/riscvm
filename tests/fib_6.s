main:
 addi sp,sp,-16
 sw ra,12(sp)
 sw s0,8(sp)
 addi s0,sp,16
 addi a0,x0,6
 call f
 halt
 mv a5,a0
 mv a0,a5
 lw ra,12(sp)
 lw s0,8(sp)
 addi sp,sp,16
 jalr ra, ra, 0
f:
 addi sp,sp,-32
 sw ra,28(sp)
 sw s0,24(sp)
 sw s1,20(sp)
 addi s0,sp,32
 sw a0,-20(s0)
 lw a5,-20(s0)
 bne a5,x0,.L2
 addi a5,x0,0
 jal x0,.L3
.L2:
 lw a4,-20(s0)
 addi a5,x0,1
 bne a4,a5,.L4
 addi a5,x0,1
 jal x0,.L3
.L4:
 lw a5,-20(s0)
 addi a5,a5,-1
 mv a0,a5
 call f
 mv s1,a0
 lw a5,-20(s0)
 addi a5,a5,-2
 mv a0,a5
 call f
 mv a5,a0
 add a5,s1,a5
.L3:
 mv a0,a5
 lw ra,28(sp)
 lw s0,24(sp)
 lw s1,20(sp)
 addi sp,sp,32
 jalr ra, ra, 0
