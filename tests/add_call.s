# int add(int a, int b)
# {
#     return a + b;
# }
# 
# int main(void)
# {
#     int a = 5;
#     int b = 10;
#     int c = add(5, 10);
# }

main:
 addi sp,sp,-32
 sw ra,28(sp)
 sw s0,24(sp)
 addi s0,sp,32
 addi a1,zero,10
 addi a0,zero,5
 call add
 sw a0,-28(s0)
 addi a1, zero, 0
 addi a4, zero, 0
 addi a5, zero, 0
 addi x1, zero, 0
 addi s0, zero, 0
 halt
 halt
add:
 addi sp,sp,-32
 sw ra,28(sp)
 sw s0,24(sp)
 addi s0,sp,32
 sw a0,-20(s0)
 sw a1,-24(s0)
 lw a4,-20(s0)
 lw a5,-24(s0)
 add a5,a4,a5
 mv a0,a5
 lw ra,28(sp)
 lw s0,24(sp)
 addi sp,sp,32
 jalr x0, ra, 0
