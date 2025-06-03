# void bubble_sort(int* arr, int n)
# {
#     for (int i = 0; i < n - 1; i++) {
#         for (int j = 0; j < n - 1 - i; j++) {
#             if (arr[j] > arr[j + 1]) {
#                 int tmp = arr[j];
#                 arr[j] = arr[j + 1];
#                 arr[j + 1] = tmp;
#             }
#         }
#     }
# }
# 
# 
# int main() {
#     int data[] = {5, 1, 4, 2, 8};
#     int size = sizeof(data) / sizeof(data[0]);
#     bubble_sort(data, size);
#     int a = data[1];
# 
#     return 0;
# }
main:
        addi    sp, sp, -64
        sd      ra, 56(sp)
        sd      s0, 48(sp)
        addi    s0, sp, 64
        li      a0, 0
        sd      a0, -56(s0)
        sw      a0, -20(s0)
        li      a0, 8
        sw      a0, -24(s0)
        li      a0, 1
        slli    a1, a0, 33
        addi    a1, a1, 4
        sd      a1, -32(s0)
        slli    a0, a0, 32
        addi    a0, a0, 5
        sd      a0, -40(s0)
        li      a0, 5
        sw      a0, -44(s0)
        lw      a1, -44(s0)
        addi    a0, s0, -40
        call    bubble_sort
        addi    x1, zero, 0
        lw      a0, -40(s0) # 0th element
        lw      a1, -36(s0) # 1st element
        lw      a2, -32(s0) # 2nd element
        lw      a3, -28(s0) # 3rd element
        lw      a4, -24(s0) # 3rd element
        addi    s0, zero, 0
        halt
bubble_sort:
        addi    sp, sp, -48
        sd      ra, 40(sp)
        sd      s0, 32(sp)
        addi    s0, sp, 48
        sd      a0, -24(s0)
        sw      a1, -28(s0)
        li      a0, 0
        sw      a0, -32(s0)
        j       .LBB0_1
.LBB0_1:
        lw      a0, -32(s0)
        lw      a1, -28(s0)
        addi    a1, a1, -1
        bge     a0, a1, .LBB0_10
        j       .LBB0_2
.LBB0_2:
        li      a0, 0
        sw      a0, -36(s0)
        j       .LBB0_3
.LBB0_3:
        lw      a0, -36(s0)
        lw      a2, -28(s0)
        lw      a1, -32(s0)
        # not     a1, a1
        xori    a1, a1, -1
        add     a1, a1, a2
        bge     a0, a1, .LBB0_8
        j       .LBB0_4
.LBB0_4:
        ld      a0, -24(s0)
        lw      a1, -36(s0)
        slli    a1, a1, 2
        add     a0, a0, a1
        lw      a1, 0(a0)
        lw      a0, 4(a0)
        bge     a0, a1, .LBB0_6
        j       .LBB0_5
.LBB0_5:
        ld      a0, -24(s0)
        lw      a1, -36(s0)
        slli    a1, a1, 2
        add     a0, a0, a1
        lw      a0, 0(a0)
        sw      a0, -40(s0)
        ld      a0, -24(s0)
        lw      a1, -36(s0)
        slli    a1, a1, 2
        add     a1, a1, a0
        lw      a0, 4(a1)
        sw      a0, 0(a1)
        lw      a0, -40(s0)
        ld      a2, -24(s0)
        lw      a1, -36(s0)
        slli    a1, a1, 2
        add     a1, a1, a2
        sw      a0, 4(a1)
        j       .LBB0_6
.LBB0_6:
        j       .LBB0_7
.LBB0_7:
        lw      a0, -36(s0)
        addi    a0, a0, 1
        sw      a0, -36(s0)
        j       .LBB0_3
.LBB0_8:
        j       .LBB0_9
.LBB0_9:
        lw      a0, -32(s0)
        addi    a0, a0, 1
        sw      a0, -32(s0)
        j       .LBB0_1
.LBB0_10:
        ld      ra, 40(sp)
        ld      s0, 32(sp)
        addi    sp, sp, 48
        ret
