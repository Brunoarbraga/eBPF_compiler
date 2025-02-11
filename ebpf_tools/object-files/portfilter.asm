ld      imm    2
ld      [r1+4]
ld      [r1]
mov     r4, r1
add     r4, 14
jeq     r4, r2, 26
ldxb    [r1+12]
ldxb    [r1+13]
lsh     r5, 8
or      r5, r3
jeq     r5, 8, 21
mov     r3, r1
add     r3, 34
jeq     r3, r2, 18
ldxb    [r4]
and     r4, 15
jeq     r4, 5, 15
ldxh    [r1+20]
and 
