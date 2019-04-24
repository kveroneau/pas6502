.rodata

welcome: .byte "Stack test program.",$a,$0

msg: .byte "Stack broken!",$a,$0

; This will test the 6502 stack...

.code

lda #<welcome
ldx #>welcome
jsr $ffd0

lda #65
pha
lda #80
pha
lda #20
pha
lda #0
pla
cmp #20
bne error
pla
cmp #80
bne error
pla
cmp #65
bne error
jmp $fff0

error: lda #<msg
       ldx #>msg
       jsr $ffd0
       jmp $fff0
