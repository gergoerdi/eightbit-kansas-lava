VIDEO    = $8000

.proc  hex_debug
        pha
        ldx #$01
        jsr print_nybble

        lsr A
        lsr A
        lsr A
        lsr A
        ldx #$00
        jsr print_nybble

        pla
        rts
.endproc

.proc print_nybble
        pha

        and #$0F
        cmp #$0A
        bpl large
small:  clc
        adc #$30
        sta VIDEO,X

        pla
        rts
large:
        sec
        sbc #$09
        sta VIDEO,X

        pla
        rts
.endproc
