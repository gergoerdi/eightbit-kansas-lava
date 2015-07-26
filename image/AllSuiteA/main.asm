        .org $C000

reset:
        LDY #$07
        STY $E813

.proc irq
        cmp $E812
;;         lda #$20
;;         ldx #$00
;; :       sta VIDEO,X
;;         inx
;;         bne :-
.endproc

        .include "AllSuiteA.asm"

        lda $0210
        jsr hex_debug

        cli
        jmp *

.include "pet-hexdebug.asm"

        .res $FFFC-*

        .org $FFFC
resetv: .addr reset
irqv:   .addr irq
