        .org $F000

        LDX #$00
loop:
        TXA
        STA $8000,X
        INX
        BNE loop
