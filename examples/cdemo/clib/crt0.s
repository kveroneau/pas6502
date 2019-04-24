.export _init, _exit
.import _main

.export __STARTUP__ : absolute = 1
.import __HEAP_START__, __HEAP_SIZE__

.import zerobss, initlib, donelib

.include "zeropage.inc"

.segment "STARTUP"

_init:  LDX #$FF
        TXS
        CLD

        LDA #<(__HEAP_START__ + __HEAP_SIZE__)
        STA sp
        LDA #>(__HEAP_START__ + __HEAP_SIZE__)
        STA sp+1

        ;JSR zerobss
        JSR initlib
        JSR _main

_exit:  JSR donelib
        JMP $FFF0 ; JMP directly to the Memory Mapped I/O termination function
