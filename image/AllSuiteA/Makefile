#XPET = /home/cactus/prog/lava/vice/vice-2.4.dfsg/src/xpet
XPET = xpet

MAIN=main
MODS=AllSuiteA pet-hexdebug

.PHONY: all run

run: all
	$(XPET) -basic img-basic.prg -kernal img-kernal.prg

all: img-basic.prg img-kernal.prg

$(MAIN).prg: $(MAIN).asm $(foreach MOD, $(MODS), $(MOD).asm)
	cl65 -o $@ --listing $(MAIN).lst -t none $<

img-basic.prg: $(MAIN).prg
	dd if=$< of=$@ bs=1k count=8

img-kernal.prg: $(MAIN).prg
	dd if=$< of=$@ bs=1k skip=12
