
all: xr.bin

lbr: xr.lbr

clean:
	rm -f xr.lst
	rm -f xr.bin
	rm -f xr.lbr

xr.bin: xr.asm include/bios.inc include/kernel.inc
	asm02 -L -b xr.asm
	rm -f xr.build

xr.lbr: xr.bin
	rm -f xr.lbr
	lbradd xr.lbr xr.bin

