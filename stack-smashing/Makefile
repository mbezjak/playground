.PHONY: help init exploit

help:
	@echo "See Makefile"

init:
	echo 0 | sudo tee /proc/sys/kernel/randomize_va_space

simple.S:
	gcc -o simple.S -S -fno-stack-protector -fno-asynchronous-unwind-tables simple.c

shellcode.bin:
	cc -m64 -c -o shellcode.o shellcode.S
	objcopy -S -O binary -j .text shellcode.o shellcode.bin

vulnerable:
	gcc -g -fno-stack-protector -z execstack vulnerable.c -o vulnerable -D_FORTIFY_SOURCE=0

exploit: vulnerable shellcode.bin
	./exploit.py | env - setarch -R ./vulnerable
