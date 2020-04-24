.PHONY: help init example1

help:
	@echo "See Makefile"

init:
	echo 0 | sudo tee /proc/sys/kernel/randomize_va_space

example1:
	gcc -S -fno-stack-protector -z execstack -no-pie -fno-asynchronous-unwind-tables -fno-dwarf2-cfi-asm -o example1.s example1.c
