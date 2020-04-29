	.file	"simple.c"
	.text
	.globl	copy
	.type	copy, @function
copy:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rdx
	leaq	-16(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	strcpy@PLT
	leaq	-16(%rbp), %rax
	movq	%rax, %rdi
	call	puts@PLT
	nop
	leave
	ret
	.size	copy, .-copy
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	addq	$8, %rax
	movq	(%rax), %rax
	movq	%rax, %rdi
	call	copy
	movl	$0, %eax
	leave
	ret
	.size	main, .-main
	.ident	"GCC: (Arch Linux 9.3.0-1) 9.3.0"
	.section	.note.GNU-stack,"",@progbits
