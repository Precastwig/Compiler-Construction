	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
	.globl	print
	.type	print, @function
print:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf
	movl	$0, %edi
	call	exit
	.cfi_endproc
.LFE2:
	.size	print, .-print
	.globl	main
	.type	main, @function
main:
.LFB3:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
//End template code
push $0
push $0
WHILE0:
//offset 1
mov -24(%rbp), %rax
push %rax
push $100
pop %rax
pop %rbx
cmp %rax, %rbx
push %rbx
jz ENDWHILE0
//offset 1
mov -24(%rbp), %rax
push %rax
push $1
pop %rax
pop %rbx
add %rax, %rbx
push %rbx
pop -24(%rbp)
//offset 1
mov -24(%rbp), %rax
push %rax
popq %rax
push $3
popq %rbx
idiv %rbx
push %rax
push $1
pop %rax
pop %rbx
cmp %rax, %rbx
push %rbx
jz IF1
//offset 1
mov -24(%rbp), %rax
push %rax
popq %rax
push $5
popq %rbx
idiv %rbx
push %rax
push $1
pop %rax
pop %rbx
cmp %rax, %rbx
push %rbx
jz IF2
//offset 1
mov -24(%rbp), %rax
push %rax
jmp ENDIF2
IF2:
push $5
popq %rdi
call print
ENDIF2:
jmp ENDIF2
IF2:
//offset 1
mov -24(%rbp), %rax
push %rax
popq %rax
push $5
popq %rbx
idiv %rbx
push %rax
push $1
pop %rax
pop %rbx
cmp %rax, %rbx
push %rbx
jz IF3
push $3
popq %rdi
call print
jmp ENDIF3
IF3:
push $53
popq %rdi
call print
ENDIF3:
ENDIF3:
jmp WHILE3
ENDWHILE3:
pop %rax
pop %rbx
push %rax
pop %rax
pop %rbx
push %rax
//Begin templates again Moves top stack to edi
	popq %rdi
	//Below auto
	call	print
	movl	$1, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.2) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
