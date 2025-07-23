	.file	"test.c"
	.text
	.p2align 4
	.globl	scheme_entry
	.type	scheme_entry, @function
scheme_entry:
	movl	$42, %eax
	ret
	.size	scheme_entry, .-scheme_entry
	.ident	"GCC: (GNU) 15.1.1 20250521 (Red Hat 15.1.1-2)"
	.section	.note.GNU-stack,"",@progbits
