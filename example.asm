		global _start
main:
		mov rax, 3
		ret
_start:
		call main
		mov rdi, rax
		mov rax, 60
		syscall
