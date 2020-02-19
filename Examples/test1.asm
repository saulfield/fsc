		section .text
		global main
		global _start
main:
		mov rax, 1
		neg rax
		ret
_start:
		call main
		mov rdi, rax
		mov rax, 60
		syscall
