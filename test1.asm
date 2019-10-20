		section .text
		global main
main:
		mov rax, 5
		ret
		global _start
_start:
		call main
		mov rdi, rax
		mov rax, 60
		syscall
