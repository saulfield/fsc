		global _start
main:
		mov rax, 5
		push rax
		mov rax, 2
		mov rcx, rax
		pop rax
		mul rcx
		push rax
		mov rax, 6
		push rax
		mov rax, 3
		mov rcx, rax
		pop rax
		div rcx
		mov rcx, rax
		pop rax
		sub rax, rcx
		push rax
		mov rax, 1
		mov rcx, rax
		pop rax
		add rax, rcx
		push rax
		mov rax, 6
		mov rcx, rax
		pop rax
		add rax, rcx
		ret
_start:
		call main
		mov rdi, rax
		mov rax, 60
		syscall
