	; global variables
	section .data
x: db 3
y: db 5

	; code
	section .text
func_add:
	; function prologue
	push rbp
	mov rbp, rsp

	; function body
	mov rax, [rbp + 16]	; get arg: x
	push rax     		; save left operand
	mov rax, [rbp + 24]	; get arg: y
	mov rcx, rax 		; move right operand into rcx
	pop rax      		; restore left operand
	add rax, rcx
	
	; function epilogue
	mov rsp, rbp
	pop rbp
	ret
func_main:
	; function prologue
	push rbp
	mov rbp, rsp

	; function body
	mov rax, 1
	push rax     		; save left operand
	mov rax, 1
	mov rcx, rax 		; move right operand into rcx
	pop rax      		; restore left operand
	cmp rax, rcx 		; compare left and right operands
	mov rax, 0   		; clear rax
	setne al 		; save result in rax
	
	; function epilogue
	mov rsp, rbp
	pop rbp
	ret
	
	; entry point
	global _start
_start:
	call func_main
	mov rdi, rax 		; call exit with return code from main
	mov rax, 60  		; sys_exit
	syscall
