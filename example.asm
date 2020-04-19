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
	push rax      		; initialize new variable: z
	mov rax, [rbp - 8]	; get local variable: z
	push rax     		; save left operand
	mov rax, 0
	mov rcx, rax 		; move right operand into rcx
	pop rax      		; restore left operand
	cmp rax, rcx 		; compare left and right operands
	mov rax, 0   		; clear rax
	setg al 		; save result in rax
	cmp rax, 0
	je .L0
	mov rax, 2
	mov [rbp + -8], rax	; assign local variable or arg
	jmp .L1
.L0:
	mov rax, 3
	mov [rbp + -8], rax	; assign local variable or arg
.L1:
	mov rax, [rbp - 8]	; get local variable: z
	
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
