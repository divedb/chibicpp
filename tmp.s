.intel_syntax noprefix
.global main
main:
  push rbp
  mov rbp, rsp
  sub rsp, 0
  push 1
  push 2
  push 3
  push 4
  push 5
  push 6
  pop r9
  pop r8
  pop rcx
  pop rdx
  pop rsi
  pop rdi
  mov rax, rsp
  and rax, 15
  jnz .L.call.0
  mov rax, 0
  call add6
  jmp .L.end.0
.L.call.0:
  sub rsp, 8
  mov rax, 0
  call add6
  add rsp, 8
.L.end.0:
  push rax
  pop rax
  jmp .L.return
.L.return:
  mov rsp, rbp
  pop rbp
  ret
