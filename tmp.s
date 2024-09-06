.intel_syntax noprefix
.global main
main:
  push rbp
  mov rbp, rsp
  sub rsp, 0
  call ret5
  push rax
  pop rax
  jmp .L.return
.L.return:
  mov rsp, rbp
  pop rbp
  ret
