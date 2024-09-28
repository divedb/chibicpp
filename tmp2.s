.intel_syntax noprefix
.data
.L.data.0:
  .byte 37
  .byte 115
  .byte 32
  .byte 61
  .byte 62
  .byte 32
  .byte 37
  .byte 100
  .byte 10
  .byte 0
.data
.L.data.1:
  .byte 37
  .byte 115
  .byte 32
  .byte 61
  .byte 62
  .byte 32
  .byte 37
  .byte 100
  .byte 32
  .byte 101
  .byte 120
  .byte 112
  .byte 101
  .byte 99
  .byte 116
  .byte 101
  .byte 100
  .byte 32
  .byte 98
  .byte 117
  .byte 116
  .byte 32
  .byte 103
  .byte 111
  .byte 116
  .byte 32
  .byte 37
  .byte 100
  .byte 10
  .byte 0
.data
.L.data.2:
  .byte 115
  .byte 116
  .byte 114
  .byte 117
  .byte 99
  .byte 116
  .byte 32
  .byte 123
  .byte 99
  .byte 104
  .byte 97
  .byte 114
  .byte 32
  .byte 97
  .byte 59
  .byte 32
  .byte 105
  .byte 110
  .byte 116
  .byte 32
  .byte 98
  .byte 59
  .byte 32
  .byte 99
  .byte 104
  .byte 97
  .byte 114
  .byte 32
  .byte 99
  .byte 59
  .byte 125
  .byte 32
  .byte 120
  .byte 59
  .byte 32
  .byte 120
  .byte 46
  .byte 97
  .byte 61
  .byte 49
  .byte 59
  .byte 32
  .byte 120
  .byte 46
  .byte 98
  .byte 61
  .byte 50
  .byte 59
  .byte 32
  .byte 120
  .byte 46
  .byte 99
  .byte 61
  .byte 51
  .byte 59
  .byte 32
  .byte 120
  .byte 46
  .byte 97
  .byte 59
  .byte 0
.data
.L.data.3:
  .byte 79
  .byte 75
  .byte 10
  .byte 0
.text
.global assert
assert:
  push rbp
  mov rbp, rsp
  sub rsp, 24
  mov [rbp-24], rdi
  mov [rbp-16], rsi
  mov [rbp-8], rdx
  lea rax, [rbp-24]
  push rax
  pop rax
  mov rax, [rax]
  push rax
  lea rax, [rbp-16]
  push rax
  pop rax
  mov rax, [rax]
  push rax
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzb rax, al
  push rax
  pop rax
  cmp rax, 0
  je  .L.else.0
  push offset .L.data.0
  lea rax, [rbp-8]
  push rax
  pop rax
  mov rax, [rax]
  push rax
  lea rax, [rbp-16]
  push rax
  pop rax
  mov rax, [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  mov rax, rsp
  and rax, 15
  jnz .L.call.1
  mov rax, 0
  call printf
  jmp .L.end.1
.L.call.1:
  sub rsp, 8
  mov rax, 0
  call printf
  add rsp, 8
.L.end.1:
  push rax
  add rsp, 8
  jmp .L.end.0
.L.else.0:
  push offset .L.data.1
  lea rax, [rbp-8]
  push rax
  pop rax
  mov rax, [rax]
  push rax
  lea rax, [rbp-24]
  push rax
  pop rax
  mov rax, [rax]
  push rax
  lea rax, [rbp-16]
  push rax
  pop rax
  mov rax, [rax]
  push rax
  pop rcx
  pop rdx
  pop rsi
  pop rdi
  mov rax, rsp
  and rax, 15
  jnz .L.call.2
  mov rax, 0
  call printf
  jmp .L.end.2
.L.call.2:
  sub rsp, 8
  mov rax, 0
  call printf
  add rsp, 8
.L.end.2:
  push rax
  add rsp, 8
  push 1
  pop rdi
  mov rax, rsp
  and rax, 15
  jnz .L.call.3
  mov rax, 0
  call exit
  jmp .L.end.3
.L.call.3:
  sub rsp, 8
  mov rax, 0
  call exit
  add rsp, 8
.L.end.3:
  push rax
  add rsp, 8
.L.end.0:
.L.return.assert:
  mov rsp, rbp
  pop rbp
  ret
.text
.global main
main:
  push rbp
  mov rbp, rsp
  sub rsp, 10
  push 1
  lea rax, [rbp-10]
  push rax
  pop rax
  add rax, 0
  push rax
  push 1
  pop rdi
  pop rax
  mov [rax], dil
  push rdi
  add rsp, 8
  lea rax, [rbp-10]
  push rax
  pop rax
  add rax, 1
  push rax
  push 2
  pop rdi
  pop rax
  mov [rax], rdi
  push rdi
  add rsp, 8
  lea rax, [rbp-10]
  push rax
  pop rax
  add rax, 9
  push rax
  push 3
  pop rdi
  pop rax
  mov [rax], dil
  push rdi
  add rsp, 8
  lea rax, [rbp-10]
  push rax
  pop rax
  add rax, 0
  push rax
  pop rax
  movsx rax, byte ptr [rax]
  push rax
  push offset .L.data.2
  pop rdx
  pop rsi
  pop rdi
  mov rax, rsp
  and rax, 15
  jnz .L.call.4
  mov rax, 0
  call assert
  jmp .L.end.4
.L.call.4:
  sub rsp, 8
  mov rax, 0
  call assert
  add rsp, 8
.L.end.4:
  push rax
  add rsp, 8
  push offset .L.data.3
  pop rdi
  mov rax, rsp
  and rax, 15
  jnz .L.call.5
  mov rax, 0
  call printf
  jmp .L.end.5
.L.call.5:
  sub rsp, 8
  mov rax, 0
  call printf
  add rsp, 8
.L.end.5:
  push rax
  add rsp, 8
  push 0
  pop rax
  jmp .L.return.main
.L.return.main:
  mov rsp, rbp
  pop rbp
  ret
