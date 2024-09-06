add_executable(tokenizer_test tokenizer_test.cc)
target_link_libraries(tokenizer_test PRIVATE lex)

add_executable(lexer_test lexer_test.cc)
target_link_libraries(lexer_test PRIVATE lex)