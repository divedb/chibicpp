add_library(ast ast/node.cc ast/type.cc)
add_library(lex lex/tokenizer.cc)
add_library(parser parser/parser.cc)
target_link_libraries(parser PRIVATE ast)

add_library(codegen codegen/backend.cc)
add_executable(chibicpp main/chibicpp.cc)
target_link_libraries(chibicpp PRIVATE ast lex parser codegen)