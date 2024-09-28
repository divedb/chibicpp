add_executable(chibicpp chibicpp.cc)
target_link_libraries(chibicpp PRIVATE ast codegen lex parser util)