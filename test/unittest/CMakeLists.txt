set(TEST_DIR "${CMAKE_SOURCE_DIR}/test/unittest")

message(STATUS "${CMAKE_BINARY_DIR}/tmp.s")

add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/tmp.s
    COMMAND chibicpp ${TEST_DIR}/tests > ${CMAKE_BINARY_DIR}/tmp.s
    DEPENDS chibicpp ${TEST_DIR}/tests
    COMMENT "Generating assembly from test file in ${TEST_DIR}"
)

add_custom_target(compile_test
  COMMAND gcc -static -o ${CMAKE_BINARY_DIR}/tmp ${CMAKE_BINARY_DIR}/tmp.s
  DEPENDS ${CMAKE_BINARY_DIR}/tmp.s
  COMMENT "Compiling assembly to executable"
)

add_test(NAME run_chibicpp_test
  COMMAND ${CMAKE_BINARY_DIR}/tmp
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

add_library(dummy dummy.cc)
add_dependencies(dummy compile_test)
