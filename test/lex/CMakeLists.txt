chibicpp_add_test(
NAME
    tokenizer_test
HDRS
SRCS
    "tokenizer_test.cc"
COPTS
    ${CHIBICPP_TEST_CXXFLAGS}
DEPS
    ${GTEST_LIBRARIES} lex
)