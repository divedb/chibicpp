chibicpp_add_test(
NAME
    parser_test
HDRS
SRCS
    "parser_test.cc"
COPTS
    ${CHIBICPP_TEST_CXXFLAGS}
DEPS
    ${GTEST_LIBRARIES} lex parser
)

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

chibicpp_add_test(
NAME
    scope_test
HDRS
SRCS
    "scope_test.cc"
COPTS
    ${CHIBICPP_TEST_CXXFLAGS}
DEPS
    ${GTEST_LIBRARIES} parser
)