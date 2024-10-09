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

chibicpp_add_test(
NAME
    parser_test
HDRS
SRCS
    "parser_test.cc"
COPTS
    ${CHIBICPP_TEST_CXXFLAGS}
DEPS
    ${GTEST_LIBRARIES} parser
)