chibicpp_add_library(
NAME
    codegen
HDRS
SRCS
    "backend.cc"
COPTS
    ${CHIBICPP_LIB_CXXFLAGS}
DEPS
    lex
    parser
    util
ALIAS
    chibicpp::codegen
)