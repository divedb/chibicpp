chibicpp_add_library(
NAME
    lex
HDRS
SRCS
    "tokenizer.cc"
COPTS
    ${CHIBICPP_LIB_CXXFLAGS}
DEPS
    util
ALIAS
    chibicpp::lex
)