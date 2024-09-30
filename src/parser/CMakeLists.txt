chibicpp_add_library(
NAME
    parser
HDRS
SRCS
    "scope.cc"    
    "symbol_table.cc"
COPTS
    ${CHIBICPP_LIB_CXXFLAGS}
DEPS
    ast
    # lex
    # util
ALIAS
    chibicpp::parser
)