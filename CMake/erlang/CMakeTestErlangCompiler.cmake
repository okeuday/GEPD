
# This file is used by EnableLanguage in cmGlobalGenerator to
# determine that that selected Erlang compiler can actually compile
# and link the most basic of programs.   If not, a fatal error
# is set and cmake stops processing commands and will not generate
# any makefiles or projects.
SET(CMAKE_Erlang_COMPILER_WORKS 1 CACHE INTERNAL "")
