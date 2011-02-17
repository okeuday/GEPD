# This should be included before the _INIT variables are
# used to initialize the cache.  Since the rule variables
# have if blocks on them, users can still define them here.
# But, it should still be after the platform file so changes can
# be made to those values.

IF(CMAKE_USER_MAKE_RULES_OVERRIDE)
   INCLUDE(${CMAKE_USER_MAKE_RULES_OVERRIDE})
ENDIF(CMAKE_USER_MAKE_RULES_OVERRIDE)

IF(CMAKE_USER_MAKE_RULES_OVERRIDE_CXX)
   INCLUDE(${CMAKE_USER_MAKE_RULES_OVERRIDE_CXX})
ENDIF(CMAKE_USER_MAKE_RULES_OVERRIDE_CXX)

# this is a place holder if java needed flags for javac they would go here.
IF(NOT CMAKE_Erlang_CREATE_STATIC_LIBRARY)
  SET(CMAKE_Erlang_CREATE_STATIC_LIBRARY
      "<CMAKE_Erlang_ARCHIVE> -n app beam <OBJECT_DIR>")
ENDIF(NOT CMAKE_Erlang_CREATE_STATIC_LIBRARY)

# compile a Erlang file into an object file
IF(NOT CMAKE_Erlang_COMPILE_OBJECT)
  SET(CMAKE_Erlang_COMPILE_OBJECT
    "<CMAKE_Erlang_COMPILER> <FLAGS> -o <OBJECT_DIR> <SOURCE>")
ENDIF(NOT CMAKE_Erlang_COMPILE_OBJECT)

# compile a Erlang file into an object file
IF(NOT CMAKE_Erlang_LINK_EXECUTABLE)
  SET(CMAKE_Erlang_LINK_EXECUTABLE
    "<CMAKE_Erlang_COMPILER> <FLAGS> -o <OBJECT_DIR> <SOURCE>")
ENDIF(NOT CMAKE_Erlang_LINK_EXECUTABLE)

SET(CMAKE_Erlang_FLAGS "-W +debug_info")
