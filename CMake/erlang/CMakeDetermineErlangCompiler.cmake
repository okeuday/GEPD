
# determine the compiler to use for Erlang programs
# NOTE, a generator may set CMAKE_Erlang_COMPILER before
# loading this file to force a compiler.

IF(NOT CMAKE_Erlang_COMPILER)
  # prefer the environment variable CC
  IF($ENV{ERLANG_COMPILER} MATCHES ".+")
    GET_FILENAME_COMPONENT(CMAKE_Erlang_COMPILER_INIT $ENV{ERLANG_COMPILER} PROGRAM PROGRAM_ARGS CMAKE_Erlang_FLAGS_ENV_INIT)
    IF(CMAKE_Erlang_FLAGS_ENV_INIT)
      SET(CMAKE_Erlang_COMPILER_ARG1 "${CMAKE_Erlang_FLAGS_ENV_INIT}" CACHE STRING "First argument to Erlang compiler")
    ENDIF(CMAKE_Erlang_FLAGS_ENV_INIT)
    IF(NOT EXISTS ${CMAKE_Erlang_COMPILER_INIT})
      MESSAGE(SEND_ERROR "Could not find compiler set in environment variable ERLANG_COMPILER:\n$ENV{ERLANG_COMPILER}.")
    ENDIF(NOT EXISTS ${CMAKE_Erlang_COMPILER_INIT})
  ENDIF($ENV{ERLANG_COMPILER} MATCHES ".+")

  IF($ENV{ERLANG_RUNTIME} MATCHES ".+")
    GET_FILENAME_COMPONENT(CMAKE_Erlang_RUNTIME_INIT $ENV{ERLANG_RUNTIME} PROGRAM PROGRAM_ARGS CMAKE_Erlang_FLAGS_ENV_INIT)
    IF(NOT EXISTS ${CMAKE_Erlang_RUNTIME_INIT})
      MESSAGE(SEND_ERROR "Could not find compiler set in environment variable ERLANG_RUNTIME:\n$ENV{ERLANG_RUNTIME}.")
    ENDIF(NOT EXISTS ${CMAKE_Erlang_RUNTIME_INIT})
  ENDIF($ENV{ERLANG_RUNTIME} MATCHES ".+")

  IF($ENV{ERLANG_ARCHIVE} MATCHES ".+")
    GET_FILENAME_COMPONENT(CMAKE_Erlang_ARCHIVE_INIT $ENV{ERLANG_ARCHIVE} PROGRAM PROGRAM_ARGS CMAKE_Erlang_FLAGS_ENV_INIT)
    IF(NOT EXISTS ${CMAKE_Erlang_ARCHIVE_INIT})
      MESSAGE(SEND_ERROR "Could not find compiler set in environment variable ERLANG_ARCHIVE:\n$ENV{ERLANG_ARCHIVE}.")
    ENDIF(NOT EXISTS ${CMAKE_Erlang_ARCHIVE_INIT})
  ENDIF($ENV{ERLANG_ARCHIVE} MATCHES ".+")

  SET(Erlang_BIN_PATH
    $ENV{ERLANG_HOME}/bin
    /usr/bin
    /usr/local/bin
    /opt/local/bin
    /sw/bin
    )
  # if no compiler has been specified yet, then look for one
  IF(CMAKE_Erlang_COMPILER_INIT)
    SET(CMAKE_Erlang_COMPILER ${CMAKE_Erlang_COMPILER_INIT} CACHE PATH "Erlang Compiler")
  ELSE(CMAKE_Erlang_COMPILER_INIT)
    FIND_PROGRAM(CMAKE_Erlang_COMPILER
      NAMES erlc
      PATHS ${Erlang_BIN_PATH}
    )
  ENDIF(CMAKE_Erlang_COMPILER_INIT)

  # if no runtime has been specified yet, then look for one
  IF(CMAKE_Erlang_RUNTIME_INIT)
    SET(CMAKE_Erlang_RUNTIME ${CMAKE_Erlang_RUNTIME_INIT} CACHE PATH "Erlang Compiler")
  ELSE(CMAKE_Erlang_RUNTIME_INIT)
    FIND_PROGRAM(CMAKE_Erlang_RUNTIME
      NAMES erl
      PATHS ${Erlang_BIN_PATH}
    )
  ENDIF(CMAKE_Erlang_RUNTIME_INIT)

ENDIF(NOT CMAKE_Erlang_COMPILER)
MARK_AS_ADVANCED(CMAKE_Erlang_COMPILER)

  # if no archive has been specified yet, then look for one
  IF(CMAKE_Erlang_ARCHIVE_INIT)
    SET(CMAKE_Erlang_ARCHIVE ${CMAKE_Erlang_ARCHIVE_INIT} CACHE PATH "Erlang Compiler")
  ELSE(CMAKE_Erlang_ARCHIVE_INIT)
    FIND_PROGRAM(CMAKE_Erlang_ARCHIVE
      NAMES zip
      PATHS ${Erlang_BIN_PATH}
    )
  ENDIF(CMAKE_Erlang_ARCHIVE_INIT)
MARK_AS_ADVANCED(CMAKE_Erlang_COMPILER)

# configure variables set in this file for fast reload later on
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/CMake/erlang/CMakeErlangCompiler.cmake.in
  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeErlangCompiler.cmake IMMEDIATE @ONLY)
SET(CMAKE_Erlang_COMPILER_ENV_VAR "ERLANG_COMPILER")
