# - Find Erlang
# This module finds if Erlang is installed and determines where the
# include files and libraries are. This code sets the following
# variables:
#
#  ERLANG_RUNTIME    = the full path to the Erlang runtime
#  ERLANG_COMPILE    = the full path to the Erlang compiler
#  ERLANG_EI_PATH    = the full path to the Erlang erl_interface path
#  ERLANG_ERTS_PATH    = the full path to the Erlang erts path
#  ERLANG_EI_INCLUDE_PATH = /include appended to ERLANG_EI_PATH
#  ERLANG_EI_LIBRARY_PATH = /lib appended to ERLANG_EI_PATH
#  ERLANG_ERTS_INCLUDE_PATH = /include appended to ERLANG_ERTS_PATH
#  ERLANG_ERTS_LIBRARY_PATH = /lib appended to ERLANG_ERTS_PATH
#

SET(ERLANG_BIN_PATH
  $ENV{ERLANG_HOME}/bin
  /usr/bin
  /usr/local/bin
  /opt/local/bin
  /sw/bin
  )
FIND_PROGRAM(ERLANG_RUNTIME
  NAMES erl
  PATHS ${ERLANG_BIN_PATH}
)

FIND_PROGRAM(ERLANG_COMPILE
  NAMES erlc
  PATHS ${ERLANG_BIN_PATH}
)

EXECUTE_PROCESS(COMMAND
         erl -noshell -eval "io:format(\"~s\", [code:lib_dir()])" -s erlang halt
         OUTPUT_VARIABLE ERLANG_OTP_LIB_DIR)

EXECUTE_PROCESS(COMMAND
         erl -noshell -eval "io:format(\"~s\", [code:root_dir()])" -s erlang halt
         OUTPUT_VARIABLE ERLANG_OTP_ROOT_DIR)

MESSAGE(STATUS "Using OTP lib: ${ERLANG_OTP_LIB_DIR} - found")

EXECUTE_PROCESS(COMMAND ls ${ERLANG_OTP_LIB_DIR}
                COMMAND grep erl_interface
                COMMAND sort -n
                COMMAND tail -1
                COMMAND tr -d \n
                OUTPUT_VARIABLE ERLANG_EI_DIR)

EXECUTE_PROCESS(COMMAND ls ${ERLANG_OTP_ROOT_DIR}
                COMMAND grep erts
                COMMAND sort -n
                COMMAND tail -1
                COMMAND tr -d \n
                OUTPUT_VARIABLE ERLANG_ERTS_DIR)

MESSAGE(STATUS "Using erl_interface version: ${ERLANG_EI_DIR}")
MESSAGE(STATUS "Using erts version: ${ERLANG_ERTS_DIR}")

SET(ERLANG_EI_PATH ${ERLANG_OTP_LIB_DIR}/${ERLANG_EI_DIR})
SET(ERLANG_EI_INCLUDE_PATH ${ERLANG_OTP_LIB_DIR}/${ERLANG_EI_DIR}/include)
SET(ERLANG_EI_LIBRARY_PATH ${ERLANG_OTP_LIB_DIR}/${ERLANG_EI_DIR}/lib)

SET(ERLANG_ERTS_PATH ${ERLANG_OTP_ROOT_DIR}/${ERLANG_ERTS_DIR})
SET(ERLANG_ERTS_INCLUDE_PATH ${ERLANG_OTP_ROOT_DIR}/${ERLANG_ERTS_DIR}/include)
SET(ERLANG_ERTS_LIBRARY_PATH ${ERLANG_OTP_ROOT_DIR}/${ERLANG_ERTS_DIR}/lib)

FIND_PROGRAM(ERLANG_ARCHIVE
  NAMES zip
  PATHS ${ERLANG_BIN_PATH}
)
MARK_AS_ADVANCED(
ERLANG_RUNTIME
ERLANG_COMPILE
ERLANG_ARCHIVE
ERLANG_EI_PATH
ERLANG_EI_INCLUDE_PATH
ERLANG_EI_LIBRARY_PATH
)
