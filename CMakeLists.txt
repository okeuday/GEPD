CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

SET(MODS ${CMAKE_SOURCE_DIR}/CMake/erlang)
SET(CMAKE_MODULE_PATH ${MODS} ${CMAKE_MODULE_PATH})
SET(CMAKE_PREFIX_PATH /usr/local/ /opt/local/ /usr/ /sw/)

FIND_PACKAGE(Erlang)
FIND_PACKAGE(Boost)

PROJECT(GEPD)

# CHANGE THESE FOR YOUR INDIVIDUAL PROJECT NAMES AND VERSIONS:
SET(target test)
SET(vsn vsn-1)

SET(PortName       ${target}_functions_port_${vsn})
SET(PortDriverName ${target}_functions_port_driver_${vsn})

INCLUDE_DIRECTORIES(
  ${ERLANG_EI_INCLUDE_PATH}
  ${ERLANG_ERTS_INCLUDE_PATH}
  ${Boost_INCLUDE_DIRS}
)

LINK_DIRECTORIES(
  ${ERLANG_EI_LIBRARY_PATH}
  ${Boost_LIBRARY_DIRS}
)

SET(inc "-include ${PROJECT_SOURCE_DIR}/${target}_bindings.h")
SET(dver -DCURRENT_VERSION=${vsn})
SET(CMAKE_CXX_FLAGS "${inc} -save-temps ${dver}")

ADD_EXECUTABLE(${PortName} port.cpp main.cpp ${target}_functions.c)
TARGET_LINK_LIBRARIES(${PortName} ei ${Boost_LIBRARIES} )

ADD_LIBRARY(${PortDriverName} SHARED port_driver.cpp ${target}_functions.c)
SET_TARGET_PROPERTIES(${PortDriverName} PROPERTIES
  PREFIX "")

ADD_DEPENDENCIES(${PortName} ErlangFunctions)
ADD_CUSTOM_TARGET(ErlangFunctions
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_C_COMPILER} -include ${PROJECT_SOURCE_DIR}/${target}_bindings.h ${dver} -I${Boost_INCLUDE_DIRS} -E -P ${PROJECT_SOURCE_DIR}/erlang_functions_hrl.h > erlang_functions.hrl
  COMMAND erlc ${PROJECT_SOURCE_DIR}/test_bindings.erl
  VERBATIM)
