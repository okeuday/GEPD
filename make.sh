#!/bin/sh
ERLANG_INSTALL=/usr/local
ERTS_PATH=$ERLANG_INSTALL/lib/erlang/erts-5.6.5
ERL_INTERFACE_PATH=$ERLANG_INSTALL/lib/erlang/lib/erl_interface-3.5.9
CURRENT_VERSION=_vsn-1

gcc -I$ERTS_PATH/include/ -o test_functions.o -c -fpic test_functions.c
g++ -I$ERTS_PATH/include/ -DCURRENT_VERSION=$CURRENT_VERSION \
  -save-temps -o port_driver.o -c -fpic port_driver.cpp \
  && echo "port_driver.ii contains generated code"
g++ -I$ERTS_PATH/include/ -o test_functions_port_driver$CURRENT_VERSION.so \
  -shared port_driver.o test_functions.o

g++ -I$ERL_INTERFACE_PATH/include/ -DCURRENT_VERSION=$CURRENT_VERSION \
  -save-temps -o port.o -c -fpic port.cpp \
  && echo "port.ii contains generated code"
g++ -L$ERL_INTERFACE_PATH/lib/ -o test_functions_port$CURRENT_VERSION \
  port.o test_functions.o -lei

gcc -DCURRENT_VERSION=$CURRENT_VERSION \
  -E -P erlang_functions_hrl.h > erlang_functions.hrl

erlc test_bindings.erl

