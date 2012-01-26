#!/bin/sh
ERLANG_INSTALL=/usr/local
ERTS_PATH=$ERLANG_INSTALL/lib/erlang/erts-5.8.5
ERL_INTERFACE_PATH=$ERLANG_INSTALL/lib/erlang/lib/erl_interface-3.7.5
CURRENT_VERSION=vsn-1

gcc -Wall -Wextra -Werror -g -O0 -I$ERTS_PATH/include/ \
  -o test_functions.o -c -fpic test_functions.c
g++ -Wall -Wextra -Werror -Wno-unused-function -g -O0 -I$ERTS_PATH/include/ \
  -DCURRENT_VERSION=$CURRENT_VERSION \
  -include test_bindings.h \
  -save-temps -o port_driver.o -c -fpic port_driver.cpp \
  && echo "port_driver.ii contains generated code"
g++ -Wall -Wextra -Werror -g -O0 -I$ERTS_PATH/include/ \
  -o test_functions_port_driver_$CURRENT_VERSION.so \
  -shared port_driver.o test_functions.o

g++ -Wall -Wextra -Werror -g -O0 -I$ERL_INTERFACE_PATH/include/ \
  -DCURRENT_VERSION=$CURRENT_VERSION \
  -include test_bindings.h \
  -save-temps -o port.o -c -fpic port.cpp \
  && echo "port.ii contains generated code"
g++ -Wall -Wextra -Werror -g -O0 -o main.o -c -fpic main.cpp
g++ -Wall -Wextra -Werror -g -O0 \
  -L$ERL_INTERFACE_PATH/lib/ -o test_functions_port_$CURRENT_VERSION \
  port.o test_functions.o main.o -lei

gcc -Wall -Wextra -Werror -DCURRENT_VERSION=$CURRENT_VERSION \
  -include test_bindings.h \
  -E -P erlang_functions_hrl.h > erlang_functions.hrl

erlc test_bindings.erl

