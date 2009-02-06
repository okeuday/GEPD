#!/bin/sh
ERTS_PATH=/usr/local/lib/erlang/erts-5.6.5

gcc -I$ERTS_PATH/include/ -o test_drv_funcs.o -c -fpic test_drv_funcs.c
g++ -I$ERTS_PATH/include/ -save-temps -o test_drv.o -c -fpic test_drv.cpp \
  && echo "test_drv.ii contains generated code"
g++ -I$ERTS_PATH/include/ -o test_drv.so -shared test_drv.o test_drv_funcs.o

erlc test_drv.erl
