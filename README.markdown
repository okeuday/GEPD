#GENERIC ERLANG PORT \[DRIVER\] (GEP[D]), VERSION 0.9.5 (August 31 2016)

## PURPOSE

To automatically generate an Erlang port driver or Erlang port
for C/C++ bindings using a single self-contained file.


Files:

* `test_bindings.h` contains the ONLY configuration to support the C functions
  (in `test_functions.h` and `test_functions.c`)
* `test_bindings.erl` provides the `gen_server` to manage the port or
  port driver
* `erlang_functions_hrl.h` creates the erlang function interface
* `port_driver.cpp` provides the Erlang port driver implementation
* `port.cpp` and `port.hpp` provide the Erlang port implementation

Whether `PORT_DRIVER_NAME_PREFIX` or `PORT_NAME_PREFIX` is defined determines
if a port driver or port is built, respectively (in `test_bindings.h`).
If both are defined, both are built and the port driver is used within
`test_bindings.erl` (undefine `ERL_PORT_DRIVER_NAME` if you want to use
the port).


Features:

* a function binding with only a single line tuple
* efficient passing of function types with no intermediate character buffer
* functionality like the Erlang Driver Tool Kit (EDTK) (no fd handling though)
* macro expansion to avoid code duplication errors
* floating point type handling
* stdout/stderr handling in the generated port code


Caveat:

(This is no longer true for `Erlang >= R15`)
The generated port driver code can not be used for hot code updating
if it performs an asynchronous call because erts will lock the driver
(making it "permanent") with `driver_lock_driver()`
(`http://erlang.org/doc/man/erl_driver.html#driver_lock_driver`).
With the driver locked, there is no possibility that an async
operation would create instability after a hot code update.


## BUILDING

Some of the features in `port_driver.cpp` requires
`erts >= 5.6.1` (`Erlang >= R12B01`).
Boost is required for the preprocessor macro expansion code
([http://www.boost.org/](http://www.boost.org/)).

To build with cmake:

    mkdir build
    cd build
    cmake ..
    make

To build with the make script:

    ./make.sh


## RUNNING

The `test_bindings` code should generate output similar to:

    $ erl +A 16
    1> test_bindings:start().
    using port driver
    {ok,<0.35.0>}
    2> test_bindings:test().
    sync sleep
    async sleep
    ...
    stdout writing before 2 second sleep
                                        ok
    3> stdout writing after 2 second sleep
                                          stderr
                                                line
                                                    break(s)
                                                            missasync function call returned: {ok,ok}
    3> 


## LICENSE

BSD license


## CONTACT

Michael Truog <mjtruog at gmail dot com>


## THANKS

* Matt Stancliff (GEPD CMake integration)
* Scott Lystig Fritchie (EDTK, i.e., Erlang Driver Tool Kit)

