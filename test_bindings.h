#ifndef TEST_BINDINGS_H
#define TEST_BINDINGS_H

#if ! defined(CURRENT_VERSION)
#error CURRENT_VERSION is required as part of the port/port_driver name \
       for code upgrades/downgrades
#endif

// SUPPORTED FUNCTION ARGUMENT TYPES:
// int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t, uint64_t,
// bool, char, uchar, double, time_t, pchar_len, puint32_len
// (pchar_len represents two function arguments: char* and uint32_t
//  puint32_len represents two function arguments: uint32_t* and uint32_t
//  function argument memory is managed externally and should never be freed)

// SUPPORTED FUNCTION RETURN VALUE TYPES:
// int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t, uint64_t,
// bool, char, uchar, double, float, time_t, void,
// pchar, pchar_free, pchar_nofree,
// pchar_len_t, pchar_len_t_free, pchar_len_t_nofree
// (pchar == pchar_nofree, pchar_len_t == pchar_len_t_free,
//  pchar returns an Erlang string, i.e., list of integers,
//  pchar_len_t return an Erlang binary)

//////////////////////////////////////////////////////////////////////////////
// Port Driver Declaration
//////////////////////////////////////////////////////////////////////////////

// specify the name of the driver, as provided to driver initialization
// (e.g., erl_ddll:load_driver/2, erlang:open_port/2, ErlDrvEntry, etc.)
#define PORT_DRIVER_NAME_PREFIX test_functions_port_driver

// specify the C or C++ include file with the functions that will be called
// from within the Erlang code
#define PORT_DRIVER_C_FUNCTIONS_HEADER_FILE "test_functions.h"
//#define PORT_DRIVER_CXX_FUNCTIONS_HEADER_FILE "test_functions.h"

// specify all the functions to generate bindings for
//  __________________________________________________________________________
//  || FUNCTION     || ARITY/TYPES             || RETURN TYPE || ASYNC CALL ||
#define PORT_DRIVER_FUNCTIONS \
    ((sleep_test1,     1, (uint32_t),             void,                   0)) \
    ((sleep_test2,     1, (uint32_t),             void,                   1)) \
    ((integer_test1,   0, (),                     uint64_t,               0)) \
    ((char_test1,      1, (char),                 char,                   0)) \
    ((char_test2,      1, (uchar),                uchar,                  0)) \
    ((float_test1,     0, (),                     float,                  0)) \
    ((pchar_test1,     1, (pchar_len),            pchar_nofree,           0)) \
    ((time_test1,      1, (time_t),               pchar_nofree,           0)) \
    ((float_test2,     1, (double),               float,                  0)) \
    ((integer_test2,   4, (int8_t, int16_t,                                   \
                           int32_t, int64_t),     int32_t,                0)) \
    ((integer_test3,   4, (uint8_t, uint16_t,                                 \
                           uint32_t, uint64_t),   uint32_t,               0)) \
    ((pchar_test2,     6, (pchar_len, char,                                   \
                           pchar_len, char,                                   \
                           pchar_len, char),      pchar_nofree,           0)) \
    ((hello_test1,     0, (),                     pchar_len_t_free,       0))

//////////////////////////////////////////////////////////////////////////////
// Port Declaration
//////////////////////////////////////////////////////////////////////////////

// specify the name of the port, as provided to port initialization
// (e.g., erlang:open_port/2, executable name)
#define PORT_NAME_PREFIX test_functions_port

// specify the C or C++ include file with the functions that will be called
// from within the Erlang code
#define PORT_C_FUNCTIONS_HEADER_FILE "test_functions.h"
//#define PORT_CXX_FUNCTIONS_HEADER_FILE "test_functions.h"

// not necessary if PORT_DRIVER_FUNCTIONS is defined
//#define PORT_FUNCTIONS \
    ((sleep_test1,     1, (uint32_t),                          void    )) \
    ((sleep_test2,     1, (uint32_t),                          void    )) \
    ((integer_test1,   0, (),                                  uint64_t)) \
    ((char_test1,      1, (char),                              char    )) \
    ((char_test2,      1, (uchar),                             uchar   )) \
    ((float_test1,     0, (),                                  float   )) \
    ((pchar_test1,     1, (pchar_len),                         pchar   )) \
    ((time_test1,      1, (time_t),                            pchar   )) \
    ((float_test2,     1, (double),                            float   )) \
    ((integer_test2,   4, (int8_t,int16_t,int32_t,int64_t),    int32_t )) \
    ((integer_test3,   4, (uint8_t,uint16_t,uint32_t,uint64_t),uint32_t)) \
    ((pchar_test2,     6, (pchar_len, char, pchar_len, char, \
                           pchar_len, char), pchar))

//////////////////////////////////////////////////////////////////////////////

#include <boost/preprocessor/cat.hpp>

#if defined(PORT_DRIVER_NAME_PREFIX)
#define PORT_DRIVER_NAME \
    BOOST_PP_CAT(BOOST_PP_CAT(PORT_DRIVER_NAME_PREFIX, _), CURRENT_VERSION)
#endif
#if defined(PORT_NAME_PREFIX)
#define PORT_NAME \
    BOOST_PP_CAT(BOOST_PP_CAT(PORT_NAME_PREFIX, _), CURRENT_VERSION)
#endif


#endif // TEST_BINDINGS_H
