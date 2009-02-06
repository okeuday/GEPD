
% specify the PORT_DRIVER_NAME as a list (ascii string)

-define(PORT_DRIVER_NAME, "test_drv").

% list the PORT_DRIVER_FUNCTIONS in the same order
% as an enumeration starting at 1

-define(FUNCTION_sleep_test1,                 1).
-define(FUNCTION_sleep_test2,                 2).
-define(FUNCTION_integer_test1,               3).
-define(FUNCTION_char_test1,                  4).
-define(FUNCTION_char_test2,                  5).
-define(FUNCTION_float_test1,                 6).
-define(FUNCTION_pchar_test1,                 7).
-define(FUNCTION_time_test1,                  8).
-define(FUNCTION_float_test2,                 9).
-define(FUNCTION_integer_test2,              10).
-define(FUNCTION_integer_test3,              11).

% helper macros for proper binary argument types
-define(float,    :64/float-native).
-define(double,   :64/float-native).
-define(int8_t,   :8/signed-integer-native).
-define(int16_t,  :16/signed-integer-native).
-define(int32_t,  :32/signed-integer-native).
-define(int64_t,  :64/signed-integer-native).
-define(uint8_t,  :8/unsigned-integer-native).
-define(uint16_t, :16/unsigned-integer-native).
-define(uint32_t, :32/unsigned-integer-native).
-define(uint64_t, :64/unsigned-integer-native).
