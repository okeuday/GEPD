#include "test_drv_funcs.h"

#include <string.h>
#include <unistd.h>
#include <time.h>
#include <stdint.h>
#include <stdio.h>

// test asynchronous/synchronous call mechanism (and a void return value)
void sleep_test1(uint32_t arg) { sleep(arg); }
void sleep_test2(uint32_t arg) { sleep(arg); }

uint64_t integer_test1()
{
    return 0xffffffffffffffff;
}

char char_test1(char a)
{
    return a;
}

unsigned char char_test2(unsigned char a)
{
    return a;
}

float float_test1()
{
    const float x = 1.0f / 3.0f;
    return (x * 3.0 - 1.0);
}

static char pchar_test1_data[128];
char * pchar_test1(char * p, uint32_t length)
{
    memcpy(pchar_test1_data, p, length);
    pchar_test1_data[length] = '\0';
    return pchar_test1_data;
}

static char time_test1_data[128];
char * time_test1(time_t t)
{
    return ctime_r(&t, time_test1_data);
}

float float_test2(double value)
{
    return (float) value;
}

int32_t integer_test2(int8_t size1, int16_t size2,
                      int32_t size4, int64_t size8)
{
    return size1 + size2 + size4 + 
           ((size8 & 0x7fffffff) | ((size8 & 0x8000000000000000) >> 32));
}

uint32_t integer_test3(uint8_t size1, uint16_t size2,
                       uint32_t size4, uint64_t size8)
{
    return size1 + size2 + size4 + (size8 & 0xffffffff);
}

