#include "test_functions.h"

#include <string.h>
#include <unistd.h>
#include <time.h>
#include <stdint.h>
#include <stdio.h>

// test asynchronous/synchronous call mechanism (and a void return value)
void sleep_test1(uint32_t arg)
{
    sleep(arg);
}

void sleep_test2(uint32_t arg)
{
    fprintf(stdout, "stdout writing before %d second sleep\n", arg);
    sleep(arg);
    fprintf(stdout, "stdout writing after %d second sleep\n", arg);
    fprintf(stderr, "stderr\nline\nbreak(s)\nmissing");
}

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

static char pchar_test2_data[128];
char * pchar_test2(char * p1, uint32_t l1, char c1,
                   char * p2, uint32_t l2, char c2,
                   char * p3, uint32_t l3, char c3)
{
    memcpy(&pchar_test2_data[0], p1, l1);
    pchar_test2_data[l1] = c1;
    memcpy(&pchar_test2_data[l1 + 1], p2, l2);
    pchar_test2_data[l1 + l2 + 1] = c2;
    memcpy(&pchar_test2_data[l1 + l2 + 2], p3, l3);
    pchar_test2_data[l1 + l2 + l3 + 2] = c3;
    pchar_test2_data[l1 + l2 + l3 + 3] = '\0';
    return pchar_test2_data;
}

