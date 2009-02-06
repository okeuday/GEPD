#ifndef TEST_DRV_FUNCS_H
#define TEST_DRV_FUNCS_H

#include <stdint.h>
#include <time.h>
void sleep_test1(uint32_t arg);
void sleep_test2(uint32_t arg);
uint64_t integer_test1();
char char_test1(char a);
unsigned char char_test2(unsigned char a);
float float_test1();
char * pchar_test1(char * p, uint32_t length);
char * time_test1(time_t t);


float float_test2(double value);
int32_t integer_test2(int8_t size1, int16_t size2,
                      int32_t size4, int64_t size8);
uint32_t integer_test3(uint8_t size1, uint16_t size2,
                       uint32_t size4, uint64_t size8);

#endif // TEST_DRV_FUNCS_H
