#ifndef GEPD_PCHAR_LEN_T_H
#define GEPD_PCHAR_LEN_T_H

#include <stdint.h>

/* Memory Usage Concerns
 *
 * pchar_len_t  return values always have free() called on pchar
 *                after the contents has been processed
 * char *       return values never have free() called on the pointer
 *
 * This is done to facilitate different usage which is common to both.
 * If constant strings (char const *) need to be returned, then the return
 * value can be char * which becomes a list of integers in Erlang
 * (an Erlang "string").  Any heap allocated memory should be passed within
 * a pchar_len_t and should use malloc to allocate the pchar variable.
 * The pchar_len_t return value becomes a binary in Erlang.
 */
typedef struct pchar_len_t {
    char * pchar;
    uint32_t length;
} pchar_len_t;

#endif /* GEPD_PCHAR_LEN_T_H */
