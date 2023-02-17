#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

extern int TOP_ARGC;
extern char** TOP_ARGV;

void* print_int(void* arg0) {
    printf("%ld\n", (int64_t)arg0);
    return NULL;
}

void* scan_int() {
    int64_t res;
    scanf("%ld", &res);
    return (void*)res;
}

void* env_int(void* arg0) {
    int64_t res;
    sscanf(TOP_ARGV[(int64_t)arg0], "%ld", &res);
    return (void*)res;
}

void* prog_exit(void* arg0) {
    exit((int64_t)arg0);
    return (void*)NULL;
}
