#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

void* print_int(void* arg0) {
    printf("%ld\n", (int64_t)arg0);
    return NULL;
}

void* scan_int() {
    int64_t res;
    scanf("%ld", &res);
    return (void*)res;
}