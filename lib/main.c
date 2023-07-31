#include "core.h"

int main() {
    int s[] = {1, 2, 3, 4, 5};
    struct slice sl = {.len = sizeof(s)/sizeof(s[0]), .value = s};
    struct { bool some; int32 value; } opt = { .some = false };

    return 0;
}