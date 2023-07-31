#include "test.h"
int32 main() {
int32 i[4][4] = {{1, 2, 3, 4}, {2, 3, 4, 5}, {3, 4, 5, 6}, {4, 5, 6, 7}};
int32 ii[4] = ((int64 (*)[4]) { .ptr = i, .len = sizeof(i)/sizeof(i[0]) - 2}.ptr)[1];
struct slice ival = ii[1];
struct { usize len; struct { usize len; int32 *ptr; } *ptr; } i;
}
