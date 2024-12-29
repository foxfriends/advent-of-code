#include <stdio.h>

int main() {
    int lo1, hi1, lo2, hi2;

    int count = 0;

    while (scanf("%d-%d,%d-%d", &lo1, &hi1, &lo2, &hi2) != EOF) {
        if (hi1 >= lo2 && lo1 <= hi2) { ++count; }
        else if (hi2 >= lo1 && lo2 <= hi1) { ++count; }
    }

    printf("%d\n", count);

    return 0;
}
