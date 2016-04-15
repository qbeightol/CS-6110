#include <stdbool.h>
#include <stdio.h>

// int length(int xs[]) {
//     return sizeof(xs) / sizeof(int);
// }

void print(int xs[], int length) {
    printf("{");
    for (int i = 0; i < length - 1; i++) {
        printf("%i, ", xs[i]);
    }
    if (length > 0) { printf("%i", xs[length - 1]); }
    printf(")");
}

bool a0(int xs[], int j) {
    bool result = true;
    for (int k = 0; k < j - 1; k++) {
        result = result && xs[k] > xs[k + 1];
    }
    return result;
}

bool a1(int xs[], int i, int j) {
    bool result = true;
    for (int k = 0; k < i; k++) {
        result = result && xs[k] > xs[k + 1];
    }
    for (int k = i + 2; k < j; k++) {
        result = result && xs[k] > xs[k + 1];
    }
    return result;
}

void sort(int xs[], int length) {
    int j = 1;
    // ∀ $k. 0 ≤ $k < j - 1 => xs[$k] > xs[$k + 1]
    while (j < length) {
        printf("A0 satisfied? %i \n", a0(xs, j));
        int x = xs[j];
        int i = j - 1;
        // ∀ $k. (0 ≤ $k < i) ∨ (i + 2 ≤ $k < j - 1) => xs[$k] > xs[$k + 1]
        while (i >= 0 && xs[i] < x) {
            printf("A1 satisfied? %i \n", a1(xs, i, j));
            xs[i + 1] = xs[i];
            i--;
        }
        printf("A1 satisfied? %i \n", a1(xs, i, j));
        xs[i + 1] = x;
        j++;
    }
    printf("A0 satisfied? %i \n", a0(xs, j));
}

void test(void) {
    int xs[] = {5, 6, 8, 9, 1, 2, 4, 3, 7};
    int xs_size = sizeof(xs) / sizeof(int);
    printf("unsorted array: ");
    print(xs, xs_size);
    printf("\n");
    sort(xs, xs_size);
    printf("sorted array: ");
    print(xs, xs_size);
    printf("\n");
}

int main(void) {
    test();
}
