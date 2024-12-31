#include <iostream>

using namespace std;

int main() {
    int previous = 2147483647, current = 0;
    int total = 0;

    while (cin >> current) {
        if (current > previous) {
            total++;
        }
        previous = current;
    }

    cout << total << endl;
    return 0;
}
