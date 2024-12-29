#include <iostream>
#include <fstream>

using namespace std;

int main() {
    ifstream input("input");
    int previous = 2147483647, current = 0;
    int total = 0;

    while (input >> current) {
        if (current > previous) {
            total++;
        }
        previous = current;
    }

    cout << total << endl;
    return 0;
}
