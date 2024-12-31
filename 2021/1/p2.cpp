#include <iostream>
#include <fstream>

using namespace std;

int main() {
    int total = 0;
    int a = 0, b = 0, c = 0, d = 0;
    int rownumber = 0;

    while (cin >> d) {
        if (rownumber >= 3) {
            if (a + b + c < b + c + d) {
                total++;
            }
        }
        rownumber++;
        a = b;
        b = c;
        c = d;
    }

    cout << total << endl;
    return 0;
}
