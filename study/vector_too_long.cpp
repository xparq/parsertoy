/*****************************************************************************
  The amazing "vector too long" error for `auto v = vector<X>{"a", "b"}`.
  Both in MSVC and GCC. No warning even with -Wall.
  https://stackoverflow.com/questions/77011695/haunted-stdvector-construction
 *****************************************************************************/

#include <vector>
#include <exception>
#include <iostream>
using namespace std;

int main()
{
    try { 
        struct X {
            X(string) {}       // Just adding this to confuse you even more!... ;)
            explicit X(int) {} // But this one's still a mystery: why doesn't the `explicit` stop it?
        };
        vector<X>("a", "b");
    } catch (exception& x) {
        cerr << x.what();
    }
}
