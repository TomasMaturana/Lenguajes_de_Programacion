#include <iostream>
using namespace std;


long naive_f (long n)
{
    if (n == 0)
    {
        return 0;
    }
    else
    {
        short c = (n % 2 == 0) ? 1 : -1;
        return naive_f(n - 1) + c * n;
    }
}

// PARTE a)
// ¿Qué valor observó que “agota” la pila?
// Para los valores mayores a 174631 (desde el 174632 en adelante) 



// PARTE b)
long smart_f (long n, long accum =0){
    if (n == 0){
        return accum;
    }
    else{
        short c = (n % 2 == 0) ? 1 : -1;
        return smart_f(n - 1, accum + (c * n));
    }
}


int main ()
{
    long number, res;
    cout << "Please enter a natural number: ";
    cin >> number;
    // res = naive_f(number);
    res = smart_f(number);
    cout << "Its image through f is: " << res << ".\n";
    return 0;
}