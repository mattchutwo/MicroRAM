/******************************************************************************

                     Intended to compile to many selects,
     So microram will produce many cmov and test the register allocator

*******************************************************************************/

int A0;
int A1 ;
int A2 ;
int A3 ;
int A4 ;
int A5 ;
int A6 ;
int A7 ;
int A8 ;
int A9 ;

int main() {
    
    int a0 = 0;
    int a1 = 1;
    int a2 = 2;
    int a3 = 3;
    int a4 = 4;
    int a5 = 5;
    int a6 = 6;
    int a7 = 7;
    int a8 = 8;
    int a9 = 9;
    
    if (A0 ==  1) a0 = A1+A2+A3+A4+A5+A6+A7+A8+A9;
    if (A1 == 2) a1 = a0;
    if (A2 == 3) a2 = a1;
    if (A3 == 4) a3 = a2;
    if (A4 == 5) a4 = a3;
    if (A5 == 6) a5 = a4;
    if (A6 == 7) a6 = a5;
    if (A7 == 8) a7 = a6;
    if (A8 == 9) a8 = a7;
    if (A9  == 10) a9 = a8;
    
    return a0+a2+a3+a4+a5+a6+a7+a8+a9;
}
