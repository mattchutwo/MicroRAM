/******************************************************************************

                            Simple use of Linked lists.

*******************************************************************************/
#include <stdlib.h>

extern int SECRET_SIZE __attribute__((section("__DATA,__secret")));
extern int SECRET_NUMBER __attribute__((section("__DATA,__secret")));

int main()
{
    int *array;
    int array_size = SECRET_SIZE, i=0;
    array = (int*) malloc ( sizeof(int) * array_size );

    array[0] = 21;
    array[1] = 22;
    array[2] = 23;
    array[3] = SECRET_NUMBER;
    return array[SECRET_SIZE];;
}
