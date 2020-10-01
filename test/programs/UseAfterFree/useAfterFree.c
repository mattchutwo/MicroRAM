/******************************************************************************

                            Simple bug Use after free

*******************************************************************************/
#include <stdlib.h>

extern int SECRET_BOUND __attribute__((section("__DATA,__secret")));
extern int SECRET_NUMBER __attribute__((section("__DATA,__secret")));

int main()
{
    int *array;
    int ret, array_size = 3;
    
    array = (int*) malloc ( sizeof(int) * array_size );

    array[0] = 21;
    array[1] = 22;
    array[2] = SECRET_NUMBER;
    ret = array [1];
    
    
    // Free the array
    if (SECRET_BOUND <= 144) { free (array); }

    
    if (SECRET_BOUND >= 144) { ret = array[2]; }

    return ret;
}
