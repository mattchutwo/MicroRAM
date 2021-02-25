/******************************************************************************

                            (Very) Simple use of Arrays.

Simplest use of Arrays. If you optimize, this becomes trivial.

*******************************************************************************/
static int SECRET_ARRAY[10] __attribute__((section("__DATA,__secret"))) = {2,1,3,4,7,11,18,29,47,76};
static int SECRET_INDEX __attribute__((section("__DATA,__secret"))) = 5;

int main() {
    return SECRET_ARRAY[SECRET_INDEX];
};
