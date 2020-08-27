/******************************************************************************

                            (Very) Simple use of Structs.

Simplest use of structs. If you optimize, this becomes trivial.

*******************************************************************************/
static int SECRET_NUMBER1 __attribute__((section("__DATA,__secret"))) = 3;
static int SECRET_NUMBER2 __attribute__((section("__DATA,__secret"))) = 5;
static int SECRET_NUMBER3 __attribute__((section("__DATA,__secret"))) = 8;

 
struct Node { 
    int data; 
    struct Node* next; 
};  

int main() {
  struct Node third = {SECRET_NUMBER3, 0};
  struct Node second = {SECRET_NUMBER2, &third};
  struct Node first = {SECRET_NUMBER1, &second};
  struct Node* tail;

  int x1,tmp;

  
  x1 = first.data;
  tail = first.next;

  tmp = tail->data;
  x1 = x1 + tmp;
  tail = tail->next;
  
  tmp = tail->data;
  x1 = x1 + tmp;
  
  return x1;
};
