/******************************************************************************

           (Very) Simple use ofbinary Trees.

           Testing Structs and getelementptr

*******************************************************************************/
static int SECRET_NUMBER __attribute__((section("__DATA,__secret"))) = 42;

struct node
{
  int key,data; //node will stores a key and some data
  struct node *left_child; // left child
  struct node *right_child; // right child
};

int search(struct node *root, int x)
{
    if(root==0)
        return 0;
    else if (root->key==x) //if root->data is x then the element is found
        return root->data;
    else if (x>root->key) // x is greater, so we will search the right subtree
        return search(root->right_child, x);
    else //x is smaller than the data, so we will search the left subtree
        return search(root->left_child,x);
}

int main()
{
  struct node secret_leaf = {10, SECRET_NUMBER, 0, 0};
  struct node leaf2 = {1,41, 0, 0};
  struct node root = {5, 45, &leaf2, &secret_leaf};

  return search(&root,10);
}


