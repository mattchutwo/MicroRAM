/******************************************************************************

                            Simple use of Binary Trees.

           Could be made better by taking the data as input.

*******************************************************************************/

struct node
{
  int key,data; //node will stores a key and some data
  struct node *right_child; // right child
  struct node *left_child; // left child
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

//function to create a node
struct node* new_node(int k, int d)
{
    struct node *p;
    p = malloc(sizeof(struct node));
    p->key = k;
    p->data = d;
    p->left_child = 0;
    p->right_child = 0;

    return p;
}

struct node* insert(struct node *root, int k , int d)
{
    //searching for the place to insert
    if(root==0)
        return new_node(k,d);
    else if(k>root->data) // k is greater. Should be inserted to right
        root->right_child = insert(root->right_child, k, d);
    else // k is smaller should be inserted to left
        root->left_child = insert(root->left_child,k,d);
    return root;
}

int main()
{
    struct node *root;
    root = new_node(20,40);
    insert(root,5,10);
    insert(root,1,2);
    insert(root,15,30);
    insert(root,9,18);
    insert(root,7,14);
    insert(root,12,24);
    insert(root,30,60);
    insert(root,25,50);
    insert(root,40,80);
    insert(root, 45,90);
    insert(root, 42,84);

    return search(root,42);
}


