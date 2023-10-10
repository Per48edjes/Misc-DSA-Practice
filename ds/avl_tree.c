#include "avl_tree.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/* Helper functions */

static void swap_keys(Node_t* node1, Node_t* node2)
{
    if (node1 == NULL || node2 == NULL)
    {
        fprintf(stderr, "Error: node is NULL in swap_keys\n");
        exit(EXIT_FAILURE);
    }
    int temp = node1->key;
    node1->key = node2->key;
    node2->key = temp;
}

static Node_t* predecessor(Node_t* node)
{
    if (node == NULL)
    {
        fprintf(stderr, "Error: node is NULL in predecessor\n");
        exit(EXIT_FAILURE);
    }
    // Case: Left child exists
    if (node->left != NULL)
    {
        return find_max_avl_tree(&node->left);
    }
    // Case: Left child does not exist
    else
    {
        Node_t* parent = node->parent;
        while (parent != NULL && node == parent->left)
        {
            node = parent;
            parent = parent->parent;
        }
        if (parent == NULL)
        {
            return NULL;
        }
        else
        {
            return parent;
        }
    }
}

static Node_t* successor(Node_t* node)
{
    if (node == NULL)
    {
        fprintf(stderr, "Error: node is NULL in successor\n");
        exit(EXIT_FAILURE);
    }
    // Case: Right child exists
    if (node->right != NULL)
    {
        return find_min_avl_tree(&node->right);
    }
    // Case: Right child does not exist
    else
    {
        Node_t* parent = node->parent;
        while (parent != NULL && node == parent->right)
        {
            node = parent;
            parent = parent->parent;
        }
        if (parent == NULL)
        {
            return NULL;
        }
        else
        {
            return parent;
        }
    }
}

static void rotate_right(Node_t* node)
{
    if (node == NULL)
    {
        fprintf(stderr, "Error: node is NULL in rotate_right\n");
        exit(EXIT_FAILURE);
    }
    // Right rotation is noop when left child is NULL
    if (node->left == NULL)
    {
        return;
    }
    else
    {
        Node_t* new_root = node->left;

        // New root's parent becomes old root's parent
        new_root->parent = node->parent;
        if (node->parent != NULL)
        {
            // Old root's parent's left child becomes new root if old root was
            // the left child
            if (node->parent->left == node)
            {
                node->parent->left = new_root;
            }
            // Old root's parent's right child becomes new root if old root was
            // the right child
            else
            {
                node->parent->right = new_root;
            }
        }

        // Old root's left child becomes new root's right child
        node->left = new_root->right;
        new_root->right->parent = node;

        // Old root becomes new root's right child
        node->parent = new_root;
        new_root->right = node;

        // Recompute subtree augmentations
        node->height =
            node->left == NULL && node->right == NULL
                ? 0
                : 1 + MAX(node->left == NULL ? 0 : node->left->height,
                          node->right == NULL ? 0 : node->right->height);
        new_root->height =
            new_root->left == NULL && new_root->right == NULL
                ? 0
                : 1 + MAX(new_root->left == NULL ? 0 : new_root->left->height,
                          new_root->right == NULL ? 0
                                                  : new_root->right->height);
        node->size = 1 + (node->left == NULL ? 0 : node->left->size) +
                     (node->right == NULL ? 0 : node->right->size);
        new_root->size = 1 +
                         (new_root->left == NULL ? 0 : new_root->left->size) +
                         (new_root->right == NULL ? 0 : new_root->right->size);
    }
}

static void rotate_left(Node_t* node)
{
    if (node == NULL)
    {
        fprintf(stderr, "Error: node is NULL in rotate_left\n");
        exit(EXIT_FAILURE);
    }
    // Left rotation is noop when right child is NULL
    if (node->right == NULL)
    {
        return;
    }
    else
    {
        Node_t* new_root = node->right;

        // New root's parent becomes old root's parent
        new_root->parent = node->parent;
        if (node->parent != NULL)
        {
            // Old root's parent's left child becomes new root if old root was
            // the left child
            if (node->parent->left == node)
            {
                node->parent->left = new_root;
            }
            // Old root's parent's right child becomes new root if old root was
            // the right child
            else
            {
                node->parent->right = new_root;
            }
        }

        // Old root's right child becomes new root's left child
        node->right = new_root->left;
        new_root->left->parent = node;

        // Old root becomes new root's left child
        node->parent = new_root;
        new_root->left = node;

        // Recompute subtree augmentations
        node->height =
            node->left == NULL && node->right == NULL
                ? 0
                : 1 + MAX(node->left == NULL ? 0 : node->left->height,
                          node->right == NULL ? 0 : node->right->height);
        new_root->height =
            new_root->left == NULL && new_root->right == NULL
                ? 0
                : 1 + MAX(new_root->left == NULL ? 0 : new_root->left->height,
                          new_root->right == NULL ? 0
                                                  : new_root->right->height);
        node->size = 1 + (node->left == NULL ? 0 : node->left->size) +
                     (node->right == NULL ? 0 : node->right->size);
        new_root->size = 1 +
                         (new_root->left == NULL ? 0 : new_root->left->size) +
                         (new_root->right == NULL ? 0 : new_root->right->size);
    }
}

static int skew(Node_t* node)
{
    if (node == NULL)
    {
        fprintf(stderr, "Error: node is NULL in skew\n");
        exit(EXIT_FAILURE);
    }
    return (node->right == NULL ? 0 : node->right->height) -
           (node->left == NULL ? 0 : node->left->height);
}

static void rebalance_at_node(Node_t* node)
{
    if (node == NULL)
    {
        return;
    }
    int skewness = skew(node);
    // Case: Right-heavy subtree
    if (skewness == 2)
    {
        // Subcase: Right-left-heavy subtree two rotations
        if (skew(node->right) == -1)
        {
            rotate_right(node->right);
        }
        rotate_left(node);
    }
    // Case: Left-heavy subtree
    else if (skewness == -2)
    {
        // Subcase: Left-right-heavy subtree requires two rotations
        if (skew(node->left) == 1)
        {
            rotate_left(node->left);
        }
        rotate_right(node);
    }
    // Case: Subtree is balanced, but subtree augmentations must be updated
    else
    {
        node->height =
            (node->left == NULL && node->right == NULL)
                ? 0
                : 1 + MAX(node->left == NULL ? 0 : node->left->height,
                          node->right == NULL ? 0 : node->right->height);
        node->size = 1 + (node->left == NULL ? 0 : node->left->size) +
                     (node->right == NULL ? 0 : node->right->size);
    }
    // Recursively rebalance ancestors
    rebalance_at_node(node->parent);
}

static void free_subtree_at_node(Node_t* node)
{
    if (node != NULL)
    {
        if (node->left != NULL)
        {
            free_subtree_at_node(node->left);
        }
        if (node->right != NULL)
        {
            free_subtree_at_node(node->right);
        }
        free(node);
    }
}

/* Set ADT functions */

AVLTree_t init_avl_tree(int data[], size_t size)
{
    if (data == NULL)
    {
        fprintf(stderr, "Error: data is NULL in init_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    AVLTree_t root = malloc(sizeof(AVLTree_t));
    if (root == NULL)
    {
        fprintf(stderr, "Error: malloc failed in init_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < size; i++)
    {
        insert_avl_tree(root, data[i]);
    }
    return root;
}

void insert_avl_tree(AVLTree_t root, int key)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in insert_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    if (query_avl_tree(root, key))
    {
        fprintf(stdout, "Warning: key %i already exists in tree\n", key);
        return;
    }

    // Create new node
    Node_t* new_node = calloc(1, sizeof(Node_t));
    if (new_node == NULL)
    {
        fprintf(stderr, "Error: calloc failed in insert_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    new_node->key = key;
    new_node->size = 1;

    // Case: Empty tree
    if (*root == NULL)
    {
        *root = new_node;
    }
    // Case: Non-empty tree
    else
    {
        Node_t* node = *root;
        if (new_node->key < node->key)
        {
            if (node->left == NULL)
            {
                node->left = new_node;
                new_node->parent = node;
                rebalance_at_node(node);
            }
            else
            {
                insert_avl_tree(&node->left, key);
            }
        }
        else
        {
            if (node->right == NULL)
            {
                node->right = new_node;
                new_node->parent = node;
                rebalance_at_node(node);
            }
            else
            {
                insert_avl_tree(&node->right, key);
            }
        }
    }
}

void delete_avl_tree(AVLTree_t root, int key)
{
    Node_t* node = find_avl_tree(root, key);
    if (node == NULL)
    {
        fprintf(stdout, "Warning: key %i not found in tree\n", key);
        return;
    }

    int node_key = node->key;

    // Case: Node has a child; swap with predecessor or successor
    if (node->left != NULL || node->right != NULL)
    {
        if (node->left != NULL)
        {
            Node_t* predecessor = find_prev_avl_tree(root, node_key);
            swap_keys(node, predecessor);
            delete_avl_tree(&node->left, node_key);
        }
        else
        {
            Node_t* successor = find_next_avl_tree(root, node_key);
            swap_keys(node, successor);
            delete_avl_tree(&node->right, node_key);
        }
    }
    // Case: Node is non-root leaf; no swapping required, but parent
    // pointers must be updated
    else if (node->parent != NULL)
    {
        if (node->parent->left == node)
        {
            node->parent->left = NULL;
        }
        else
        {
            node->parent->right = NULL;
        }
        rebalance_at_node(node->parent);
        free_subtree_at_node(node);
    }
    // Case: Node is root leaf
    else
    {
        free_subtree_at_node(node);
        *root = NULL;
    }
}

Node_t* find_avl_tree(AVLTree_t root, int key)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in find_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    Node_t* node = *root;
    // Case: Empty tree
    if (node == NULL)
    {
        return NULL;
    }
    // Case: Key found
    else if (node->key == key)
    {
        return node;
    }
    // Case: Recurse left
    else if (key < node->key)
    {
        return find_avl_tree(&node->left, key);
    }
    // Case: Recurse right
    else
    {
        return find_avl_tree(&node->right, key);
    }
}

bool query_avl_tree(AVLTree_t root, int key)
{
    return find_avl_tree(root, key) != NULL;
}

bool is_empty_avl_tree(AVLTree_t root)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in is_empty_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    return *root == NULL;
}

Node_t* find_prev_avl_tree(AVLTree_t root, int key)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in find_prev_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    Node_t* node = *root;
    // Case: Empty tree
    if (node == NULL)
    {
        return NULL;
    }
    // Case: Non-empty tree
    Node_t* parent = node->parent;
    while (node != NULL && node->key != key)
    {
        // Subcase: Recurse left
        if (key < node->key)
        {
            parent = node;
            node = node->left;
        }
        // Subcase: Recurse right
        else
        {
            parent = node;
            node = node->right;
        }
        // Case: Key not found
        if (node == NULL)
        {
            node = parent;
        }
    }
    return predecessor(node);
}

Node_t* find_next_avl_tree(AVLTree_t root, int key)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in find_next_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    Node_t* node = *root;
    // Case: Empty tree
    if (node == NULL)
    {
        return NULL;
    }
    // Case: Non-empty tree
    Node_t* parent = node->parent;
    while (node != NULL && node->key != key)
    {
        // Subcase: Recurse left
        if (key < node->key)
        {
            parent = node;
            node = node->left;
        }
        // Subcase: Recurse right
        else
        {
            parent = node;
            node = node->right;
        }
        // Case: Key not found
        if (node == NULL)
        {
            node = parent;
        }
    }
    return successor(node);
}

Node_t* find_min_avl_tree(AVLTree_t root)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in find_min_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    Node_t* node = *root;
    // Case: Empty tree
    if (node == NULL)
    {
        return NULL;
    }
    // Case: Leftmost node is min
    else if (node->left == NULL)
    {
        return node;
    }
    // Case: Recurse left
    else
    {
        return find_min_avl_tree(&node->left);
    }
}

Node_t* find_max_avl_tree(AVLTree_t root)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in find_min_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    Node_t* node = *root;
    // Case: Empty tree
    if (node == NULL)
    {
        return NULL;
    }
    // Case: Rightmost node is max
    else if (node->right == NULL)
    {
        return node;
    }
    // Case: Recurse right
    else
    {
        return find_max_avl_tree(&node->right);
    }
}

void free_avl_tree(AVLTree_t root)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in free_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    else
    {
        free_subtree_at_node(*root);
    }
}
