#include "avl_tree.h"
#include <stdio.h>
#include <stdlib.h>

/* Helper functions */

static Node_t* init_node(int key)
{
    Node_t* node = calloc(1, sizeof(Node_t));
    if (node == NULL)
    {
        fprintf(stderr, "Error: calloc failed in init_node\n");
        exit(EXIT_FAILURE);
    }
    node->key = key;
    node->height = 0;
    node->size = 1;
    return node;
}

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
    // Case: Left child exists; predecessor is lower in the tree
    if (node->left != NULL)
    {
        return find_max_avl_tree(&node->left);
    }
    // Case: Left child does not exist; predecessor is higher in the tree (if it
    // exists)
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
    // Case: Right child exists; successor is lower in the tree
    if (node->right != NULL)
    {
        return find_min_avl_tree(&node->right);
    }
    // Case: Right child does not exist; successor is higher in the tree (if
    // it exists)
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

static void update_subtree_augmentations(Node_t* node)
{
    if (node == NULL)
    {
        return;
    }
    node->height = (node->left == NULL && node->right == NULL)
                       ? 0
                       : 1 + MAX(node->left == NULL ? 0 : node->left->height,
                                 node->right == NULL ? 0 : node->right->height);
    node->size = 1 + (node->left == NULL ? 0 : node->left->size) +
                 (node->right == NULL ? 0 : node->right->size);
    // Recursively update ancestors' subtree augmentations
    update_subtree_augmentations(node->parent);
}

static AVLTree_t rotate_right(AVLTree_t root, Node_t* node)
{
    if (node == NULL)
    {
        fprintf(stderr, "Error: node is NULL in rotate_right\n");
        exit(EXIT_FAILURE);
    }
    // Right rotation is noop when left child is NULL
    if (node->left == NULL)
    {
        return root;
    }
    else
    {
        Node_t* new_subtree_root = node->left;

        // New root's parent becomes old root's parent
        new_subtree_root->parent = node->parent;
        if (node->parent != NULL)
        {
            // Old root's parent's left child becomes new root if old root was
            // the left child
            if (node->parent->left == node)
            {
                node->parent->left = new_subtree_root;
            }
            // Old root's parent's right child becomes new root if old root was
            // the right child
            else
            {
                node->parent->right = new_subtree_root;
            }
        }
        else
        {
            *root = new_subtree_root;
        }

        // Old root's left child becomes new root's right child
        node->left = new_subtree_root->right;
        if (new_subtree_root->right != NULL)
        {
            new_subtree_root->right->parent = node;
        }

        // Old root becomes new root's right child
        node->parent = new_subtree_root;
        new_subtree_root->right = node;

        // Recompute subtree augmentations
        update_subtree_augmentations(node);
    }
    return root;
}

static AVLTree_t rotate_left(AVLTree_t root, Node_t* node)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in rotate_left\n");
        exit(EXIT_FAILURE);
    }
    if (node == NULL)
    {
        fprintf(stderr, "Error: node is NULL in rotate_left\n");
        exit(EXIT_FAILURE);
    }
    // Left rotation is noop when right child is NULL
    if (node->right == NULL)
    {
        return root;
    }
    else
    {
        Node_t* new_subtree_root = node->right;

        // New root's parent becomes old root's parent
        new_subtree_root->parent = node->parent;
        if (node->parent != NULL)
        {
            // Old root's parent's left child becomes new root if old root was
            // the left child
            if (node->parent->left == node)
            {
                node->parent->left = new_subtree_root;
            }
            // Old root's parent's right child becomes new root if old root was
            // the right child
            else
            {
                node->parent->right = new_subtree_root;
            }
        }
        else
        {
            *root = new_subtree_root;
        }

        // Old root's right child becomes new root's left child
        node->right = new_subtree_root->left;
        if (new_subtree_root->left != NULL)
        {
            new_subtree_root->left->parent = node;
        }

        // Old root becomes new root's left child
        node->parent = new_subtree_root;
        new_subtree_root->left = node;

        // Recompute subtree augmentations
        update_subtree_augmentations(node);
    }
    return root;
}

static int skew(Node_t* node)
{
    if (node == NULL)
    {
        fprintf(stderr, "Error: node is NULL in skew\n");
        exit(EXIT_FAILURE);
    }
    return (node->right == NULL ? -1 : node->right->height) -
           (node->left == NULL ? -1 : node->left->height);
}

static AVLTree_t rebalance_at_node(AVLTree_t root, Node_t* node)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in rebalance_at_node\n");
        exit(EXIT_FAILURE);
    }
    if (node == NULL)
    {
        return root;
    }
    int skewness = skew(node);
    // Case: Right-heavy subtree
    if (skewness == 2)
    {
        // Subcase: Right-left-heavy subtree two rotations
        if (skew(node->right) == -1)
        {
            root = rotate_right(root, node->right);
        }
        root = rotate_left(root, node);
    }
    // Case: Left-heavy subtree
    else if (skewness == -2)
    {
        // Subcase: Left-right-heavy subtree requires two rotations
        if (skew(node->left) == 1)
        {
            root = rotate_left(root, node->left);
        }
        root = rotate_right(root, node);
    }
    // Recursively rebalance ancestors
    return rebalance_at_node(root, node->parent);
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

    *root = NULL;
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

    Node_t* node = *root;

    // Case: Empty tree
    if (node == NULL)
    {
        Node_t* new_node = init_node(key);
        *root = new_node;
        return;
    }
    // Case: Non-empty tree
    do
    {
        if (key < node->key)
        {
            if (node->left == NULL)
            {
                Node_t* new_node = init_node(key);
                node->left = new_node;
                new_node->parent = node;
                update_subtree_augmentations(new_node);
                *root = *rebalance_at_node(root, new_node);
                return;
            }
            else
            {
                node = node->left;
            }
        }
        else
        {
            if (node->right == NULL)
            {
                Node_t* new_node = init_node(key);
                node->right = new_node;
                new_node->parent = node;
                update_subtree_augmentations(new_node);
                *root = *rebalance_at_node(root, new_node);
                return;
            }
            else
            {
                node = node->right;
            }
        }
    } while (true);
}

void delete_avl_tree(AVLTree_t root, int key)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in find_avl_tree\n");
        exit(EXIT_FAILURE);
    }

    if (!query_avl_tree(root, key))
    {
        fprintf(stdout, "Warning: key %i not found in tree\n", key);
        return;
    }
    // Case: Node has a child (is not a leaf)
    Node_t* node = find_avl_tree(root, key);
    while (node->left != NULL || node->right != NULL)
    {
        if (node->left != NULL)
        {
            Node_t* prev = predecessor(node);
            swap_keys(node, prev);
            node = prev;
        }
        else
        {
            Node_t* next = successor(node);
            swap_keys(node, next);
            node = next;
        }
    }
    // Case: Node is non-root leaf; no swapping required, but parent
    // pointers must be updated
    if (node->parent != NULL)
    {
        if (node->parent->left == node)
        {
            node->parent->left = NULL;
        }
        else
        {
            node->parent->right = NULL;
        }
        update_subtree_augmentations(node->parent);
        *root = *rebalance_at_node(root, node->parent);
        free_subtree_at_node(node);
    }
    // Case: Node is root leaf
    else
    {
        *root = NULL;
        free_subtree_at_node(node);
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
    if (query_avl_tree(root, key))
    {
        return predecessor(find_avl_tree(root, key));
    }
    Node_t *min_node, *max_node;
    if (key < (min_node = find_min_avl_tree(root))->key ||
        is_empty_avl_tree(root))
    {
        return NULL;
    }
    if (key > (max_node = find_max_avl_tree(root))->key)
    {
        return max_node;
    }
    Node_t* node = *root;
    while (node->left != NULL || node->right != NULL)
    {
        if (key < node->key)
        {
            node = node->left;
        }
        else
        {
            node = node->right;
        }
    }
    return (key > node->key) ? node : successor(node);
}

Node_t* find_next_avl_tree(AVLTree_t root, int key)
{
    if (root == NULL)
    {
        fprintf(stderr, "Error: root is NULL in find_next_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    if (query_avl_tree(root, key))
    {
        return successor(find_avl_tree(root, key));
    }
    Node_t *min_node, *max_node;
    if (key > (max_node = find_max_avl_tree(root))->key ||
        is_empty_avl_tree(root))
    {
        return NULL;
    }
    if (key < (min_node = find_min_avl_tree(root))->key)
    {
        return min_node;
    }
    Node_t* node = *root;
    while (node->left != NULL || node->right != NULL)
    {
        if (key < node->key)
        {
            node = node->left;
        }
        else
        {
            node = node->right;
        }
    }
    return (key < node->key) ? node : successor(node);
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
    free(root);
}
