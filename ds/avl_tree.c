#include "avl_tree.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

static int skew(Node_t* node)
{
    if (node == NULL)
    {
        fprintf(stderr, "Error: node is NULL in skew\n");
        exit(EXIT_FAILURE);
    }
    return node->left->height - node->right->height;
}

static bool is_leaf(Node_t* node)
{
    if (node == NULL)
    {
        fprintf(stderr, "Error: node is NULL in is_leaf\n");
        exit(EXIT_FAILURE);
    }
    return node->left == NULL && node->right == NULL;
}

// TODO: Check whether rotations are safe and logically correct
// FIX: Height subtree augmentation is not being updated in rotation functions
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
    }
}

AVLTree_t* init_avl_tree(int data[], int size)
{
    if (data == NULL)
    {
        fprintf(stderr, "Error: data is NULL in init_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    AVLTree_t* tree = malloc(sizeof(AVLTree_t));
    if (tree == NULL)
    {
        fprintf(stderr, "Error: malloc failed in init_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    tree->root = NULL;
    for (int i = 0; i < size; i++)
    {
        insert_avl_tree(tree, data[i]);
    }
    return tree;
}

// TODO: Impletment dynamic operations
void insert_avl_tree(AVLTree_t* tree, int key)
{
    if (tree == NULL)
    {
        fprintf(stderr, "Error: tree is NULL in insert_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    // Create new node containing key
    Node_t* new_node = calloc(1, sizeof(Node_t));
    if (new_node == NULL)
    {
        fprintf(stderr, "Error: calloc failed in insert_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    new_node->key = key;
    // TODO: Finish implementing rest of insert function
}

int delete_avl_tree(AVLTree_t* tree, int key);

bool query_avl_tree(AVLTree_t* tree, int key)
{
    if (tree == NULL)
    {
        fprintf(stderr, "Error: tree is NULL in query_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    Node_t* node = tree->root;
    while (node != NULL)
    {
        if (node->key == key)
        {
            return true;
        }
        // NOTE: Need to decide strictness of comparison
        else if (node->key > key)
        {
            node = node->left;
        }
        else
        {
            node = node->right;
        }
    }
    return false;
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

void free_avl_tree(AVLTree_t* tree)
{
    if (tree == NULL)
    {
        fprintf(stderr, "Error: tree is NULL in free_avl_tree\n");
        exit(EXIT_FAILURE);
    }
    if (tree->root == NULL)
    {
        free(tree);
        return;
    }
    else
    {
        free_subtree_at_node(tree->root);
        free(tree);
    }
}
