#pragma once

#include <stdbool.h>

/* This module implements the Set ADT using an AVL tree. */

typedef struct Node
{
    int key;
    struct Node* left;
    struct Node* right;
    struct Node* parent;
    int height;
} Node_t;

// NOTE: Can just make Node_t** AVL_Tree_t given recursive nature of the data
// structure?
typedef struct AVLTree
{
    Node_t* root;
} AVLTree_t;

// Initialization and destruction of tree
AVLTree_t* init_avl_tree(int data[], int size);
void free_avl_tree(AVLTree_t* tree);

// Search operations on tree
bool query_avl_tree(AVLTree_t* tree, int key);
int find_min_avl_tree(AVLTree_t* tree);
int find_max_avl_tree(AVLTree_t* tree);
int find_avl_tree(AVLTree_t* tree, int key);
int find_prev_avl_tree(AVLTree_t* tree, int key);
int find_next_avl_tree(AVLTree_t* tree, int key);

// Dynamic operations
void insert_avl_tree(AVLTree_t* tree, int key);
int delete_avl_tree(AVLTree_t* tree, int key);
