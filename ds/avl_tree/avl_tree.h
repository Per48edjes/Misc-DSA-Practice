/* This module implements the Set ADT using an AVL tree. */
/*
 * The AVL tree is a self-balancing binary search tree, i.e.,
 * the following two invariants are maintained:
 *   - AVL Property: For every node in the tree, the heights of its
 *     left and right subtrees differ by at most 1.
 *   - BST Property: For every node in the tree, the keys in its left
 *     subtree are less than its key, and the keys in its right subtree
 *     are greater than its key.
 *
 * No duplicate keys are allowed; if a key is inserted that is already
 * in the tree, the insertion operation will noop. Similarly, deleting a key
 * that is not in the tree will noop.
 *
 * If the tree is empty, the root Node_t* is NULL.
 */

#pragma once

#define MAX(a, b) ((a) > (b) ? (a) : (b))

#include <stdbool.h>
#include <stddef.h>

typedef struct Node
{
    int key;
    struct Node* left;
    struct Node* right;
    struct Node* parent;
    // Subtree augmentations
    int height;
    int size;
} Node_t;

// AVLTree_t recursively takes ownership of all nodes in the tree
typedef struct Node** AVLTree_t;

/* Initialization and destruction of subtree rooted at `root` */
AVLTree_t init_avl_tree(int data[], size_t size);
void free_avl_tree(AVLTree_t root);

/* Search operations in subtree rooted at `root` */
bool is_empty_avl_tree(AVLTree_t root);
bool query_avl_tree(AVLTree_t root, int key);

Node_t* find_min_avl_tree(AVLTree_t root);
Node_t* find_max_avl_tree(AVLTree_t root);
Node_t* find_avl_tree(AVLTree_t root, int key);
Node_t* find_prev_avl_tree(AVLTree_t root, int key);
Node_t* find_next_avl_tree(AVLTree_t root, int key);

/* Dynamic operations on subtree rooted at `root` */
void insert_avl_tree(AVLTree_t root, int key);
void delete_avl_tree(AVLTree_t root, int key);
