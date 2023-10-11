#include "avl_tree.h"
#include "test-framework/unity.h"

void test_is_empty_avl_tree(void);
void test_two_node_avl_tree_shape(void);
void test_three_node_avl_tree_shape(void);
void test_three_node_avl_tree_shape_after_single_delete(void);
void test_full_avl_tree_find_min_max(void);
void test_full_avl_tree_shape_after_multiple_deletes(void);

void setUp(void) {}

void test_is_empty_avl_tree(void)
{
    int data[2] = {0, 1};
    AVLTree_t tree = init_avl_tree(data, 2);
    TEST_ASSERT_FALSE(is_empty_avl_tree(tree));
    free_avl_tree(tree);

    Node_t* root = NULL;
    TEST_ASSERT_TRUE(is_empty_avl_tree(&root));
}

void test_two_node_avl_tree_shape(void)
{
    int data[2] = {0, 1};
    AVLTree_t tree = init_avl_tree(data, 2);
    Node_t* root = *tree;

    // Tree structure
    TEST_ASSERT_NOT_NULL(root);
    TEST_ASSERT_EQUAL(0, root->key);
    TEST_ASSERT_NOT_NULL(root->right);
    TEST_ASSERT_EQUAL(1, root->right->key);
    TEST_ASSERT_EQUAL(NULL, root->right->left);
    TEST_ASSERT_EQUAL(NULL, root->right->right);
    TEST_ASSERT_EQUAL(root, root->right->parent);

    // Subtree augmentations
    TEST_ASSERT_EQUAL(0, root->right->height);
    TEST_ASSERT_EQUAL(1, root->right->size);
    TEST_ASSERT_EQUAL(1, root->height);
    TEST_ASSERT_EQUAL(2, root->size);

    free_avl_tree(tree);
}

void test_three_node_avl_tree_shape(void)
{
    int data[3] = {0, 1, 2};
    AVLTree_t tree = init_avl_tree(data, 3);
    Node_t* root = *tree;

    // Subtree augmentations
    TEST_ASSERT_EQUAL(1, root->height);
    TEST_ASSERT_EQUAL(3, root->size);
    TEST_ASSERT_EQUAL(0, root->right->height);
    TEST_ASSERT_EQUAL(1, root->right->size);
    TEST_ASSERT_EQUAL(0, root->left->height);
    TEST_ASSERT_EQUAL(1, root->left->size);

    // Tree structure
    TEST_ASSERT_NOT_NULL(root);
    TEST_ASSERT_EQUAL(1, root->key);
    TEST_ASSERT_NOT_NULL(root->right);
    TEST_ASSERT_NOT_NULL(root->left);
    TEST_ASSERT_EQUAL(2, root->right->key);
    TEST_ASSERT_EQUAL(0, root->left->key);
    TEST_ASSERT_EQUAL(NULL, root->right->left);
    TEST_ASSERT_EQUAL(NULL, root->right->right);
    TEST_ASSERT_EQUAL(NULL, root->right->left);
    TEST_ASSERT_EQUAL(NULL, root->left->right);
    TEST_ASSERT_EQUAL(NULL, root->left->left);

    free_avl_tree(tree);
}

void test_three_node_avl_tree_shape_after_single_delete(void)
{
    int data[4] = {0, 1, 2, 3};
    AVLTree_t tree = init_avl_tree(data, 4);
    Node_t* root = *tree;

    delete_avl_tree(tree, 2);

    // Subtree augmentations
    TEST_ASSERT_EQUAL(1, root->height);
    TEST_ASSERT_EQUAL(3, root->size);
    TEST_ASSERT_EQUAL(0, root->right->height);
    TEST_ASSERT_EQUAL(1, root->right->size);
    TEST_ASSERT_EQUAL(0, root->left->height);
    TEST_ASSERT_EQUAL(1, root->left->size);

    // Tree structure
    TEST_ASSERT_NOT_NULL(root);
    TEST_ASSERT_EQUAL(1, root->key);
    TEST_ASSERT_NOT_NULL(root->right);
    TEST_ASSERT_NOT_NULL(root->left);
    TEST_ASSERT_EQUAL(3, root->right->key);
    TEST_ASSERT_EQUAL(0, root->left->key);
    TEST_ASSERT_EQUAL(NULL, root->right->left);
    TEST_ASSERT_EQUAL(NULL, root->right->right);
    TEST_ASSERT_EQUAL(NULL, root->right->left);
    TEST_ASSERT_EQUAL(NULL, root->left->right);
    TEST_ASSERT_EQUAL(NULL, root->left->left);

    free_avl_tree(tree);
}

void test_full_avl_tree_find_min_max(void)
{
    int data[7] = {100, 7, 8, 1, 5, 2, 3};
    AVLTree_t tree = init_avl_tree(data, 7);
    Node_t* root = *tree;

    // Tree structure
    TEST_ASSERT_EQUAL(5, root->key);
    TEST_ASSERT_EQUAL(2, root->left->key);
    TEST_ASSERT_EQUAL(8, root->right->key);

    // Subtree augmentations
    TEST_ASSERT_EQUAL(2, root->height);
    TEST_ASSERT_EQUAL(7, root->size);

    // Find operations
    TEST_ASSERT_EQUAL(100, find_max_avl_tree(tree)->key);
    TEST_ASSERT_EQUAL(1, find_min_avl_tree(tree)->key);
    TEST_ASSERT_EQUAL(3, find_prev_avl_tree(tree, 4)->key);
    TEST_ASSERT_EQUAL(3, find_prev_avl_tree(tree, 5)->key);
    TEST_ASSERT_EQUAL(find_max_avl_tree(tree)->key,
                      find_prev_avl_tree(tree, INT_MAX)->key);
    TEST_ASSERT_EQUAL(NULL, find_prev_avl_tree(tree, 1));
    TEST_ASSERT_EQUAL(NULL, find_prev_avl_tree(tree, INT_MIN));
    TEST_ASSERT_EQUAL(100, find_next_avl_tree(tree, 98)->key);
    TEST_ASSERT_EQUAL(5, find_next_avl_tree(tree, 4)->key);
    TEST_ASSERT_EQUAL(find_min_avl_tree(tree)->key,
                      find_next_avl_tree(tree, INT_MIN)->key);
    TEST_ASSERT_EQUAL(NULL, find_next_avl_tree(tree, 100));
    TEST_ASSERT_EQUAL(NULL, find_next_avl_tree(tree, INT_MAX));

    free_avl_tree(tree);
}

void test_full_avl_tree_shape_after_multiple_deletes(void)
{
    int data[7] = {100, 7, 8, 1, 5, 2, 3};
    AVLTree_t tree = init_avl_tree(data, 7);

    // Tree structure (after arbitrary delete operations)
    delete_avl_tree(tree, 100);
    TEST_ASSERT_EQUAL(8, find_max_avl_tree(tree)->key);
    delete_avl_tree(tree, 5);
    TEST_ASSERT_EQUAL(3, (*tree)->key);

    free_avl_tree(tree);
}

void tearDown(void) {}

int main(void)
{
    UNITY_BEGIN();
    RUN_TEST(test_is_empty_avl_tree);
    RUN_TEST(test_two_node_avl_tree_shape);
    RUN_TEST(test_three_node_avl_tree_shape);
    RUN_TEST(test_three_node_avl_tree_shape_after_single_delete);
    RUN_TEST(test_full_avl_tree_find_min_max);
    RUN_TEST(test_full_avl_tree_shape_after_multiple_deletes);
    return UNITY_END();
}
