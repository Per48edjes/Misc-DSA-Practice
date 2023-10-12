import unittest

from floyd_warshall_algorithm import find_all_pairs_shortest_paths


class TestAllPairsShortestPaths(unittest.TestCase):
    def test_empty_graph(self):
        graph = []
        result = find_all_pairs_shortest_paths(graph)
        self.assertEqual(result, [])

    def test_single_node_graph(self):
        graph = [[0]]
        result = find_all_pairs_shortest_paths(graph)
        self.assertEqual(result, [[0]])

    def test_disconnected_graph(self):
        graph = [
            [0, float("inf"), float("inf")],
            [float("inf"), 0, float("inf")],
            [float("inf"), float("inf"), 0],
        ]
        result = find_all_pairs_shortest_paths(graph)
        self.assertEqual(
            result,
            [
                [0, float("inf"), float("inf")],
                [float("inf"), 0, float("inf")],
                [float("inf"), float("inf"), 0],
            ],
        )

    def test_positive_weight_shortest_path(self):
        graph = graph = [
            [0, 2, float("inf"), 11],
            [float("inf"), 0, 3, float("inf")],
            [float("inf"), float("inf"), 0, 5],
            [float("inf"), float("inf"), float("inf"), 0],
        ]
        result = find_all_pairs_shortest_paths(graph)
        self.assertEqual(
            result,
            [
                [0, 2, 5, 10],
                [float("inf"), 0, 3, 8],
                [float("inf"), float("inf"), 0, 5],
                [float("inf"), float("inf"), float("inf"), 0],
            ],
        )

    def test_positive_weight_cycles(self):
        graph = graph = [
            [0, 3, float("inf"), 5],
            [2, 0, float("inf"), 4],
            [float("inf"), 1, 0, float("inf")],
            [float("inf"), float("inf"), 2, 0],
        ]
        result = find_all_pairs_shortest_paths(graph)
        self.assertEqual(
            result,
            [
                [0, 3, 7, 5],
                [2, 0, 6, 4],
                [3, 1, 0, 5],
                [5, 3, 2, 0],
            ],
        )

    def test_negative_weights(self):
        graph = [
            [0, float("inf"), -2, float("inf")],
            [4, 0, 3, float("inf")],
            [float("inf"), float("inf"), 0, 2],
            [float("inf"), -1, float("inf"), 0],
        ]
        result = find_all_pairs_shortest_paths(graph)
        self.assertEqual(
            result,
            [
                [0, -1, -2, 0],
                [4, 0, 2, 4],
                [5, 1, 0, 2],
                [3, -1, 1, 0],
            ],
        )

    def test_negative_weight_cycles(self):
        graph = [
            [0, 2, 5, float("inf"), float("inf"), float("inf"), 10],
            [float("inf"), 0, 2, float("inf"), 11, float("inf"), float("inf")],
            [
                float("inf"),
                float("inf"),
                0,
                float("inf"),
                float("inf"),
                float("inf"),
                2,
            ],
            [
                float("inf"),
                float("inf"),
                float("inf"),
                0,
                float("inf"),
                float("inf"),
                float("inf"),
            ],
            [
                float("inf"),
                float("inf"),
                float("inf"),
                float("inf"),
                0,
                1,
                float("inf"),
            ],
            [
                float("inf"),
                float("inf"),
                float("inf"),
                float("inf"),
                -2,
                0,
                float("inf"),
            ],
            [
                float("inf"),
                float("inf"),
                float("inf"),
                float("inf"),
                float("inf"),
                11,
                0,
            ],
        ]
        result = find_all_pairs_shortest_paths(graph)
        self.assertIsNone(result)


if __name__ == "__main__":
    unittest.main()
