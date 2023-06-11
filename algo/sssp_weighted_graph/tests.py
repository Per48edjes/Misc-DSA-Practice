import unittest

from .DAG_relaxation import find_shortest_path_distances


class ShortestPathsTest(unittest.TestCase):
    def test_shortest_paths_distances(self):
        # Test case 1
        graph1 = {
            "A": [("B", 5), ("C", 3)],
            "B": [("C", 2), ("D", 6)],
            "C": [("D", 7)],
            "D": [("E", 4)],
            "E": [],
        }
        start_node1 = "A"
        expected_result1 = {"A": 0, "B": 5, "C": 3, "D": 10, "E": 14}
        result1 = find_shortest_path_distances(graph1, start_node1)
        self.assertEqual(result1, expected_result1)

        # Test case 2
        graph2 = {
            "A": [("B", 2), ("C", 4)],
            "B": [("C", 1), ("D", 5)],
            "C": [("D", 3)],
            "D": [("E", 2)],
            "E": [],
        }
        start_node2 = "A"
        expected_result2 = {"A": 0, "B": 2, "C": 3, "D": 6, "E": 8}
        result2 = find_shortest_path_distances(graph2, start_node2)
        self.assertEqual(result2, expected_result2)

        # Test case 3
        graph3 = {
            "A": [("B", 5), ("C", 3)],
            "B": [("D", 2)],
            "C": [("D", 4)],
            "D": [("E", 6), ("F", 1)],
            "E": [("G", 2)],
            "F": [("G", 3)],
            "G": [],
        }
        start_node3 = "A"
        expected_result3 = {"A": 0, "B": 5, "C": 3, "D": 7, "E": 13, "F": 8, "G": 11}
        result3 = find_shortest_path_distances(graph3, start_node3)
        self.assertEqual(result3, expected_result3)


if __name__ == "__main__":
    unittest.main()
