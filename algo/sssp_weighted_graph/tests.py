import unittest

from parameterized import parameterized

from .bellman_ford_algorithm import find_shortest_path_distances as sssp_bellman_ford
from .bellman_ford_algorithm import (
    find_shortest_path_distances_graph_dupe as sssp_bellman_ford_graph_dupe,
)
from .DAG_relaxation import find_shortest_path_distances as sssp_dag_relaxation
from .dijkstras_algorithm import find_shortest_path_distances as sssp_dijkstra


class DAGRelaxationTest(unittest.TestCase):
    # Test case 1: various DAGs
    def test_shortest_paths_distances(self):
        graph1 = {
            "A": [("B", 5), ("C", 3)],
            "B": [("C", 2), ("D", 6)],
            "C": [("D", 7)],
            "D": [("E", 4)],
            "E": [],
        }
        start_node1 = "A"
        expected_result1 = {"A": 0, "B": 5, "C": 3, "D": 10, "E": 14}
        result1 = sssp_dag_relaxation(graph1, start_node1)
        self.assertEqual(result1, expected_result1)

        graph2 = {
            "A": [("B", 2), ("C", 4)],
            "B": [("C", 1), ("D", 5)],
            "C": [("D", 3)],
            "D": [("E", 2)],
            "E": [],
        }
        start_node2 = "A"
        expected_result2 = {"A": 0, "B": 2, "C": 3, "D": 6, "E": 8}
        result2 = sssp_dag_relaxation(graph2, start_node2)
        self.assertEqual(result2, expected_result2)

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
        result3 = sssp_dag_relaxation(graph3, start_node3)
        self.assertEqual(result3, expected_result3)


class BellmanFordTest(unittest.TestCase):
    # Test case 1: Graph with negative edges
    @parameterized.expand([sssp_bellman_ford, sssp_bellman_ford_graph_dupe])
    def test_shortest_paths_with_negative_edges(self, function_to_test):
        graph1 = {
            "A": [("B", 5), ("C", 3)],
            "B": [("D", -2)],
            "C": [("D", 4)],
            "D": [("E", 6), ("F", 1)],
            "E": [("G", -2)],
            "F": [("G", 3)],
            "G": [],
        }
        start_node1 = "A"
        expected_result1 = {
            "A": 0,
            "B": 5,
            "C": 3,
            "D": 3,
            "E": 9,
            "F": 4,
            "G": 7,
        }
        result1 = function_to_test(graph1, start_node1)
        self.assertEqual(result1, expected_result1)

    # Test case 2: Graph with negative cycle
    @parameterized.expand([sssp_bellman_ford, sssp_bellman_ford_graph_dupe])
    def test_shortest_paths_with_negative_cycle(self, function_to_test):
        graph2 = {
            "A": [("B", 2)],
            "B": [("C", -3)],
            "C": [("D", 4)],
            "D": [("E", -5)],
            "E": [("B", 1)],
        }
        start_node2 = "A"
        expected_result2 = {
            "A": 0,
            "B": float("-inf"),
            "C": float("-inf"),
            "D": float("-inf"),
            "E": float("-inf"),
        }
        try:
            result2 = function_to_test(graph2, start_node2)
            self.assertEqual(result2, expected_result2)
        except Exception as e:
            self.assertIsInstance(e, ValueError)

    # Test case 3: Disconnected graph
    @parameterized.expand([sssp_bellman_ford, sssp_bellman_ford_graph_dupe])
    def test_shortest_paths_with_disconnected_graph(self, function_to_test):
        graph3 = {"A": [("B", 5)], "B": [], "C": [("D", 3)], "D": [("E", 2)], "E": []}
        start_node3 = "A"
        expected_result3 = {
            "A": 0,
            "B": 5,
            "C": float("inf"),
            "D": float("inf"),
            "E": float("inf"),
        }
        result3 = function_to_test(graph3, start_node3)
        self.assertEqual(result3, expected_result3)

    # Test case 4: Single node graph
    @parameterized.expand([sssp_bellman_ford, sssp_bellman_ford_graph_dupe])
    def test_single_node_graph(self, function_to_test):
        graph4 = {"A": []}
        start_node4 = "A"
        expected_result4 = {"A": 0}
        result4 = function_to_test(graph4, start_node4)
        self.assertEqual(result4, expected_result4)

    # Test case 5: Graph with no edges
    @parameterized.expand([sssp_bellman_ford, sssp_bellman_ford_graph_dupe])
    def test_graph_with_no_edges(self, function_to_test):
        graph5 = {
            "A": [],
            "B": [],
            "C": [],
        }
        start_node5 = "A"
        expected_result5 = {"A": 0, "B": float("inf"), "C": float("inf")}
        result5 = function_to_test(graph5, start_node5)
        self.assertEqual(result5, expected_result5)


class DijkstraTest(unittest.TestCase):
    def test_shortest_path(self):
        # Test case 1: Simple graph with positive weights
        graph = {
            "A": [("B", 4), ("C", 2)],
            "B": [("A", 4), ("C", 1), ("D", 5)],
            "C": [("A", 2), ("B", 1), ("D", 8)],
            "D": [("B", 5), ("C", 8)],
        }
        start = "A"
        expected_distances = {"A": 0, "B": 3, "C": 2, "D": 8}

        distances = sssp_dijkstra(graph, start)
        self.assertEqual(distances, expected_distances)

    def test_unreachable_nodes(self):
        # Test case 2: Graph with unreachable nodes
        graph = {"A": [("B", 2), ("C", 3)], "B": [("A", 2)], "C": [("A", 3)]}
        start = "A"
        expected_distances = {"A": 0, "B": 2, "C": 3}

        distances = sssp_dijkstra(graph, start)
        self.assertEqual(distances, expected_distances)

    def test_empty_graph(self):
        # Test case 3: An empty graph
        graph = {"A": []}
        start = "A"
        expected_distances = {"A": 0}

        distances = sssp_dijkstra(graph, start)
        self.assertEqual(distances, expected_distances)

    def test_self_loop(self):
        # Test case 4: Graph with self-loop
        graph = {"A": [("A", 1)]}
        start = "A"
        expected_distances = {"A": 0}

        distances = sssp_dijkstra(graph, start)
        self.assertEqual(distances, expected_distances)


if __name__ == "__main__":
    unittest.main()
