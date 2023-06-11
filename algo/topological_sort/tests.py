import unittest

from parameterized import parameterized

from .kahns_algorithm import topological_sort as topological_sort_kahns
from .reverse_dfs import topological_sort as topological_sort_reverse_dfs


class TopologicalSortTest(unittest.TestCase):
    def assertTopologicalSortValid(self, graph, result):
        visited = set()
        for node in result:
            visited.add(node)
            for neighbor in graph[node]:
                self.assertNotIn(
                    neighbor, visited, f"Invalid topological sort: {result}"
                )

    @parameterized.expand(
        [(topological_sort_kahns,), (topological_sort_reverse_dfs,)],
    )
    def test_topological_sort(self, function_to_test):
        # Test Case 1: Simple DAG with 5 nodes
        graph = {"A": ["C", "D"], "B": ["D"], "C": ["E"], "D": ["E"], "E": []}
        result = function_to_test(graph)
        self.assertTopologicalSortValid(graph, result)

        # Test Case 2: DAG with cycles
        graph = {"A": ["B"], "B": ["C"], "C": ["A"]}
        # Since there are cycles, the topological sort should raise an exception
        with self.assertRaises(Exception):
            function_to_test(graph)

        # Test Case 3: Empty graph
        graph = {}
        expected_result = []
        result = function_to_test(graph)
        self.assertTopologicalSortValid(graph, result)
        self.assertEqual(result, expected_result)

        # Test Case 4: Linear graph
        graph = {"A": ["B"], "B": ["C"], "C": ["D"], "D": ["E"], "E": []}
        result = function_to_test(graph)
        self.assertTopologicalSortValid(graph, result)


if __name__ == "__main__":
    unittest.main()
