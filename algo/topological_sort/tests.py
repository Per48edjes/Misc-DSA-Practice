import unittest


class TopologicalSortTest(unittest.TestCase):
    def assertTopologicalSortValid(self, graph, result):
        visited = set()
        for node in result:
            visited.add(node)
            for neighbor in graph[node]:
                self.assertNotIn(
                    neighbor, visited, f"Invalid topological sort: {result}"
                )

    def test_kahns_top_sort(self):
        from kahns_algorithm import topological_sort

        # Test Case 1: Simple DAG with 5 nodes
        graph = {"A": ["C", "D"], "B": ["D"], "C": ["E"], "D": ["E"], "E": []}
        result = topological_sort(graph)
        self.assertTopologicalSortValid(graph, result)

        # Test Case 2: DAG with cycles
        graph = {"A": ["B"], "B": ["C"], "C": ["A"]}
        # Since there are cycles, the topological sort should raise an exception
        with self.assertRaises(Exception):
            topological_sort(graph)

        # Test Case 3: Empty graph
        graph = {}
        expected_result = []
        result = topological_sort(graph)
        self.assertTopologicalSortValid(graph, result)
        self.assertEqual(result, expected_result)

        # Test Case 4: Linear graph
        graph = {"A": ["B"], "B": ["C"], "C": ["D"], "D": ["E"], "E": []}
        result = topological_sort(graph)
        self.assertTopologicalSortValid(graph, result)

        # Add more test cases as needed

    def test_reverse_dfs_top_sort(self):
        from reverse_dfs import topological_sort

        # Test Case 1: Simple DAG with 5 nodes
        graph = {"A": ["C", "D"], "B": ["D"], "C": ["E"], "D": ["E"], "E": []}
        result = topological_sort(graph)
        self.assertTopologicalSortValid(graph, result)

        # Test Case 2: DAG with cycles
        graph = {"A": ["B"], "B": ["C"], "C": ["A"]}
        # Since there are cycles, the topological sort should raise an exception
        with self.assertRaises(Exception):
            topological_sort(graph)

        # Test Case 3: Empty graph
        graph = {}
        expected_result = []
        result = topological_sort(graph)
        self.assertTopologicalSortValid(graph, result)
        self.assertEqual(result, expected_result)

        # Test Case 4: Linear graph
        graph = {"A": ["B"], "B": ["C"], "C": ["D"], "D": ["E"], "E": []}
        result = topological_sort(graph)
        self.assertTopologicalSortValid(graph, result)

        # Add more test cases as needed


if __name__ == "__main__":
    unittest.main()
