from collections import defaultdict

from .DAG_relaxation import find_shortest_path_distances as sssp_dag_relaxation


def find_shortest_path_distances(
    graph: dict[str, list[tuple[str, int]]], start_node: str
) -> dict[str, float | int]:
    distances = {
        node: (float("inf") if node != start_node else 0) for node in graph.keys()
    }
    # If a finite shortest path exists, it will have at most |V| - 1 edges.
    for _ in range(len(graph) - 1):
        for node in graph.keys():
            for neighbor, edge_weight in graph[node]:
                if distances[node] + edge_weight < distances[neighbor]:
                    distances[neighbor] = distances[node] + edge_weight

    # If a negative cycle exists, we can relax the distances one more time.
    for node in graph.keys():
        for neighbor, edge_weight in graph[node]:
            # distances[neighbor] > distances[node] + edge_weight implies
            # that adding another edges relaxes the distance to neighbor,
            # but this would mean the shortest path is at least |V| edges
            # long, which implies the existence of a negative cycle.
            if distances[node] + edge_weight < distances[neighbor]:
                raise ValueError("Negative cycle detected in graph.")

    return distances


def dag_duplication(
    graph: dict[str, list[tuple[str, int]]]
) -> dict[str, list[tuple[str, int]]]:
    dag = defaultdict(list)
    for node in graph.keys():
        for i in range(len(graph)):
            # 0-weight edge from node_i to node_i+1
            dag[f"{node}_{i}"].append((f"{node}_{i+1}", 0))
            # Edges from node_i to neighbors node's neighbors, but in the next level
            for neighbor, edge_weight in graph[node]:
                dag[f"{node}_{i}"].append((f"{neighbor}_{i+1}", edge_weight))
        else:
            # No out edges from node_n (last level)
            for node in graph.keys():
                dag[f"{node}_{len(graph)}"] = []
    return dag


def find_shortest_path_distances_graph_dupe(
    graph: dict[str, list[tuple[str, int]]], start_node: str
) -> dict[str, float | int]:
    # Graph duplication to construct DAG from original graph
    dag = dag_duplication(graph)

    # Run DAG relaxation algorithm on duplicated graph
    dag_distances = sssp_dag_relaxation(dag, f"{start_node}_0")

    # Extract distances from start_node to each node in original graph
    distances = {node: dag_distances[f"{node}_{len(graph)-1}"] for node in graph.keys()}

    # Mark nodes that are reachable from negative cycles
    for node in graph.keys():
        # If a node is witness to a negative cycle, then its distance
        # in the last level of the DAG will be less than its distance in
        # in the penultimate level.
        if dag_distances[f"{node}_{len(graph)}"] < distances[node]:
            distances[node] = float("-inf")

            # All nodes reachable from a (negative cycle) witness node also have
            # indeterminate distances.
            stack = [node]
            while stack:
                node = stack.pop()
                for neighbor, _ in graph[node]:
                    if distances[neighbor] != float("-inf"):
                        distances[neighbor] = float("-inf")
                        stack.append(neighbor)

    return distances
