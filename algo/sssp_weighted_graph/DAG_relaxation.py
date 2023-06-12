from topological_sort.reverse_dfs import topological_sort


def find_shortest_path_distances(
    dag: dict[str, list[str | tuple[str, int]]], start_node: str
) -> dict[str, float | int]:
    distances = {
        node: (float("inf") if node != start_node else 0) for node in dag.keys()
    }

    # Topological sort ensures that we visit each node before its neighbors,
    # which implies each edge is relaxed exactly once.
    top_sorted_nodes = topological_sort(dag)
    for node in top_sorted_nodes:
        for neighbor, edge_weight in dag[node]:
            if distances[node] + edge_weight < distances[neighbor]:
                distances[neighbor] = distances[node] + edge_weight

    return distances
