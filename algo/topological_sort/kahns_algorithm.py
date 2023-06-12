from collections import defaultdict, deque


def topological_sort(dag: dict[str, list[str | tuple[str, int]]]) -> list[str]:
    result, q = [], deque()

    # Maintain in-degree of all vertices
    in_degree = defaultdict(int)
    for node in dag:
        for neighbor in dag[node]:
            if isinstance(neighbor, tuple):
                neighbor = neighbor[0]
            in_degree[neighbor] += 1

    # Add all vertices with in-degree 0 to the queue
    for node in dag:
        if in_degree[node] == 0:
            q.append(node)

    while q:
        parentless_node = q.popleft()
        result.append(parentless_node)

        for neighbor in dag[parentless_node]:
            # Handles the case where the neighbor is a (node, edge_weight) tuple
            if isinstance(neighbor, tuple):
                neighbor = neighbor[0]
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                q.append(neighbor)

    # If there is a cycle, the in-degree of some node will never be 0
    if len(result) != len(dag):
        raise ValueError("Cycle detected in graph!")

    return result
