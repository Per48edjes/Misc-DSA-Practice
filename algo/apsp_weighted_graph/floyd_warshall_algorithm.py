def find_all_pairs_shortest_paths(
    # NOTE: This implemenation relies on a graph represented as an adjacency matrix
    graph: list[list[int]],
) -> list[list[int | float]] | None:
    # Distances memo table
    x = [[float("inf") for _ in range(len(graph))] for _ in range(len(graph))]

    # Base cases (k == 0)
    for u in range(len(graph)):
        for v in range(len(graph)):
            if u == v:
                x[u][v] = 0
            elif graph[u][v] != float("inf"):
                x[u][v] = graph[u][v]

    # Recursive cases (1 <= k <= |V|)
    for k in range(len(graph)):
        for u in range(len(graph)):
            for v in range(len(graph)):
                # Recurrence relation
                x[u][v] = min(x[u][v], x[u][k] + x[k][v])

    # Detect negative weight cycles
    if any(x[u][u] < 0 for u in range(len(graph))):
        return None
    return x
