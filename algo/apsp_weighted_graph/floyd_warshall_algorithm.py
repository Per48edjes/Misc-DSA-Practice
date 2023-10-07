def find_all_pairs_shortest_paths(
    # NOTE: This implemenation relies on a graph represented as an adjacency matrix
    graph: list[list[int]],
) -> list[list[int | float]]:
    x = [
        [
            [
                # Base cases: use adjacency matrix entries when k == 0
                graph[u][v] if k == 0 else None
                for k in range(len(graph))
            ]
            for v in range(len(graph))
        ]
        for u in range(len(graph))
    ]
    for k in range(1, len(graph)):
        for u in range(len(graph)):
            for v in range(len(graph)):
                # Recurrence relation
                x[u][v][k] = min(x[u][v][k - 1], x[u][k][k - 1] + x[k][v][k - 1])

    distances = [
        [
            # NOTE: Return negative infinity if, for any vertex, the calculated shortest
            #       path to itself is negative
            x[u][v][len(graph) - 1] if x[u][v][len(graph) - 1] >= 0 else float("-inf")
            for v in range(len(graph))
        ]
        for u in range(len(graph))
    ]
    return distances
