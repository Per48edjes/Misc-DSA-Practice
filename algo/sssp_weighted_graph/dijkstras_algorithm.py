import heapq


def find_shortest_path_distances(
    graph: dict[str, list[tuple[str, int]]], start_node: str
) -> dict[str, float | int]:
    distances = {
        node: float("inf") if node != start_node else 0 for node in graph.keys()
    }
    # Using a binary heap priority queue to process nodes in order of distance
    q = [(0, start_node)]
    while q:
        distance, node = heapq.heappop(q)
        # This implicitly excludes edges that go against order of nodes
        # in increasing distance from start_node
        if distance > distances[node]:
            continue
        # Attempt to relax edges from node to its neighbors
        for neighbor, edge_weight in graph[node]:
            if distance + edge_weight < distances[neighbor]:
                distances[neighbor] = distance + edge_weight
                heapq.heappush(q, (distances[neighbor], neighbor))
    return distances
