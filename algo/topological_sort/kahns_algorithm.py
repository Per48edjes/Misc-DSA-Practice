from collections import defaultdict, deque


def topological_sort(graph: dict[str, list[str]]) -> list[str]:
    result, q = [], deque()

    # Maintain in-degree of all vertices
    in_degree = defaultdict(int)
    for node in graph:
        for neighbor in graph[node]:
            in_degree[neighbor] += 1

    # Add all vertices with in-degree 0 to the queue
    for node in graph:
        if in_degree[node] == 0:
            q.append(node)

    while q:
        parentless_node = q.popleft()
        result.append(parentless_node)

        for neighbor in graph[parentless_node]:
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                q.append(neighbor)

    # If there is a cycle, the in-degree of some node will never be 0
    if len(result) != len(graph):
        raise Exception("Cycle detected in graph!")

    return result
