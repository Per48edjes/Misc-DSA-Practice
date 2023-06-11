from collections import defaultdict, deque


def topological_sort(graph: dict[str, list[str]]) -> list[str]:
    def dfs(node: str) -> None:
        # -1 corresponds to having visited the node in the current connected component
        if visited[node] == -1:
            raise Exception("Cycle detected")

        # 0 corresponds to unvisited
        if visited[node] == 0:
            visited[node] = -1
            for neighbor in graph[node]:
                dfs(neighbor)
            # 1 corresponds to having visited the node and all its neighbors
            visited[node] = 1
            component_result.append(node)

    visited, result = defaultdict(int), deque()
    for node in graph:
        component_result = []
        dfs(node)
        # `extendleft` reverses the order of the nodes in the component
        result.extendleft(component_result)

    return list(result)
