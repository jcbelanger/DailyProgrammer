#solution to https://www.reddit.com/r/dailyprogrammer_ideas/comments/qq4ssg/find_the_minimum_number_of_walls_to_add_such_that/

import matplotlib.pyplot as plt
import networkx as nx
import sys

def main():
    grid = read_input()
    G = make_graph(grid)
    print_graph(G)
    A, B = find_nodes(G, ['A', 'B'])
    F = make_vertex_flow(G)
    soln = vertex_cuts(F, A, B)
    print_soln(grid, *soln)

def print_soln(grid, result, cut_set):
    print(f'result = {result}')
    for row, col in cut_set:
        grid[row][col] = '@'
    for line in grid:
        print(''.join(line))

def read_input():
    lines = sys.stdin.readlines()
    return list(list(line.strip()) for line in lines[1:])

def vertex_cuts(F, s, t):
    try:
        cut, (reachable, not_reachable) = nx.minimum_cut(F, (s, 'out'), (t, 'in'))
        return cut, [u[0] for u in reachable for v in F[u] if v in not_reachable]
    except nx.exception.NetworkXUnbounded:
        return -1, []

def make_vertex_flow(G):
    '''
    Standard max flow/min cut algs work on edges, not verticies.
    We transpose each vertex in G into an edge that can be selected.
    We can force the algorithm to select a vertex edge by making it the
    only path any incoming edges must take to reach an outgoing edge.
    This generated edge is an in/out pair that is connected to corresponding
    in/out edges of the incident verticies.  Only vertex in/out pairs have
    a capacity while others have infinite capacity to prevent them from
    being selected.
    '''
    flow = nx.DiGraph()
    for v in G:
        v_in = (v, 'in')
        v_out = (v, 'out')
        flow.add_edge(v_in, v_out, capacity=1)
        for t in G[v]:
            t_in = (t, 'in')
            flow.add_edge(v_out, t_in) #capacity is infinte
        for s, _ in G.in_edges(v):
            s_out = (s, 'out')
            flow.add_edge(s_out, v_in) #capacity is infinte
    return flow


def find_nodes(G, needles):
    for needle in iter(needles):
        for node, attrs in G.nodes(data=True):
            if attrs['val'] == needle:
                yield node

def make_graph(grid):
    rows = len(grid)
    cols = len(grid[0])
    G = nx.DiGraph()
    for row in range(rows):
        for col in range(cols):
            if grid[row][col] == '#':
                continue
            G.add_node((row, col), val=grid[row][col])
            for dx, dy in [(0,1), (0,-1), (1,0), (-1,0)]:
                x, y = col + dx, row + dy
                if x not in range(cols) or y not in range(rows):
                    continue
                if grid[y][x] == '#':
                    continue
                G.add_edge((row, col), (y, x))
    return G

def print_graph(G):
    nx.draw(G,
        labels=nx.get_node_attributes(G, 'val'),
        with_labels=True,
        font_weight='bold',
        node_size=1000
    )
    plt.show()


if __name__ == '__main__':
    main()
