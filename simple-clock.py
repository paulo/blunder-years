import networkx as nx
import matplotlib.pyplot as plt
from message import Message
from node import Node

def init_graph(size):
    graph = nx.grid_2d_graph(size,size)
    graph_size = NODE_NMR*NODE_NMR
    nodes = [[0 for x in range(NODE_NMR)] for y in range(NODE_NMR)]
    for x in nx.nodes(graph):
        nodes[x[0]][x[1]] = Node(graph, x, graph_size, nodes, NODE_NMR)
    return (graph, nodes)

def start_broadcast(node_nmr):
    node_list[node_nmr[0]][node_nmr[1]].broadcast_own_event()
    work()
    for x in node_list:
        for y in x:
            print(y.node_id, y.inbox)

def draw_graph(graph):
    pos=nx.spring_layout(graph,iterations=100)
    nx.draw_spectral(graph,node_size=200,node_color='r')
    plt.show()

def get_event():
    #node = input("Node id: ")
    node = (0,0)
    start_broadcast(node)

def work():
    is_working = True
    while is_working:
        for x in node_list:
            for y in x:
                is_working = is_working and y.node_work()


NODE_NMR = 3
graph,node_list = init_graph(NODE_NMR)
get_event()
#draw_graph(graph)
