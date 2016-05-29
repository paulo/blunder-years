import networkx as nx
import matplotlib.pyplot as plt
from message import Message
from node import Node
from ast import literal_eval
import sys

GRID_SIDE_SIZE = int(sys.argv[1])

#create the graph
def init_graph(size):
    graph = nx.grid_2d_graph(size,size)
    graph_size = size * size
    nodes_objs = [[0 for x in range(size)] for y in range(size)]
    #create Node object for each node
    for x in nx.nodes(graph):
        nodes_objs[x[0]][x[1]] = Node(graph, x, graph_size, nodes_objs, size)
    return (graph, nodes_objs)

#create event on the given node
def start_broadcast(node_nmr):
    node_list[node_nmr[0]][node_nmr[1]].broadcast_own_event()

#draw the graph
def draw_graph(graph):
    pos=nx.spring_layout(graph,iterations=100)
    nx.draw_spectral(graph,node_size=200,node_color='r')
    plt.show()

#get node where the event is going to occur from user input (format (x,y))
def get_event():
    while True:
        nodes = input("Node id: ")
        nodes_info = nodes.split(' ')
        for node in nodes_info:
            node = literal_eval(node)
            start_broadcast(node)
        work()

#process event for each node, while nodes have messages in their inbox
def work():
    nodes_have_msgs = True
    while nodes_have_msgs:
        nodes_have_msgs = False
        for x in node_list:
            for y in x:
                nodes_have_msgs = nodes_have_msgs or y.node_work()
                #input("Press enter for next iteration.")
    for x in node_list:
        for y in x:
            y.print_vector_clock_matrix()

graph,node_list = init_graph(GRID_SIDE_SIZE)
get_event()
