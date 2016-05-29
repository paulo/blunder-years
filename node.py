import networkx as nx
from message import Message

class Node():

    def __init__(self, graph, nx_node, graph_size, node_list, side_size):
        self.G = graph
        self.side_size = side_size
        self.node_id = nx_node
        self.inbox = []
        self.saved_msg = []
        self.graph_size = graph_size
        self.vector_clock = self.init_vector_clock(graph, side_size)
        self.neighbors = graph.neighbors(nx_node)
        self.node_list = node_list

    def print_node_info(self):
        print("Node id:",self.node_id,"VC:",self.vector_clock, "Neighbours:",self.neighbors)

    def deliver_msg(self, msg):
        if msg.is_newer(self.vector_clock):
            if msg not in self.inbox:
                self.inbox.append(msg)

    def init_vector_clock(self, graph, side_size):
        vc = [[0 for i in range(side_size)] for j in range(side_size)]
        return vc

    def broadcast_own_event(self):
        self.vector_clock[self.node_id[0]][self.node_id[1]] += 1
        new_msg = Message(self.node_id, "msg", self.vector_clock)
        broadcast_message(new_msg)

    def broadcast_message(self, msg):
        for x in self.neighbors:
            self.node_list[x[0]][x[1]].deliver_msg(msg)

    def node_work():
        for msg in self.inbox:
            if msg.is_newer(self.vector_clock):
                broadcast_message(msg)
                saved_msg.append(msg)

                #check causality
                    #ver se dá para fazer update

                self.saved_msg
            self.inbox.remove(msg)

    def is_causal():
        for msg in self.saved_msg:
