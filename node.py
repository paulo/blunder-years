import networkx as nx
from message import Message
import copy

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

    #puts msg in the node inbox if it's newer than the ones already processed
    #or if it's not already there, ignores it otherwise
    def deliver_msg(self, msg):
        if msg.is_newer(self.vector_clock):
            if msg not in self.inbox and msg not in self.saved_msg:
                self.inbox.append(msg)

    #initializes vector clock with 0 value for every process
    def init_vector_clock(self, graph, side_size):
        vc = [[0 for i in range(side_size)] for j in range(side_size)]
        return vc

    #creates local node event and broadcasts it
    def broadcast_own_event(self):
        self.vector_clock[self.node_id[0]][self.node_id[1]] += 1
        new_msg = Message(self.node_id, "msg", copy.deepcopy(self.vector_clock))
        self.broadcast_message(new_msg)

    #puts given message in the neighbor nodes inbox
    def broadcast_message(self, msg):
        for x in self.neighbors:
            print('({0},{1}) SENDS to ({2},{3})'.format(self.node_id[0], self.node_id[1],x[0],x[1]))
            self.node_list[x[0]][x[1]].deliver_msg(msg)

    #processes messages in the node inbox
    def node_work(self):
        #if there's not messages to process, return false
        if not self.inbox:
            return False
        #if self.inbox:
        for msg in self.inbox:
            #print('msg from: ({0},{1})'.format(msg.node_id[0], msg.node_id[1]))
            #immediately broadcasts message to neighbors
            self.broadcast_message(msg)
            #if message has causality...
            if self.is_causal(msg):
                print('({0},{1}) RECEIVES from ({2},{3}): CAUSAL'.format(self.node_id[0], self.node_id[1],msg.node_id[0], msg.node_id[1]))
                #update own vector clock with the message vector clock
                self.update_vector_clock(msg)
                still_causal = True
                #for each message in the saved messages (messages that didn't had causality)
                #check if it has causality and own vector clock in that case
                while still_causal:
                    still_causal = False
                    for s_msg in self.saved_msg:
                        if self.is_causal(s_msg):
                            self.update_vector_clock(s_msg)
                            self.saved_msg.remove(s_msg)
                            still_causal = True
            else:
                print('({0},{1}) RECEIVES from ({2},{3}): NOT CAUSAL'.format(self.node_id[0], self.node_id[1],msg.node_id[0], msg.node_id[1]))
                #if the message is not causal, save it for later processing
                if msg not in self.saved_msg:
                    self.saved_msg.append(msg)
            #removes message after being processed (not necessary to repeat
            #broadcasting because it's a simulation)
            self.inbox.remove(msg)
        return True

    #checks if message is causal
    def is_causal(self, msg):
        if self.vector_clock[msg.node_id[0]][msg.node_id[1]] + 1 != \
        msg.vector_clock[msg.node_id[0]][msg.node_id[1]]:
            return False
        for x in range(self.side_size):
            for y in range(self.side_size):
                if x != msg.node_id[0] and y != msg.node_id[1]:
                    if msg.vector_clock[x][y] > self.vector_clock[x][y]:
                        return False
        return True

    #updates vector clock event info
    def update_vector_clock(self, msg):
        node_id = msg.node_id
        self.vector_clock[node_id[0]][node_id[1]] = msg.vector_clock[node_id[0]][node_id[1]]

    #prints the vector clock from the node in matrix form
    def print_vector_clock_matrix(self):
        list = ""
        print(self.node_id,":")
        for x in range(self.side_size):
            for y in range(self.side_size):
                list += " | {}".format(self.vector_clock[x][y])
            list += " |"
            print(list)
            list = ""
