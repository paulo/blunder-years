#Each message has the node_id and vector_clock from the node that
#created it and some content
class Message():
    def __init__(self, node_id, content, vector_clock):
        self.node_id = node_id
        self.content = content
        self.vector_clock = vector_clock

    #compare messages
    def equals(self, msg):
        return self.node_id == msg.node_id and \
        self.content == msg.content and \
        self.vector_clock == msg.vector_clock

    #check if given message is newer by checking the given vector_clock
    def is_newer(self, node_vector_clock):
        return self.vector_clock[self.node_id[0]][self.node_id[1]] \
         > node_vector_clock[self.node_id[0]][self.node_id[1]]
