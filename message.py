# Cada mensagem tem: vector_clock, id do nodo original, mensagem
class Message():
    def __init__(self, node_id, content, vector_clock):
        self.node_id = node_id
        self.content = content
        self.vector_clock = vector_clock

    def equals(self, msg):
        return self.node_id == msg.node_id and \
        self.content == msg.content and \
        self.vector_clock == msg.vector_clock

    def is_newer(self, node_vc):
        return self.vector_clock[self.node_id[0]][self.node_id[1]] \
         > node_vc[self.node_id[0]][self.node_id[1]]
