package ServerActors;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import org.zeromq.ZMQ;

class EventPublisher extends BasicActor<Message.RetrievableMessage, Void> {
    
    private int port;

    EventPublisher(int port){
        this.port = port;
    }
    
    @Override
    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {
       
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.PUB);
        socket.bind("tcp://*:" + port);
        
        while (receive(msg -> {
            switch(msg.type){
                case DATA:
                    socket.send((String) msg.o);
                }
            return false;
        }));
        
        socket.close();
        context.term();
              
        return null; 
               
    }

}
