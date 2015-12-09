package ServerActors;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import org.zeromq.ZMQ;

public class EventSubscriber extends BasicActor<Message.RetrievableMessage, Void> {

    private int port;

    EventSubscriber(int port) {
        this.port = port+1;
    }

    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {

        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.SUB);
        socket.connect("tcp://localhost:" + port);
        new SubscriberBot(socket).spawn();
        
        while (receive((Message.RetrievableMessage msg) -> {
            switch (msg.type) {
                case SUBSCRIBE:
                    String event = (String) msg.o;
                    socket.subscribe(event.getBytes());
                    return true;
            }
            return false;
        }));

        socket.close();
        context.term();

        return null;

    }
    
    class SubscriberBot extends BasicActor{
        private ZMQ.Socket socket;
        SubscriberBot(ZMQ.Socket socket){
            this.socket = socket;
        }

        @Override
        protected Object doRun() throws InterruptedException, SuspendExecution {
            while(true){
                String s = socket.recvStr();
                System.out.println(s);
            }
        }  
    } 
}
