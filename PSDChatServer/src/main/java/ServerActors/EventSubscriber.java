package ServerActors;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import org.zeromq.ZMQ;

public class EventSubscriber extends BasicActor<Message.RetrievableMessage, Void> {

    private int port;

    EventSubscriber(int port) {
        this.port = port;
    }

    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {

        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.SUB);
        socket.connect("tcp://localhost:" + port);
        
        while (receive((Message.RetrievableMessage msg) -> {
            switch (msg.type) {
                case SUBSCRIBE:
                    String event = (String) msg.o;
                    socket.subscribe(event.getBytes());
                    return true;
                default:
                    byte[] b = socket.recv(ZMQ.DONTWAIT);
                    System.out.println(new String(b));
            }
            return false;
        }));

        socket.close();
        context.term();

        return null;

    }

}
