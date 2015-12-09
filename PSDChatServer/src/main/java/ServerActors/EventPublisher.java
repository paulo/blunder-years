package ServerActors;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.time.LocalDateTime;
import org.zeromq.ZMQ;

class EventPublisher extends BasicActor<Message.RetrievableMessage, Void> {
    
    private int port;

    EventPublisher(int port){
        this.port = port+1;
    }
    
    @Override
    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {
       
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.PUB);
        
        
        System.out.println("USING PORT " + port);
        socket.bind("tcp://*:" + port);
       
        while (receive(msg -> {
            String res;
            switch(msg.type){
                case USER_LOGIN_EVENT:
                    res = stampMessage("@USERMANAGER",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case ROOM_EVENT:
                    res = stampMessage("@ROOMMANAGER",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                default:
                    System.out.println("No message type");
                }
            return false;
        }));
        
        socket.close();
        context.term();

        return null; 
               
    }
    
    private String stampMessage(String eventPrefix, String message){
        StringBuilder res = new StringBuilder(eventPrefix);
        int hours = LocalDateTime.now().getHour();
        int minutes = LocalDateTime.now().getMinute();
        int seconds = LocalDateTime.now().getSecond();
        
        res.append(" [");
        res.append(hours);
        res.append(":");
        res.append(minutes);
        res.append(":");
        res.append(seconds);
        res.append("]> ");
        res.append(message);
        
        return res.toString();   
    }

}
