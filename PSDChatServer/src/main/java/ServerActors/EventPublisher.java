package ServerActors;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.time.LocalDateTime;
import java.util.Arrays;
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
                case USER_LOGIN_EVENTS:
                    res = stampMessage("@LOGIN",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case USER_LOGOUT_EVENTS:
                    res = stampMessage("@LOGIN",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case USER_CREATION_EVENTS:
                    res = stampMessage("@USER",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case USER_REMOVAL_EVENTS:
                    res = stampMessage("@USER",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case ROOM_PRIVATE_CREATION_EVENTS:
                    res = stampMessage("@PRIVATE_ROOM_MANAGEMENT",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case ROOM_PRIVATE_REMOVAL_EVENTS:
                    res = stampMessage("@PRIVATE_ROOM_MANAGEMENT",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case ROOM_PUBLIC_CREATION_EVENTS:
                    res = stampMessage("@PUBLIC_ROOM_MANAGEMENT",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case ROOM_PUBLIC_REMOVAL_EVENTS:
                    res = stampMessage("@PUBLIC_ROOM_MANAGEMENT",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case ROOM_PUBLIC_NEWUSER_EVENTS:
                    res = stampMessage("@PUBLIC_ROOM_USERS",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case ROOM_PUBLIC_USEREXIT_EVENTS:
                    res = stampMessage("@PUBLIC_ROOM_USERS",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true; 
                case ROOM_PRIVATE_NEWUSER_EVENTS:
                    res = stampMessage("@PRIVATE_ROOM_USERS",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true;
                case ROOM_PRIVATE_USEREXIT_EVENTS:
                    res = stampMessage("@PRIVATE_ROOM_USERS",(String) msg.o);
                    System.out.print(res);
                    socket.send(res);
                    return true; 
                case USER_FOLLOW:
                    res = stampMessage("@<"+ getUsername(msg).toUpperCase()+">", getInfo(msg));
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

    private String getUsername(Message.RetrievableMessage msg){
        String data = (String) msg.o;
        String[] splitData = data.split(" ");
        return splitData[0];
    }
    
    private String getInfo(Message.RetrievableMessage msg){
        String data = (String) msg.o;
        String[] splitData = data.split(" ");
        int x = splitData.length;
        return glue(Arrays.copyOfRange(splitData, 1, x));
    }

    private String glue(String[] arrayToGlue) {
        StringBuilder res = new StringBuilder();
        
        for(int i = 0; i < arrayToGlue.length; i++){
            res.append(arrayToGlue[i]);
            if(i != arrayToGlue.length - 1){
                res.append(" ");
            }
            
        }
        return res.toString();
        
    }
}
