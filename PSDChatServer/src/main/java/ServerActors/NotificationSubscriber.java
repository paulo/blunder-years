/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ServerActors;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;

/**
 *
 * @author SimaoDias
 */
public class NotificationSubscriber extends BasicActor<Message.RetrievableMessage, Void>{
    
    private int port;

    NotificationSubscriber(int port){
        this.port = port;
    }
    
    @Override
    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {
       
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.SUB);
        socket.connect("tcp://localhost:"+port);
        
        while (receive(msg -> {
            switch(msg.type){
                case SUBSCRIBE:
                    String event =(String) msg.o;
                    socket.subscribe(event.getBytes());
                    break;
                default:
                    byte[] b = socket.recv();
                    System.out.println(new String(b));
            }            
            return false;
        }));
        
        socket.close();
        context.term();
              
        return null; 
               
    }
    
}
