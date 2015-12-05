/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ServerActors;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.Scanner;
import org.zeromq.ZMQ;

/**
 *
 * @author SimaoDias
 */
/*
class NotificationPublisher extends BasicActor<Message.RetrievableMessage, Void> {
    
    private int port;

    NotificationPublisher(int port){
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
                    socket.send(msg.o);
                }
            return false;
        }));
        
        socket.close();
        context.term();
              
        return null; 
               
    }

}*/
