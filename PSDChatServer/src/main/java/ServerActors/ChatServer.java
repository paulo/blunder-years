package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.FiberServerSocketChannel;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;


public class ChatServer {

    ActorRef acceptor, room_manager;
    int port;
    
    ChatServer(int port_nmr){
        this.port = port_nmr;
    }
    
    public void init(){
        this.room_manager = new RoomManager().spawn();
        Acceptor ac = new Acceptor(port, this.room_manager);
        this.acceptor = ac.spawn();
        
        try {
            ac.join();
        } catch (ExecutionException | InterruptedException ex) {
            Logger.getLogger(ChatServer.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    //criar algum método de contenção para o caso da fiber falhar (tipo, meter um watch sobre isto)
    static class Acceptor extends BasicActor {

        final int port;
        final ActorRef roomManager;

        Acceptor(int port, ActorRef roomManager) {
            this.port = port;
            this.roomManager = roomManager;
        }

        @Override
        protected Void doRun() throws InterruptedException, SuspendExecution {
            try {
                FiberServerSocketChannel ss = 
                        FiberServerSocketChannel
                        .open()
                        .bind(new InetSocketAddress(port));
                
                while (true) {
                    FiberSocketChannel socket = ss.accept();
                    new User(roomManager, socket).spawn();
                }
            } catch (IOException e) {
            }
            return null;
        }
    }

}
