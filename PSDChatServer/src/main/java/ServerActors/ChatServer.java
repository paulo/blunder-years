package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.actors.behaviors.EventHandler;
import co.paralleluniverse.actors.behaviors.EventSource;
import co.paralleluniverse.actors.behaviors.EventSourceActor;
import co.paralleluniverse.actors.behaviors.Supervisor;
import co.paralleluniverse.actors.behaviors.Supervisor.ChildMode;
import co.paralleluniverse.actors.behaviors.Supervisor.ChildSpec;
import co.paralleluniverse.actors.behaviors.SupervisorActor;
import co.paralleluniverse.actors.behaviors.SupervisorActor.RestartStrategy;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.FiberServerSocketChannel;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ChatServer extends Thread{

    ActorRef acceptor, room_manager, user_manager, event_publisher;
    Supervisor user_supervisor;
    int port;
    EventSource es;

    ChatServer(int port_nmr) {
        this.port = port_nmr;
    }

    private ActorRef createUserSupervisor() {
        return new SupervisorActor(RestartStrategy.ONE_FOR_ONE).spawn();
    }

    private ActorRef createRoomManager() {
        return new RoomManager().spawn();
    }

    private Acceptor createAcceptor(ActorRef room_manager, ActorRef user_supervisor, ActorRef user_manager) {
        return new Acceptor(this.port, room_manager, (Supervisor) user_supervisor, user_manager);
    }

    private ActorRef createEventSource(String source_type) {
        return new EventSourceActor<String>(source_type).spawn();
    }

    private ActorRef createUserManager(){
        return new UserManager().spawn();
    }
    
/*    private ActorRef createEventPublisher(){
        return new EventPublisher(port).spawn();
    }
  */  
    @Override
    public void run(){
            //event_publisher = createEventPublisher(); 
        room_manager = createRoomManager();
        //user_supervisor = (Supervisor) createUserSupervisor();
        /*es = (EventSource) createEventSource("user_event_actor");*/  
        user_manager = createUserManager();
        Acceptor ac = createAcceptor(room_manager, user_supervisor, user_manager);
        this.acceptor = ac.spawn();

        try {
            ac.join();
        } catch (ExecutionException | InterruptedException ex) {
            Logger.getLogger(ChatServer.class.getName()).log(Level.SEVERE, null, ex);
        }
    
    }
    

    //criar algum método de contenção para o caso da fiber falhar (tipo, meter um watch sobre isto)
    class Acceptor extends BasicActor {

        final int port;
        int user_count;
        final ActorRef user_manager;
        final ActorRef room_manager;
        final Supervisor user_supervisor;

        Acceptor(int port, ActorRef rm, Supervisor us, ActorRef um) {
            this.port = port;
            this.room_manager = rm;
            this.user_supervisor = us;
            this.user_count = 1;
            this.user_manager = um;
        }

        @Override
        protected Void doRun() throws InterruptedException, SuspendExecution {

            try {
                FiberServerSocketChannel ss
                        = FiberServerSocketChannel
                        .open()
                        .bind(new InetSocketAddress(port));

                while (true) {
                    
                    String actor_id = "actor" + user_count++;
                    FiberSocketChannel socket = ss.accept();
                    ActorRef new_actor; 
                    //se for cliente normal:
                    new_actor = new User(actor_id, room_manager, user_manager, socket, es).spawn();
                    //se for cliente tipo notification console:
                    //new_actor = new NotificationSubscriber(port).spawn();

                    /*es.addHandler((EventHandler) (Object event) -> {
                     System.out.println(event.toString());
                     });
                    
                     this.user_supervisor.addChild(new ChildSpec(
                     actor_id, ChildMode.TRANSIENT, 10, 1, TimeUnit.SECONDS, 3,
                     new_actor));*/
                }
            } catch (IOException e) {
            }
            return null;
        }
    }

}
