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

public class ChatServer {

    ActorRef acceptor, room_manager;
    Supervisor user_supervisor;
    int port;
    EventSource es;

    ChatServer(int port_nmr) {
        this.port = port_nmr;
    }

    public void init() {
        this.room_manager = new RoomManager().spawn();
        user_supervisor = new SupervisorActor(RestartStrategy.ONE_FOR_ONE).spawn();

        Acceptor ac = new Acceptor(port, this.room_manager, this.user_supervisor);

        es = new EventSourceActor<String>("user_event_actor").spawn();
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
        final ActorRef room_manager;
        final Supervisor user_supervisor;

        Acceptor(int port, ActorRef rm, Supervisor us) {
            this.port = port;
            this.room_manager = rm;
            this.user_supervisor = us;
            this.user_count = 1;
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
                    ActorRef new_actor = new User(actor_id, room_manager, socket, es).spawn();
                    
                    es.addHandler((EventHandler) (Object event) -> {
                        System.out.println(event.toString());
                    });
                    
                    this.user_supervisor.addChild(new ChildSpec(
                            actor_id, ChildMode.TRANSIENT, 10, 1, TimeUnit.SECONDS, 3,
                            new_actor));

                }
            } catch (IOException e) {
            }
            return null;
        }
    }

}
