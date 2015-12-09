package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
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

public class ChatServer extends Thread {

    ActorRef acceptor, room_manager, user_manager, event_publisher;
    Supervisor user_supervisor, room_supervisor, manager_supervisor;
    int port;

    ChatServer(int port_nmr) {
        this.port = port_nmr;
    }

    private ActorRef createUserSupervisor() {
        return new SupervisorActor(RestartStrategy.ONE_FOR_ONE).spawn();
    }

    private ActorRef createRoomSupervisor() {
        return new SupervisorActor(RestartStrategy.ONE_FOR_ONE).spawn();
    }

    private ActorRef createManagerSupervisor(ActorRef user_manager, ActorRef room_manager) {
        manager_supervisor = new SupervisorActor(RestartStrategy.ONE_FOR_ONE).spawn();

        try {
            manager_supervisor.addChild(new ChildSpec(
                    "room_manager", ChildMode.TRANSIENT, 10, 1, TimeUnit.SECONDS, 3, room_manager));
            manager_supervisor.addChild(new ChildSpec(
                    "user_manager", ChildMode.TRANSIENT, 10, 1, TimeUnit.SECONDS, 3, user_manager));
        } catch (SuspendExecution | InterruptedException ex) {
            Logger.getLogger(ChatServer.class.getName()).log(Level.SEVERE, null, ex);
        }
        return new SupervisorActor(RestartStrategy.ONE_FOR_ONE).spawn();
    }

    private ActorRef createRoomManager(Supervisor room_supervisor, ActorRef event_publisher) {
        return new RoomManager(room_supervisor, event_publisher).spawn();
    }

    private Acceptor createAcceptor(ActorRef room_manager, ActorRef user_manager, ActorRef user_supervisor) {
        return new Acceptor(this.port, room_manager, user_manager, (Supervisor) user_supervisor);
    }

    private ActorRef createUserManager(ActorRef event_publisher){
        return new UserManager(event_publisher).spawn();
    }
    
    private ActorRef createEventPublisher(){
        return new EventPublisher(port).spawn();
    }

    @Override
    public void run() {

        event_publisher = createEventPublisher(); 
        try {
            event_publisher.send(new Message.RetrievableMessage(Message.MessageType.SUBSCRIBE, "@ROOMMANAGER"));
            event_publisher.send(new Message.RetrievableMessage(Message.MessageType.SUBSCRIBE, "@USERMANAGER"));
        } catch (SuspendExecution ex) {
            Logger.getLogger(ChatServer.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        user_supervisor = (Supervisor) createUserSupervisor();
        room_supervisor = (Supervisor) createRoomSupervisor();

        room_manager = createRoomManager(room_supervisor, event_publisher);
        user_manager = createUserManager(event_publisher);

        manager_supervisor = (Supervisor) createManagerSupervisor(user_manager, room_manager);

        Acceptor ac = createAcceptor(room_manager, user_manager, user_supervisor);
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

        Acceptor(int port, ActorRef rm, ActorRef um, Supervisor us) {
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

                    addChildToUserSupervisor(actor_id, new User(actor_id, room_manager, user_manager, socket, port).spawn());
                    
                }
            } catch (IOException e) {
            }
            return null;
        }

        private void addChildToUserSupervisor(String actor_id, ActorRef new_actor) throws SuspendExecution {
            try {
                this.user_supervisor.addChild(new ChildSpec(
                        actor_id, ChildMode.TRANSIENT, 10, 1, TimeUnit.SECONDS, 3, new_actor));
            } catch (InterruptedException ex) {
                Logger.getLogger(ChatServer.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

    }

}
