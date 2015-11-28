package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.HashSet;
import java.util.Set;

public class Room extends BasicActor<Message.RetrievableMessage, Void> {

    private final Set<ActorRef> users;
    private ActorRef manager;

    public Room(ActorRef room_manager) {
        this.users = new HashSet();
        this.manager = room_manager;
    }


    @SuppressWarnings("empty-statement")
    @Override
    //opções: entrar sala, sair de sala (neste momento está change, mas a logica pertence ao manager,
    //portanto se calhar é melhor meter para sair), enviar mensagem para sala
    protected Void doRun() throws InterruptedException, SuspendExecution {

        while (receive(msg -> {
            switch (msg.type) {
                case ROOM_ENTER:
                    users.add((ActorRef) msg.o);
                    return true;
                case ROOM_CHANGE:
                    users.remove((ActorRef) msg.sender);
                    manager.send(msg);
                    return true;
                case LINE:
                    for (ActorRef u : users) {
                        u.send(msg);
                    }
                    return true;
                case LEAVE:
                    return true;
            }
            return false;
        }));
        return null;
    }
}
