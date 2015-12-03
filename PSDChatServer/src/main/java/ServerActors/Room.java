package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.*;


//falta a opção para quando a sala se desliga a si propria
//falta, ao adicionar um utilizador, adicionar o username dele
//falta criar quarto com o seu nome como atributo
public class Room extends BasicActor<Message.RetrievableMessage, Void> {

    private String room_name;
    private final Map<String, ActorRef> user_list;
    private final Set<ActorRef> users;
    private ActorRef manager;
    private boolean isPrivate;
    
    
    public Room(ActorRef room_manager, boolean permission, String room_name) {
        this.room_name = room_name;
        this.user_list = new HashMap<>();
        this.users = new HashSet();
        this.manager = room_manager;
        this.isPrivate = permission;
    }

    private Message.RetrievableMessage listUsers(Message.RetrievableMessage msg) {
        String user_room_list = "User list for room "+room_name+"\n";
        for(String s: this.user_list.keySet()){
            user_room_list = user_room_list.concat(s+"\n");
        }
        
        return new Message.RetrievableMessage(Message.MessageType.DATA, user_room_list);
    }
    
    
    @SuppressWarnings("empty-statement")
    @Override
    //opções: entrar sala, sair de sala (neste momento está change, mas a logica pertence ao manager,
    //portanto se calhar é melhor meter para sair), enviar mensagem para sala
    protected Void doRun() throws InterruptedException, SuspendExecution {

        while (receive(msg -> {
            switch (msg.type) {
                case USER_LIST_ROOM_USERS:
                    listUsers(msg);
                    return true;
                
                /*case ROOM_ENTER:
                    users.add((ActorRef) msg.o);
                    return true;
                case ROOM_CHANGE:
                    users.remove((ActorRef) msg.sender);
                    manager.send(msg);
                    return true;*/
                case LINE:
                    for (ActorRef u : users) {
                        u.send(msg);
                    }
                    return true;
                /*case LEAVE:
                    return true;*/
            }
            return false;
        }));
        return null;
    }

}
