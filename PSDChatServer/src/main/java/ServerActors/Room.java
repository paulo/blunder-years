package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.*;


//falta a opção para quando a sala se desliga a si propria
public class Room extends BasicActor<Message.RetrievableMessage, Void> {

    private final String room_name;
    private final Map<String, ActorRef> user_list;
    private final ActorRef manager;
    private boolean isPrivate;

    public Room(ActorRef room_manager, boolean privateStatus, String room_name) {
        this.room_name = room_name;
        this.user_list = new HashMap<>();
        this.manager = room_manager;
        this.isPrivate = privateStatus;
    }

    private void listUsers(Message.RetrievableMessage msg) throws SuspendExecution {
        String user_room_list = "User list for room " + room_name + "\n";
        for (String s : this.user_list.keySet()) {
            user_room_list = user_room_list.concat(s + "\n");
        }

        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, user_room_list.getBytes()));
    }

    //fazer controlo de erros (se o user já estive na sala, etc...
    private void addUser(Message.RetrievableMessage msg) throws SuspendExecution {
        user_list.put((String) msg.o, (ActorRef) msg.sender);
        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.USER_ENTER_ROOM_ACK, room_name, self()));
    }

    //falta meter controlo de erros para se o utilizador não estiver loggedin
    private void logOutUser(Message.RetrievableMessage msg) throws SuspendExecution {
        this.user_list.remove((String) msg.o);
    }

    private void userExit(Message.RetrievableMessage msg) throws SuspendExecution {
        this.user_list.remove((String) msg.o);
        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("Room " + this.room_name + " exited.").getBytes()));
    }
    
    private void removeRoom() throws SuspendExecution {
        for (ActorRef ar : this.user_list.values()) {
            ar.send(new Message.RetrievableMessage(Message.MessageType.ADMIN_REMOVE_ROOM_ACK, this.room_name));
        }
        try {
            self().close();
        } catch (Exception e) {
            System.out.println("Exception removing romo");
        }
    }

    private void writeToUsers(Message.RetrievableMessage msg) throws SuspendExecution {
        for (ActorRef u : this.user_list.values()) {
            u.send(msg);
        }
    }

    @SuppressWarnings("empty-statement")
    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {

        while (receive(msg -> {
            switch (msg.type) {
                case USER_LIST_ROOM_USERS:
                    listUsers(msg);
                    return true;
                case USER_ENTER_ROOM:
                    addUser(msg);
                    return true;
                case ADMIN_REMOVE_ROOM:
                    removeRoom();
                    return true;
                case USER_LOGOUT:
                    logOutUser(msg);
                    return true;
                case LINE:
                    writeToUsers(msg);
                    return true;
                case USER_LEAVE_ROOM:
                    userExit(msg);
                    return true;
            }
            return false;
        }));
        return null;
    }

}
