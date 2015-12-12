package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.actors.MailboxConfig;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.*;

public class Room extends BasicActor<Message.RetrievableMessage, Void> {

    public final String room_name;
    private final Map<String, ActorRef> user_list;
    private final ActorRef manager;
    public boolean isPrivate;
    public boolean active;

    public Room(ActorRef room_manager, boolean privateStatus, String room_name) {
        super(room_name, new MailboxConfig(Message.ROOM_BOX_LIMIT, Message.BOX_POLICY));
        this.room_name = room_name;
        this.user_list = new HashMap<>();
        this.manager = room_manager;
        this.isPrivate = privateStatus;
        this.active = true;
    }

    private void listUsers(Message.RetrievableMessage msg) throws SuspendExecution {
        String user_room_list = "User list for room " + room_name + "\n";
        for (String s : this.user_list.keySet()) {
            user_room_list = user_room_list.concat(s + "\n");
        }

        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, user_room_list.getBytes()));
    }

    private void addUser(Message.RetrievableMessage msg) throws SuspendExecution {
        user_list.put((String) msg.o, (ActorRef) msg.sender);
        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.USER_ENTER_ROOM_ACK, room_name, self()));
    }

    private void logOutUser(Message.RetrievableMessage msg) throws SuspendExecution {
        String user = (String) msg.o;
        this.user_list.remove(user);
        if (this.isPrivate) {
            manager.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PRIVATE_USEREXIT_EVENTS, user + " left private room " + room_name + ".\n", null));
        } else {
            manager.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PUBLIC_USEREXIT_EVENTS, user + " left public room " + room_name + ".\n"));
        }
    }

    private void userExit(Message.RetrievableMessage msg) throws SuspendExecution {
        this.user_list.remove((String) msg.o);
        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("Room " + this.room_name + " exited.\n").getBytes()));
    }

    private void removeRoom() throws SuspendExecution {
        for (ActorRef ar : this.user_list.values()) {
            ar.send(new Message.RetrievableMessage(Message.MessageType.ADMIN_REMOVE_ROOM_ACK, this.room_name));
        }
        this.active = false;
    }

    private void writeToUsers(Message.RetrievableMessage msg) throws SuspendExecution {
        Message.UserDataMessage data = (Message.UserDataMessage) msg.o;
        Message.RetrievableMessage msg_sent
                = new Message.RetrievableMessage(Message.MessageType.LINE,
                        (data.username + "@" + this.room_name + ": " + (new String((byte[]) data.userdata))).getBytes());

        for (ActorRef u : this.user_list.values()) {
            if (((ActorRef) msg.sender).equals(u) == false) {
                u.send(msg_sent);
            }
        }
    }

    @SuppressWarnings("empty-statement")
    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {

        while (active && receive(msg -> {
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
