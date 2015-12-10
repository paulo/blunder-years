package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.actors.MailboxConfig;
import co.paralleluniverse.actors.behaviors.Supervisor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.HashMap;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

public class RoomManager extends BasicActor<Message.RetrievableMessage, Void> {

    Map<String, ActorRef> publicRoomPool; //(nome da sala) -> (referencia da sala)
    Map<String, ActorRef> privateRoomPool;//(nome da sala) -> (referencia da sala)
    Map<String, Set<String>> privateRoomUserMap;
    ActorRef event_publisher;
    Supervisor room_supervisor;

    RoomManager(Supervisor rs, ActorRef event_publisher) {
        super("room_manager", new MailboxConfig(Message.ROOMMANAGER_BOX_LIMIT, Message.BOX_POLICY));

        publicRoomPool = new HashMap<>();
        privateRoomPool = new HashMap<>();
        privateRoomUserMap = new HashMap<>();
        this.event_publisher = event_publisher;
        room_supervisor = rs;
    }

    private void addChildToRoomSupervisor(String room_name, ActorRef new_room) throws SuspendExecution {
        try {
            this.room_supervisor.addChild(new Supervisor.ChildSpec(
                    room_name, Supervisor.ChildMode.TRANSIENT, 10, 1, TimeUnit.SECONDS, 3, new_room));
        } catch (InterruptedException ex) {
            Logger.getLogger(RoomManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @SuppressWarnings("empty-statement")
    private void createPublicRoom(String room_name) throws SuspendExecution {
        if (this.publicRoomPool.containsKey(room_name) == false) {
            ActorRef new_room = new Room(self(), false, room_name).spawn();
            addChildToRoomSupervisor(room_name, new_room);
            publicRoomPool.put(room_name, new_room);
        }
    }

    private void createPublicRoom(Message.RetrievableMessage msg) throws SuspendExecution {
        String room_name = (String) msg.o;
        if (this.publicRoomPool.containsKey(room_name)) {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The public room " + room_name + " already exists.\n").getBytes()));
        } else {
            ActorRef new_room = new Room(self(), false, room_name).spawn();
            addChildToRoomSupervisor(room_name, new_room);

            publicRoomPool.put(room_name, new_room);
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The public room " + room_name + " has been created.\n").getBytes()));
            event_publisher.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PUBLIC_CREATION_EVENTS, "The public room " + room_name + " has been created.\n"));
        }
    }

    //falta meter aqui o controlo de erros para o processamento da msg
    //falta adicionar o user
    private void createPrivateRoom(Message.RetrievableMessage msg) throws SuspendExecution {
        String[] args = (String[]) msg.o;
        String room_name = args[1];
        List<String> dest_users = new ArrayList<>();
        int i = 2;

        if (this.privateRoomPool.containsKey(room_name)) {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The private room " + room_name + " already exists.\n").getBytes()));
        } else {
            ActorRef new_room = new Room(self(), true, room_name).spawn();
            addChildToRoomSupervisor(room_name, new_room);

            privateRoomPool.put(room_name, new_room);
            privateRoomUserMap.put(room_name, new HashSet<>());
            while (i < args.length) {
                dest_users.add(args[i]);
                i++;
            }
            for (String u : dest_users) {
                addUser2PrivateRoom(u, room_name);
                event_publisher.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PRIVATE_NEWUSER_EVENTS, "User "+ u +" joined the private room "  + room_name + ".\n"));
            }
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The private room " + room_name + " has been created.\n").getBytes()));
            event_publisher.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PRIVATE_CREATION_EVENTS, "The private room " + room_name + " has been created.\n"));
        }
    }

    /**
     * Adicionar user a sala.
     *
     * @param user_ref Referencia do utilizador.
     * @param room_name Nome da sala.
     * @throws SuspendExecution
     */
    private void addUser2Room(Message.RetrievableMessage msg) throws SuspendExecution {

        Message.UserDataMessage data = (Message.UserDataMessage) msg.o;
        String room_name = (String) data.userdata;
        String user_name = (String) data.username;

        if (privateRoomPool.containsKey(room_name)) {
            if (privateRoomUserMap.get(room_name).contains(user_name)) {
                privateRoomPool.get(room_name)
                        .send(new Message.RetrievableMessage(
                                        Message.MessageType.USER_ENTER_ROOM, data.username, msg.sender));
                event_publisher.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PRIVATE_NEWUSER_EVENTS, "User "+ data.username +" joined the private room "  + room_name + ".\n"));
                event_publisher.send(new Message.RetrievableMessage(Message.MessageType.USER_FOLLOW, data.username +  " joined the private room "  + room_name + ".\n", null));
            } else {
                msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("You do not have the credentials to enter " + room_name).getBytes()));
            }
        } else if (publicRoomPool.containsKey(room_name)) {
            publicRoomPool.get(room_name)
                    .send(new Message.RetrievableMessage(
                                    Message.MessageType.USER_ENTER_ROOM, data.username, msg.sender));
            event_publisher.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PUBLIC_NEWUSER_EVENTS, "User "+ data.username +" joined the public room "  + room_name + ".\n"));
            event_publisher.send(new Message.RetrievableMessage(Message.MessageType.USER_FOLLOW,  data.username+ " joined the public room "  + room_name + ".\n", null));
        } else {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The room " + room_name + " doesn't exist.\n").getBytes()));
        }
    }

    public void createTestRooms() throws SuspendExecution {
        createPublicRoom("room1");
        createPublicRoom("room2");
        createPublicRoom("room3");
    }

    private void addUser2PrivateRoom(String username, String room_name) {
        if (this.privateRoomUserMap.containsKey(room_name)) {
            this.privateRoomUserMap.get(room_name).add(username);
        }
    }

    private void removeRoom(Message.RetrievableMessage msg) throws SuspendExecution {
        String room_name = (String) msg.o;

        if (publicRoomPool.containsKey(room_name)) {
            publicRoomPool.get(room_name).send(new Message.RetrievableMessage(Message.MessageType.ADMIN_REMOVE_ROOM, null));
            this.publicRoomPool.remove(room_name);
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The public room " + room_name + " was removed.\n").getBytes()));
            event_publisher.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PUBLIC_REMOVAL_EVENTS, "The public room " + room_name + " was removed.\n"));
        } else if (privateRoomPool.containsKey(room_name)) {
            privateRoomPool.get(room_name).send(new Message.RetrievableMessage(Message.MessageType.ADMIN_REMOVE_ROOM, null));
            this.privateRoomPool.remove(room_name);
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The private room " + room_name + " was removed.\n").getBytes()));
            event_publisher.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PRIVATE_REMOVAL_EVENTS, "The private room " + room_name + " was removed.\n"));
        } else {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The room " + room_name + " does not exist.\n").getBytes()));
        }

    }

    private void privateAndPublicRoomList(Message.RetrievableMessage msg) throws SuspendExecution {
        String rooms = "Public room list:\n";
        for (String s : publicRoomPool.keySet()) {
            rooms = rooms.concat(s + "\n");
        }
        rooms = rooms.concat("Private room list:\n");

        for (String ss : privateRoomPool.keySet()) {
            rooms = rooms.concat(ss + "\n");
        }

        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, rooms.getBytes()));
    }

    private void publicRoomList(Message.RetrievableMessage msg) throws SuspendExecution {
        String rooms = "Public room list:\n";
        for (String s : publicRoomPool.keySet()) {
            rooms = rooms.concat(s + "\n");
        }

        rooms = rooms.concat("Private room list:\n");

        for (String ss : privateRoomPool.keySet()) {
            if (privateRoomUserMap.containsKey(ss)) {
                if (privateRoomUserMap.get(ss).contains((String) msg.o)) {
                    rooms = rooms.concat(ss + "\n");
                }
            }
        }

        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, rooms.getBytes()));
    }

    @Override
    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {

        this.createPublicRoom("global_room");
        createTestRooms();

        while (receive((Message.RetrievableMessage msg) -> {
            switch (msg.type) {
                case USER_ENTER_ROOM:
                    addUser2Room(msg);
                    return true;
                case ROOM_PRIVATE_USEREXIT_EVENTS:
                    privateRoomUserExit(msg);
                    return true;
                case ROOM_PUBLIC_USEREXIT_EVENTS:
                    publicRoomUserExit(msg);
                    return true;
                case USER_LIST_ROOM:
                    publicRoomList(msg);
                    return true;
                case USER_CREATE_PUBLIC_ROOM:
                    createPublicRoom(msg);
                    return true;
                case USER_CREATE_PRIVATE_ROOM:
                    createPrivateRoom(msg);
                    return true;
                case ADMIN_LIST_ROOM:
                    privateAndPublicRoomList(msg);
                    return true;
                case ADMIN_REMOVE_ROOM:
                    removeRoom(msg);
                    return true;
            }
            return false;

        }));
        return null;
    }

    private void privateRoomUserExit(Message.RetrievableMessage msg) throws SuspendExecution {    
        event_publisher.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PRIVATE_USEREXIT_EVENTS, msg.o));
        event_publisher.send(new Message.RetrievableMessage(Message.MessageType.USER_FOLLOW, msg.o));
    }

    private void publicRoomUserExit(Message.RetrievableMessage msg) throws SuspendExecution{
        event_publisher.send(new Message.RetrievableMessage(Message.MessageType.ROOM_PUBLIC_USEREXIT_EVENTS, msg.o));
        event_publisher.send(new Message.RetrievableMessage(Message.MessageType.USER_FOLLOW, msg.o));
    }
}
