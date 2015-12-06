package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.HashMap;
import java.util.*;

public class RoomManager extends BasicActor<Message.RetrievableMessage, Void> {

    Map<String, ActorRef> publicRoomPool; //(nome da sala) -> (referencia da sala)
    Map<String, ActorRef> privateRoomPool;//(nome da sala) -> (referencia da sala)
    Map<String, Set<String>> privateRoomUserMap;
    
    RoomManager() {
        publicRoomPool = new HashMap<>();
        privateRoomPool = new HashMap<>();
        privateRoomUserMap = new HashMap<>();
    }


    @SuppressWarnings("empty-statement")
    private void createPublicRoom(String room_name) throws SuspendExecution {
        if (this.publicRoomPool.containsKey(room_name) == false) {
            ActorRef new_room = new Room(self(), false, room_name).spawn();
            publicRoomPool.put(room_name, new_room);
        }
    }

    private void createPublicRoom(Message.RetrievableMessage msg) throws SuspendExecution {
        String room_name = (String) msg.o;
        if (this.publicRoomPool.containsKey(room_name)) {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The public room " + room_name + " already exists.\n").getBytes()));
        } else {
            ActorRef new_room = new Room(self(), false, room_name).spawn();
            publicRoomPool.put(room_name, new_room);
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The public room " + room_name + " has been created.\n").getBytes()));
        }
    }

    //falta meter aqui o controlo de erros para o processamento da msg
    //falta adicionar o user
    private void createPrivateRoom(Message.RetrievableMessage msg) throws SuspendExecution {
        String room_name = (String) msg.o;
        if (this.privateRoomPool.containsKey(room_name)) {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The private room " + room_name + " already exists.\n").getBytes()));
        } else {
            ActorRef new_room = new Room(self(), false, room_name).spawn();
            privateRoomPool.put(room_name, new_room);
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The private room " + room_name + " has been created.\n").getBytes()));
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
        //userRoom.put(user_ref, publicRoomPool.get(room_name));
        Message.UserDataMessage data = (Message.UserDataMessage) msg.o;
        String room_name = (String) data.password;
        if (privateRoomPool.containsKey(room_name)) {
            //falta aqui
        } else if (publicRoomPool.containsKey(room_name)) { //aqui o room fica responsavel de mandar a sua ref ao user
            publicRoomPool.get(room_name)
                    .send(new Message.RetrievableMessage(
                                    Message.MessageType.USER_ENTER_ROOM, data.username, msg.sender));
        } else {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, "The room doesn't exist.\n".getBytes()));
        }
    }

    public void createTestRooms() throws SuspendExecution {
        createPublicRoom("room1");
        createPublicRoom("room2");
        createPublicRoom("room3");
    }

    /*private void addUser2PrivateRoom(Message.RetrievableMessage msg) {
     String[] users = (String[]) msg.o;
     new_room.send(new Message.RetrievableMessage(Message.MessageType.USER_PRIVATE_ROOM_ADD, users));   
     }*/
    /*private void changeRoom(Message.RetrievableMessage msg) throws SuspendExecution {
     //removeUserFromRoom(msg.sender);
     putInRoom(new Message.RetrievableMessage(
     Message.MessageType.USER_ENTER_ROOM, (String) msg.o, msg.sender));
     }*/

    /*private void removeUserFromRoom(ActorRef user_ref) throws SuspendExecution {
     if (userRoom.containsKey(user_ref)) {
     ActorRef old_room = userRoom.get(user_ref);
     old_room.send(new Message.NonRetrivableMessage(
     Message.MessageType.USER_LEAVE_ROOM, user_ref));
     userRoom.remove(user_ref);
     }
     }*/
    private void removeRoom(Message.RetrievableMessage msg) throws SuspendExecution {
        String room_name = (String) msg.o;

        if (publicRoomPool.containsKey(room_name)) {
            publicRoomPool.get(room_name).send(new Message.RetrievableMessage(Message.MessageType.ADMIN_REMOVE_ROOM, null));
            this.publicRoomPool.remove(room_name);
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The public room " + room_name + " was removed.\n").getBytes()));
        } else if (privateRoomPool.containsKey(room_name)) {
            privateRoomPool.get(room_name).send(new Message.RetrievableMessage(Message.MessageType.ADMIN_REMOVE_ROOM, null));
            this.privateRoomPool.remove(room_name);
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("The private room " + room_name + " was removed.\n").getBytes()));
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
            if(privateRoomUserMap.containsKey(ss))
                if(privateRoomUserMap.get(ss).contains((String) msg.o))
                    rooms = rooms.concat(ss + "\n");
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
                case USER_ENTER_ROOM: //falta testar
                    addUser2Room(msg);
                    return true;
                case USER_CHANGE_ROOM: //o room é que trat de tirar de lá o user, e manda user_enter_room para o room manager
                    //changeRoom(msg);
                    return true;
                case USER_LEAVE_ROOM: //tem de passar para o room default, o room é que trata de tirar de la o user
                    return true;
                case USER_LIST_ROOM: //falta testar
                    publicRoomList(msg);
                    return true;
                case USER_CREATE_PUBLIC_ROOM://falta testar
                    createPublicRoom(msg);
                    return true;
                case USER_CREATE_PRIVATE_ROOM:
                    createPrivateRoom(msg);
                    return true;
                case USER_PRIVATE_ROOM_ADD:
                    //addUser2PrivateRoom(msg);
                    return true;
                case ADMIN_LIST_ROOM: //falta testar
                    privateAndPublicRoomList(msg);
                    return true;
                case ADMIN_REMOVE_ROOM://falta testar
                    removeRoom(msg);
                    return true;
            }
            return false;

        }));
        return null;
    }
}
