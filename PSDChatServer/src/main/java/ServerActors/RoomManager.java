package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.HashMap;
import java.util.*;

public class RoomManager extends BasicActor<Message.RetrievableMessage, Void> {

    Map<String, ActorRef> publicRoomPool; //(nome da sala) -> (referencia da sala)
    Map<String, ActorRef> privateRoomPool;//(nome da sala) -> (referencia da sala)
    //Map<ActorRef, ActorRef> userRoom;//(referencia de user) -> (referencia de sala) talvez nao seja preciso

    RoomManager() {
        publicRoomPool = new HashMap<>();
        privateRoomPool = new HashMap<>();
        //userRoom = new HashMap<>();
    }

    //talvez ao criar o room dar-lhe o nome como atributo
    @SuppressWarnings("empty-statement")
    private void createPublicRoom(String room_name) {
        ActorRef new_room = new Room(self(), false, room_name).spawn();
        publicRoomPool.put(room_name, new_room);
    }

    //falta meter aqui o controlo de erros para o processamento da msg
    private void createPrivateRoom(Message.RetrievableMessage msg) throws SuspendExecution {
        String room_name = (String) msg.o;
        
        ActorRef new_room = new Room(self(), true, room_name).spawn();
        publicRoomPool.put(room_name, new_room);
        //tem de ser meter controlo de erros caso a sala já exista
        //talvez meter aqui para adicionar também o user, o problema é se o user for admin
    }

    /**
     * Adicionar user a sala.
     *
     * @param user_ref Referencia do utilizador.
     * @param room_name Nome da sala.
     * @throws SuspendExecution
     */
    private void addUser2PublicRoom(Message.RetrievableMessage msg) throws SuspendExecution {
        //userRoom.put(user_ref, publicRoomPool.get(room_name));
        String room_name = (String) msg.o;
        if (privateRoomPool.containsKey(room_name)) {
            //falta aqui
        } else if (publicRoomPool.containsKey(room_name)) { //aqui o room fica responsavel de mandar a sua ref ao user
            publicRoomPool.get(room_name)
                    .send(new Message.RetrievableMessage(
                                    Message.MessageType.USER_ENTER_ROOM, null, msg.sender));
        } else {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, "The room doesn't exist"));
        }
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
        String[] room_names = (String[]) msg.o;
        for (String s : room_names) {
            if (publicRoomPool.containsKey(s)) {
                publicRoomPool.get(s).send(new Message.RetrievableMessage(Message.MessageType.ADMIN_REMOVE_ROOM, null));
            } else if (privateRoomPool.containsKey(s)) {
                privateRoomPool.get(s).send(new Message.RetrievableMessage(Message.MessageType.ADMIN_REMOVE_ROOM, null));
            } else {
                msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, "The room "+s+" does not exist."));
            }
        }
    }

    private void userLogIn() {

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

        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.DATA, rooms));
    }

    private void publicRoomList(Message.RetrievableMessage msg) throws SuspendExecution {
        String rooms = "Public room list:\n";
        for (String s : publicRoomPool.keySet()) {
            rooms = rooms.concat(s + "\n");
        }

        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.DATA, rooms));
    }

    private void userLogOut() {

    }

    @Override
    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {
        
        this.createPublicRoom("global_room");

        while (receive((Message.RetrievableMessage msg) -> {
            switch (msg.type) {
                case USER_ENTER_ROOM: //falta testar
                    addUser2PublicRoom(msg);
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
                    createPublicRoom((String) msg.o);
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
