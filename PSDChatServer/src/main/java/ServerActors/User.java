package ServerActors;

import static ServerActors.User.MAXLEN;
import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.actors.MailboxConfig;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import com.google.common.collect.Iterables;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.*;

public class User extends BasicActor<Message.RetrievableMessage, Void> {

    static int MAXLEN = 1024;

    private String username;
    private String password;
    public boolean isLoggedIn, isAdmin;
    private final Map<String, ActorRef> rooms;
    private ActorRef writing_room;
    private String temp_user, temp_pass;
    private final ActorRef room_manager;
    final FiberSocketChannel socket;
    private final ActorRef user_manager;
    final int port;
    

    User(String actor_id, ActorRef room, ActorRef user_manager, FiberSocketChannel socket, int port) {
        super(actor_id, new MailboxConfig(Message.USER_BOX_LIMIT, Message.BOX_POLICY));
        this.room_manager = room;
        this.socket = socket;
        this.isLoggedIn = isAdmin = false;
        this.user_manager = user_manager;
        this.rooms = new HashMap<>();
        this.temp_pass = temp_user = null;
        this.port = port;
    }

    private void userLogout() throws SuspendExecution, IOException {
        if (this.isLoggedIn) {
            for (ActorRef room : this.rooms.values()) {
                room.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGOUT, this.username));
            }
            user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGOUT, this.username, self()));
            this.writing_room = null;
            this.username = this.password = this.temp_pass = this.temp_user = null;
            this.rooms.clear();
            this.isLoggedIn = isAdmin = false;
        } else {
            notLoggedInMessage();
        }
    }

    private void userLogin(Message.UserDataMessage msg) throws SuspendExecution, IOException {
        if (!this.isLoggedIn) {
            this.temp_user = (String) msg.username;
            this.temp_pass = (String) msg.userdata;
            user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGIN, msg, self()));
        } else {
            notLoggedOutMessage();
        }
    }

    private void userRegister(Message.RetrievableMessage msg) throws SuspendExecution, IOException {
        if (!this.isLoggedIn) {
            Message.UserDataMessage data = (Message.UserDataMessage) msg.o;
            this.temp_user = (String) data.username;
            this.temp_pass = (String) data.userdata;
            user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_REGISTER, data, self()));
        } else {
            notLoggedOutMessage();
        }
    }

    private void enterGlobalRoom() throws SuspendExecution {
        room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_ENTER_ROOM,
                new Message.UserDataMessage(this.username, "global_room"), self()));
    }

    private void listRoom() throws SuspendExecution, IOException {
        if (this.isLoggedIn) {
            if (this.isAdmin) {
                room_manager.send(new Message.RetrievableMessage(Message.MessageType.ADMIN_LIST_ROOM, null, self()));
            } else {
                room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM, username, self()));
            }
        } else {
            notLoggedInMessage();
        }
    }

    private void listRoomUsers() throws SuspendExecution, IOException {
        if (isLoggedIn) {
            for (ActorRef ar : this.rooms.values()) {
                ar.send(new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM_USERS, null, self()));
            }
        } else {
            notLoggedInMessage();
        }
    }

    private void sendPrivateMessage(Message.RetrievableMessage msg) throws SuspendExecution, IOException {
        if (isLoggedIn) {
            String[] args = (String[]) msg.o;
            args[0] = username;

            user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_PRIVATE_MESSAGE, args, self()));
        } else {
            notLoggedInMessage();
        }
    }

    private void writeToSocket(Message.RetrievableMessage msg) throws IOException, SuspendExecution {
        socket.write(ByteBuffer.wrap((byte[]) msg.o));
    }

    private void writeStringToSocket(String msg) throws IOException, SuspendExecution {
        socket.write(ByteBuffer.wrap(msg.getBytes()));
    }

    private void changeRoom(Message.RetrievableMessage msg) throws IOException, SuspendExecution {
        String room_name = (String) msg.o;
        if (isLoggedIn) {
            if (this.rooms.containsKey(room_name)) {
                this.writing_room = this.rooms.get(room_name);
                writeStringToSocket("Now writing to: " + room_name);
            } else {
                enterRoom(msg);
            }
        } else {
            notLoggedInMessage();
        }
    }

    private void enterRoom(Message.RetrievableMessage msg) throws SuspendExecution, IOException {
        String[] rooms_names = (String[]) msg.o;
        if (isLoggedIn) {
            for (int i = 1; i < rooms_names.length; i++) {
                room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_ENTER_ROOM,
                        new Message.UserDataMessage(this.username, rooms_names[i]), self()));
            }
        } else {
            notLoggedInMessage();
        }
    }

    private void leaveRoom(Message.RetrievableMessage msg) throws SuspendExecution, IOException {
        String[] rooms_names = (String[]) msg.o;
        if (isLoggedIn) {
            for (int i = 1; i < rooms_names.length; i++) {
                if (this.rooms.containsKey(rooms_names[i])) {
                    ActorRef room_ref = this.rooms.get(rooms_names[i]);
                    if (this.rooms.size() > 1) {
                        this.rooms.remove(rooms_names[i]);
                        if (room_ref.equals(this.writing_room)) {
                            this.writing_room = Iterables.get(this.rooms.values(), 0);
                        }
                        room_ref.send(new Message.RetrievableMessage(Message.MessageType.USER_LEAVE_ROOM, this.username, self()));
                    } else {
                        writeStringToSocket("There has to be at least another room in which you're in.");
                    }
                } else {
                    writeStringToSocket("You are not in the room: " + rooms_names[i]);
                }
            }
        } else {
            notLoggedInMessage();
        }
    }

    private void listMyRooms() throws IOException, SuspendExecution {
        String listening_list = "Listening to rooms:\n";
        if (isLoggedIn) {
            for (String s : this.rooms.keySet()) {
                listening_list = listening_list.concat(s + "\n");
            }
            writeStringToSocket(listening_list);
        } else {
            notLoggedInMessage();
        }
    }

    private void adminRemoveRoom(Message.RetrievableMessage msg) throws SuspendExecution, IOException {
        if (this.isLoggedIn && this.isAdmin) {
            if (((String) msg.o).equals("global_room")) {
                writeStringToSocket("You can't remove the global room.\n");
            } else {
                room_manager.send(new Message.RetrievableMessage(Message.MessageType.ADMIN_REMOVE_ROOM, (String) msg.o, self()));
            }
        } else {
            notAdminMessage();
        }
    }

    private void sendMessageToRoom(Message.RetrievableMessage msg) throws SuspendExecution, IOException {
        if (isLoggedIn) {
            writing_room.send(new Message.RetrievableMessage(Message.MessageType.LINE,
                    new Message.UserDataMessage(username, msg.o), self()));
        } else {
            notLoggedInMessage();
        }
    }

    private void roomAck(Message.RetrievableMessage msg) throws SuspendExecution, IOException {
        this.rooms.put((String) msg.o, (ActorRef) msg.sender);
        this.writing_room = (ActorRef) msg.sender;
        writeStringToSocket("Now receiving messages from: " + (String) msg.o + ".\n");
    }

    private void removeRoom(Message.RetrievableMessage msg) throws SuspendExecution, IOException {

        String[] rooms_names = (String[]) msg.o;
        for (int i = 1; i < rooms_names.length; i++) {
            if (this.rooms.containsKey(rooms_names[i])) {
                if (this.rooms.size() > 1) {
                    if (this.rooms.get(rooms_names[i]).equals(writing_room)) {
                        this.rooms.remove(rooms_names[i]);
                        this.writing_room = Iterables.get(this.rooms.values(), 0);
                    } else {
                        this.rooms.remove(rooms_names[i]);
                    }
                } else {
                    enterGlobalRoom();
                }
            } else {
                writeStringToSocket("You are not in the room: " + rooms_names[i]);
            }
        }
    }

    private void createPublicRoom(Message.RetrievableMessage msg) throws SuspendExecution, IOException {
        String[] rooms_names = (String[]) msg.o;
        if (this.isLoggedIn) {
            for (int i = 1; i < rooms_names.length; i++) {
                room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_CREATE_PUBLIC_ROOM, rooms_names[i], self()));
            }
        } else {
            notLoggedInMessage();
        }
    }

    private void listOnlineUsers() throws SuspendExecution, IOException {
        if (this.isLoggedIn) {
            user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LIST_USERS, null, self()));
        } else {
            notLoggedInMessage();
        }
    }

    private void createPrivateRoom(Message.RetrievableMessage msg) throws SuspendExecution, IOException {
        if (this.isLoggedIn) {
            room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_CREATE_PRIVATE_ROOM, msg.o, self()));
        } else {
            notLoggedInMessage();
        }
    }

    private void notLoggedInMessage() throws IOException, SuspendExecution {
        writeStringToSocket("You must be logged in.\n");
    }

    private void notAdminMessage() throws IOException, SuspendExecution {
        writeStringToSocket("You must be logged in and have admin privileges.\n");
    }

    private void notLoggedOutMessage() throws IOException, SuspendExecution {
        writeStringToSocket("You must be logged out.\n");
    }

    private void logInUserActor() throws SuspendExecution {
        this.username = this.temp_user;
        this.password = this.temp_pass;
        this.isLoggedIn = true;
        enterGlobalRoom();
    }

    private void logInAdminActor() throws SuspendExecution {
        this.username = this.temp_user;
        this.password = this.temp_pass;
        this.isAdmin = true;
        this.isLoggedIn = true;
        enterGlobalRoom();
    }
 
    @SuppressWarnings("empty-statement")
    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {

        new LineReader(self(), socket).spawn();

        while (receive((Message.RetrievableMessage msg) -> {
            try {
                switch (msg.type) {
                    case USER_LIST_ROOM:
                        listRoom();
                        return true;
                    case USER_LIST_ROOM_USERS:
                        listRoomUsers();
                        return true;
                    case USER_LIST_USERS:
                        listOnlineUsers();
                        return true;
                    case USER_PRIVATE_MESSAGE:
                        sendPrivateMessage(msg);
                        return true;
                    case DATA:
                        sendMessageToRoom(msg);
                        return true;
                    case USER_ENTER_ROOM:
                        enterRoom(msg);
                        return true;
                    case USER_CHANGE_ROOM:
                        changeRoom(msg);
                        return true;
                    case USER_LEAVE_ROOM:
                        leaveRoom(msg);
                        return true;
                    case USER_REGISTER:
                        userRegister(msg);
                        return true;
                    case USER_REGISTER_ACK:
                        writeToSocket(msg);
                        userLogin(new Message.UserDataMessage(this.temp_user, this.temp_pass));
                        return true;
                    case USER_LOGIN_ACK:
                        logInUserActor();
                        return true;
                    case ADMIN_LOGIN_ACK:
                        logInAdminActor();
                        return true;
                    case USER_ENTER_ROOM_ACK:
                        roomAck(msg);
                        return true;
                    case USER_LOGIN:
                        userLogin((Message.UserDataMessage) msg.o);
                        return true;
                    case USER_LOGOUT:
                        userLogout();
                        return true;
                    case USER_LIST_MY_ROOMS:
                        listMyRooms();
                        return true;
                    case ADMIN_REMOVE_ROOM:
                        adminRemoveRoom(msg);
                        return true;
                    case ADMIN_REMOVE_ROOM_ACK:
                        removeRoom(msg);
                        return true;
                    case USER_CREATE_PUBLIC_ROOM:
                        createPublicRoom(msg);
                        return true;
                    case USER_CREATE_PRIVATE_ROOM:
                        createPrivateRoom(msg);
                        return true;
                    case LINE:
                        writeToSocket(msg);
                        return true;
                    case EOF:
                        System.out.println("EOF at lineReader");
                        userLogout();
                        socket.close();
                        return true;
                    case IOE:
                        System.out.println("IOException at lineReader");
                        userLogout();
                        socket.close();
                        return false;
                }
            } catch (IOException e) {
                System.out.println("IOException at user: " + e.toString());
            }
            return false;  // stops the actor if some unexpected message is received
        }));
        return null;
    }
}

class LineReader extends BasicActor<Message.RetrievableMessage, Void> {

    final ActorRef<Message.RetrievableMessage> dest;
    final FiberSocketChannel socket;
    ByteBuffer in = ByteBuffer.allocate(MAXLEN);
    ByteBuffer out = ByteBuffer.allocate(MAXLEN);

    LineReader(ActorRef<Message.RetrievableMessage> dest, FiberSocketChannel socket) {
        this.dest = dest;
        this.socket = socket;
    }

    private Message.RetrievableMessage processInput(byte[] input) throws UnsupportedEncodingException {

        String decoded = new String(input, "UTF-8").replaceAll("[\r\n]", "");
        String[] inst = decoded.split(" ");

        switch (inst[0].toLowerCase()) {
            //mudar sala de escrita
            case "!changeroom":
                return new Message.RetrievableMessage(Message.MessageType.USER_CHANGE_ROOM, inst[1]);
            //entrar em sala
            case "!enterroom":
                return new Message.RetrievableMessage(Message.MessageType.USER_ENTER_ROOM, inst);
            //sair de sala
            case "!leaveroom":
                return new Message.RetrievableMessage(Message.MessageType.USER_LEAVE_ROOM, inst);
            //login de utilizador
            case "!login":
                return new Message.RetrievableMessage(Message.MessageType.USER_LOGIN, new Message.UserDataMessage(inst[1], inst[2]));
            //logout de utilizador
            case "!logout":
                return new Message.RetrievableMessage(Message.MessageType.USER_LOGOUT, null);
            //registo de utilizador
            case "!register":
                return new Message.RetrievableMessage(Message.MessageType.USER_REGISTER, new Message.UserDataMessage(inst[1], inst[2]));
            //listar de que quartos está a receber
            case "!listmyrooms":
                return new Message.RetrievableMessage(Message.MessageType.USER_LIST_MY_ROOMS, null);
            //criar quarto privado
            case "!createprivateroom":
                return new Message.RetrievableMessage(Message.MessageType.USER_CREATE_PRIVATE_ROOM, inst);
            //criar quarto publico(apenas admin pode fazer isto)
            case "!createroom":
                return new Message.RetrievableMessage(Message.MessageType.USER_CREATE_PUBLIC_ROOM, inst);
            //remover quarto (apenas admin pode fazer isto)
            case "!removeroom":
                return new Message.RetrievableMessage(Message.MessageType.ADMIN_REMOVE_ROOM, inst);
            //listar quartos disponiveis a user (se for admin, lista todos)
            case "!listrooms":
                return new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM, null);
            //listar users online
            case "!listusers":
                return new Message.RetrievableMessage(Message.MessageType.USER_LIST_USERS, null);
            //listar users online no mesmo room
            case "!listroomusers":
                return new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM_USERS, null);
            //enviar mensagem privada para utilizador(es)
            case "!sendpm":
                return new Message.RetrievableMessage(Message.MessageType.USER_PRIVATE_MESSAGE, inst);
            default:
                return new Message.RetrievableMessage(Message.MessageType.DATA, input);
        }
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        boolean eof = false;
        byte b = 0;
        try {
            for (;;) {
                if (socket.read(in) <= 0) {
                    eof = true;
                }
                in.flip();
                while (in.hasRemaining()) {
                    b = in.get();
                    out.put(b);
                    if (b == '\n') {
                        break;
                    }
                }
                if (eof || b == '\n') { // send line
                    out.flip();
                    if (out.remaining() > 0) {
                        byte[] ba = new byte[out.remaining()];
                        out.get(ba);
                        out.clear();
                        dest.send(processInput(ba));
                    }
                }
                if (eof && !in.hasRemaining()) {
                    break;
                }
                in.compact();
            }
            dest.send(new Message.RetrievableMessage(Message.MessageType.EOF, null));
            return null;
        } catch (IOException e) {
            dest.send(new Message.RetrievableMessage(Message.MessageType.IOE, null));
            return null;
        }
    }

}
