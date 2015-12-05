package ServerActors;

import static ServerActors.User.MAXLEN;
import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.actors.MailboxConfig;
import co.paralleluniverse.actors.behaviors.EventSource;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import co.paralleluniverse.strands.channels.Channels;
import com.google.common.collect.Iterables;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.*;

//talvez passar isto para nonRetrivableMessage
//falta fazer os checks sobre se está loggedIn ou não
public class User extends BasicActor<Message.RetrievableMessage, Void> {

    static int MAXLEN = 1024, USER_BOX_LIMIT = 100;
    static Channels.OverflowPolicy box_policy = Channels.OverflowPolicy.DROP;

    private String username;
    private String password;
    public boolean isLoggedIn;
    private Map<String, ActorRef> rooms;
    private ActorRef writing_room;
    private String temp_user, temp_pass;

    private ActorRef room_manager;
    final FiberSocketChannel socket;
    private EventSource eventsource;
    private ActorRef user_manager;

    User(String actor_id, ActorRef room, ActorRef user_manager, FiberSocketChannel socket, EventSource es) {
        super(actor_id, new MailboxConfig(USER_BOX_LIMIT, box_policy));
        this.room_manager = room;
        this.socket = socket;
        this.eventsource = es;
        this.isLoggedIn = false;
        this.user_manager = user_manager;
        this.rooms = new HashMap<>();
        this.temp_pass = temp_user = null;
    }

    private void userLogout() throws SuspendExecution {
        if (this.isLoggedIn) {
            for (ActorRef room : this.rooms.values()) {
                room.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGOUT, this.username));
            }
            user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGOUT, this.username, self()));
            this.writing_room = null;
            this.username = this.password = this.temp_pass = this.temp_user = null;
            this.rooms.clear();
            this.isLoggedIn = false;
        }
    }

    private void userLogin(Message.UserDataMessage msg) throws SuspendExecution {
        this.temp_user = msg.username;
        this.temp_pass = msg.password;
        user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGIN, msg, self()));
    }

    private void userRegister(Message.RetrievableMessage msg) throws SuspendExecution {
        Message.UserDataMessage data = (Message.UserDataMessage) msg.o;
        user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_REGISTER, data, self()));
    }

    private void enterGlobalRoom() throws SuspendExecution {
        room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_ENTER_ROOM,
                new Message.UserDataMessage(this.username, "global_room"), self()));
    }

    /*private void listRoom() throws SuspendExecution {
     if (user_info.isIsAdmin()) {
     room_manager.send(new Message.RetrievableMessage(Message.MessageType.ADMIN_LIST_ROOM, null, self()));
     } else {
     room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM, null, self()));
     }
     }*/
    /*private void listRoomUsers() throws SuspendExecution {
     room.send(new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM_USERS, null, self()));
     }*/

    /*private void sendPrivateMessage(Message.RetrievableMessage msg) throws SuspendExecution {
     String[] args = (String[]) msg.o;
     args[0] = user_info.getUsername();

     user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_PRIVATE_MESSAGE, args, self()));
     }*/
    private void writeToSocket(Message.RetrievableMessage msg) throws IOException, SuspendExecution {
        socket.write(ByteBuffer.wrap((byte[]) msg.o));
    }

    private void writeStringToSocket(String msg) throws IOException, SuspendExecution {
        socket.write(ByteBuffer.wrap((byte[]) msg.getBytes()));
    }

    //fazer controlo de erros para quando a sala nao existe (talvez implementar do lado do room_manager)
    private void changeRoom(Message.RetrievableMessage msg) throws IOException, SuspendExecution {
        String room_name = (String) msg.o;
        if (this.rooms.containsKey(room_name)) {
            this.writing_room = this.rooms.get(room_name);
            writeStringToSocket("Now writing to: " + room_name);
        } else {
            //aqui falta ficar a escrever para a sala em que se entrou, mas talvez deixar assim
            enterRoom(msg);
        }
    }

    private void enterRoom(Message.RetrievableMessage msg) throws SuspendExecution {
        room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_ENTER_ROOM,
                new Message.UserDataMessage(this.username, (String) msg.o), self()));
    }

    private void leaveRoom(Message.RetrievableMessage msg) throws SuspendExecution, IOException {
        String room_name = (String) msg.o;
        if (this.rooms.containsKey(room_name)) {
            if (this.rooms.size() > 1) {
                this.rooms.remove(room_name);
                this.writing_room = Iterables.get(this.rooms.values(), 0);
                this.rooms.get(room_name).send(new Message.RetrievableMessage(Message.MessageType.USER_LEAVE_ROOM, this.username));
            } else {
                writeStringToSocket("There has to be at least another room in which you're in.");
            }
        } else {
            writeStringToSocket("You are not in the room: " + room_name);
        }
    }

    private void listMyRooms() throws IOException, SuspendExecution {
        String listening_list = "Listening to rooms:\n";
        for (String s : this.rooms.keySet()) {
            listening_list = listening_list.concat(s + "\n");
        }

        writeStringToSocket(listening_list);
    }

    //Mudar aqui para meter o username de actor na mensagem
    //Mudar para apenas mandar mensagem para o room em que está a escrever
    private void sendMessageToRooms(Message.RetrievableMessage msg) throws SuspendExecution {
        for (ActorRef room : rooms.values()) {
            room.send(new Message.RetrievableMessage(Message.MessageType.LINE, msg.o));
        }
    }

    @SuppressWarnings("empty-statement")
    //talvez meter a logica dos acks nos metodos de envio para os managers
    protected Void doRun() throws InterruptedException, SuspendExecution {

        new LineReader(self(), socket).spawn();
        //room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_ENTER_ROOM, "global_room", self()));
        //room = (ActorRef) receive().o;
        //eventsource.notify("Nova conexão");

        while (receive((Message.RetrievableMessage msg) -> {
            try {
                switch (msg.type) {
                    //case USER_LIST_ROOM:
                    //listRoom();
                    //    return true;
                    //case USER_LIST_ROOM_USERS:
                    //listRoomUsers();
                    //    return true;
                    //case USER_PRIVATE_MESSAGE:
                    //sendPrivateMessage(msg);
                    //    return true;
                    case DATA:
                        sendMessageToRooms(msg);
                        //eventsource.notify("Enviada linha");
                        //room.send(new Message.RetrievableMessage(Message.MessageType.LINE, msg.o));
                        return true;
                    case USER_ENTER_ROOM: //implementar (nao obriga o utilizador a sair da sala)
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
                        userLogin(new Message.UserDataMessage(this.username, this.password));
                        return true;
                    case USER_LOGIN_ACK:
                        this.username = this.temp_user;
                        this.password = this.temp_pass;
                        enterGlobalRoom();
                        return true;
                    case USER_ENTER_ROOM_ACK:
                        this.rooms.put((String) msg.o, msg.sender);
                        writeStringToSocket("Now receiving messages from: " + (String) msg.o);
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
                    case LINE:
                        writeToSocket(msg);
                        return true;

                    /*case REMOVE:
                     login.send(new Message.RetrievableMessage(Message.MessageType.REMOVE, new Message.LoginMessage(Message.MessageType.OK, ((Message.LoginMessage) msg.o).username, ((Message.LoginMessage) msg.o).password, self())));
                     return true;
                     */
                    case EOF:
                        return true;
                    case IOE:
                        //room.send(new Message.RetrievableMessage(Message.MessageType.USER_LEAVE_ROOM, self()));
                        socket.close();
                        return false;
                }
            } catch (IOException e) {
                //room.send(new Message.RetrievableMessage(Message.MessageType.USER_LEAVE_ROOM, self()));
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
        String decoded = new String(input, "UTF-8");

        String[] inst = decoded.split(" ");

        switch (inst[0]) {
            //mudar sala de escrita
            case "!changeroom":
                return new Message.RetrievableMessage(Message.MessageType.USER_CHANGE_ROOM, inst[1]);
            //entrar em sala
            case "!enterroom":
                return new Message.RetrievableMessage(Message.MessageType.USER_ENTER_ROOM, inst[1]);
            //sair de sala
            case "!leaveroom":
                return new Message.RetrievableMessage(Message.MessageType.USER_LEAVE_ROOM, inst[1]);
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
            //listar quartos disponiveis a user (se for admin, lista todos)
            //case "!listrooms":
            //    return new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM, null);
            //listar users online
            //case "!listusers":
            //    return null;
            //case "!listroomusers":
            //    return new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM_USERS, null);
            //enviar mensagem privada para utilizador(es)
            //case "!sendpm":
            //    return new Message.RetrievableMessage(Message.MessageType.USER_PRIVATE_MESSAGE, inst);
            //criar quarto privado com a lista de utilizadores que estão à frente
            //case "!createprivateroom":
            //    return null;
            //adicionar utilizador a quarto privado
            //case "!adduser":
            //    return null;
            //criar quarto publico(apenas admin pode fazer isto)
            //case "!createroom":
            //    return new Message.RetrievableMessage(Message.MessageType.CREATE, new Message.LoginMessage(Message.MessageType.OK, inst[1], inst[2]));
            //remover quarto (apenas admin pode fazer isto)
            //case "!removeroom":
            //    return new Message.RetrievableMessage(Message.MessageType.REMOVE, new Message.LoginMessage(Message.MessageType.OK, inst[1], inst[2]));
            //notificar-me de criação de quartos (apenas para consola de notificação)
            //case "?roomcreation":
            //    return null;
            //notificar-me da entrada de um dado user (apenas para consola de notificação)
            //case "?userlogin":
            //    return null;
            //notificar-me da remoção de quartos (apenas para consola de notificação)
            //case "?roomremoval":
            //    return null;
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
