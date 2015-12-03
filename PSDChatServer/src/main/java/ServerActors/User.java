package ServerActors;

import static ServerActors.User.MAXLEN;
import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.actors.MailboxConfig;
import co.paralleluniverse.actors.behaviors.EventSource;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import co.paralleluniverse.strands.channels.Channels;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;

//talvez passar isto para nonRetrivableMessage
//falta fazer os checks sobre se está loggedIn ou não
public class User extends BasicActor<Message.RetrievableMessage, Void> {

    static int MAXLEN = 1024, USER_BOX_LIMIT = 100;
    static Channels.OverflowPolicy box_policy = Channels.OverflowPolicy.DROP;

    private UserInfo user_info;

    private ActorRef room_manager;
    final FiberSocketChannel socket;
    private ActorRef room;
    private EventSource eventsource;
    private ActorRef user_manager;
    public boolean isLoggedIn;

    User(String actor_id, ActorRef room, ActorRef user_manager, FiberSocketChannel socket, EventSource es) {
        super(actor_id, new MailboxConfig(USER_BOX_LIMIT, box_policy));
        this.room_manager = room;
        this.socket = socket;
        this.eventsource = es;
        this.isLoggedIn = false;
        this.user_manager = user_manager;
    }

    private void userLogout() throws SuspendExecution {
        if (isLoggedIn) {
            user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGOUT, user_info.getUsername()));
            room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGOUT, self()));
        }
    }

    private void userLogin(Message.RetrievableMessage msg) throws SuspendExecution {
        user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGIN, new Message.LoginMessage(Message.MessageType.OK, ((Message.LoginMessage) msg.o).username, ((Message.LoginMessage) msg.o).password, self())));
    }

    private void userRegister(Message.RetrievableMessage msg) throws SuspendExecution {
        user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_REGISTER, new Message.LoginMessage(Message.MessageType.OK, ((Message.LoginMessage) msg.o).username, ((Message.LoginMessage) msg.o).password, self())));
    }

    private void listRoom() throws SuspendExecution {
        if (user_info.isIsAdmin()) {
            room_manager.send(new Message.RetrievableMessage(Message.MessageType.ADMIN_LIST_ROOM, null, self()));
        } else {
            room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM, null, self()));
        }
    }

    private void listRoomUsers() throws SuspendExecution {
        room.send(new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM_USERS, null, self()));
    }
    
    private void sendPrivateMessage(Message.RetrievableMessage msg) throws SuspendExecution {
        String[] args = (String[]) msg.o;
        args[0] = user_info.getUsername();
        
        user_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_PRIVATE_MESSAGE, args, self()));
    }

    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {

        new LineReader(self(), socket).spawn();
        room_manager.send(new Message.RetrievableMessage(Message.MessageType.USER_ENTER_ROOM, "global_room", self()));
        room = (ActorRef) receive().o;
        //eventsource.notify("Nova conexão");

        while (receive((Message.RetrievableMessage msg) -> {
            try {
                switch (msg.type) {
                    case USER_LIST_ROOM:
                        listRoom();
                        return true;
                    case USER_LIST_ROOM_USERS:
                        listRoomUsers();
                        return true;
                    case USER_PRIVATE_MESSAGE:
                        sendPrivateMessage(msg);
                        return true;
                    case DATA:
                        //eventsource.notify("Enviada linha");
                        room.send(new Message.RetrievableMessage(Message.MessageType.LINE, msg.o));
                        return true;
                    case USER_CHANGE_ROOM:
                        room.send(new Message.RetrievableMessage(Message.MessageType.USER_CHANGE_ROOM, msg.o, self()));
                        return true;
                    case EOF:
                        return true;
                    case IOE:
                        room.send(new Message.RetrievableMessage(Message.MessageType.USER_LEAVE_ROOM, self()));
                        socket.close();
                        return false;
                    case USER_REGISTER:
                        userRegister(msg);
                        return true;
                    case USER_LOGIN:
                        userLogin(msg);
                        return true;
                    case USER_LOGOUT:
                        userLogout();
                        return true;
                    case LINE:
                        socket.write(ByteBuffer.wrap((byte[]) msg.o));
                        return true;
                    /*case REMOVE:
                     login.send(new Message.RetrievableMessage(Message.MessageType.REMOVE, new Message.LoginMessage(Message.MessageType.OK, ((Message.LoginMessage) msg.o).username, ((Message.LoginMessage) msg.o).password, self())));
                     return true;
                     */
                }
            } catch (IOException e) {
                room.send(new Message.RetrievableMessage(Message.MessageType.USER_LEAVE_ROOM, self()));
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
            //listar quartos disponiveis a user (se for admin, lista todos)
            case "!listrooms":
                return new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM, null);
            //listar users online
            case "!listusers":
                return null;
            case "!listroomusers":
                return new Message.RetrievableMessage(Message.MessageType.USER_LIST_ROOM_USERS, null);
            //enviar mensagem privada para utilizador(es)
            case "!sendpm":                
                return new Message.RetrievableMessage(Message.MessageType.USER_PRIVATE_MESSAGE, inst);
            //criar quarto privado com a lista de utilizadores que estão à frente
            case "!createprivateroom":
                return null;
            //adicionar utilizador a quarto privado
            case "!adduser":
                return null;
            //case "!changeroom":
            //    return new Message.RetrievableMessage(Message.MessageType.ROOM_CHANGE, inst[1]);
            //case "!enterroom":
            //    return new Message.RetrievableMessage(Message.MessageType.ROOM_ENTER, inst[1]);
            //login de utilizador
            case "!login":
                return new Message.RetrievableMessage(Message.MessageType.USER_LOGIN, new Message.LoginMessage(Message.MessageType.OK, inst[1], inst[2]));
            //logout de utilizador
            case "!logout":
                return new Message.RetrievableMessage(Message.MessageType.USER_LOGOUT, null);
            case "!register":
                return null;
            //criar quarto publico(apenas admin pode fazer isto)
            //case "!createroom":
            //    return new Message.RetrievableMessage(Message.MessageType.CREATE, new Message.LoginMessage(Message.MessageType.OK, inst[1], inst[2]));
            //remover quarto (apenas admin pode fazer isto)
            //case "!removeroom":
            //    return new Message.RetrievableMessage(Message.MessageType.REMOVE, new Message.LoginMessage(Message.MessageType.OK, inst[1], inst[2]));
            //notificar-me de criação de quartos (apenas para consola de notificação)
            case "?roomcreation":
                return null;
            //notificar-me da entrada de um dado user (apenas para consola de notificação)
            case "?userlogin":
                return null;
            //notificar-me da remoção de quartos (apenas para consola de notificação)
            case "?roomremoval":
                return null;
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
