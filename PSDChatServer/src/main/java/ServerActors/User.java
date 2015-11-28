package ServerActors;

import static ServerActors.User.MAXLEN;
import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;

//o objetivo era criar uma abstraçao do user ao manager, mas tornou-se bastante complicado e mudei de ideias
public class User extends BasicActor<Message.RetrievableMessage, Void> {

    static int MAXLEN = 1024;
    private ActorRef roomManager;
    final FiberSocketChannel socket;
    private ActorRef room;

    User(ActorRef room, FiberSocketChannel socket) {
        this.roomManager = room;
        this.socket = socket;
    }

    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {

        new LineReader(self(), socket).spawn();
        roomManager.send(new Message.RetrievableMessage(Message.MessageType.ROOM_ENTER, "default", self()));
        room = (ActorRef) receive().o;

        while (receive((Message.RetrievableMessage msg) -> {
            try {
                switch (msg.type) {
                    case DATA:
                        room.send(new Message.RetrievableMessage(Message.MessageType.LINE, msg.o));
                        return true;
                    case ROOM_CHANGE:
                        room.send(new Message.RetrievableMessage(Message.MessageType.ROOM_CHANGE, msg.o, self()));
                        return true;
                    case EOF:
                        return true;
                    case IOE:
                        room.send(new Message.RetrievableMessage(Message.MessageType.LEAVE, self()));
                        socket.close();
                        return false;
                    case LINE:
                        socket.write(ByteBuffer.wrap((byte[]) msg.o));
                        return true;
                }
            } catch (IOException e) {
                room.send(new Message.RetrievableMessage(Message.MessageType.LEAVE, self()));
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
            case "changeroom":
                return new Message.RetrievableMessage(Message.MessageType.ROOM_CHANGE, inst[1]);
            case "enterroom":
                return new Message.RetrievableMessage(Message.MessageType.ROOM_ENTER, inst[1]);
            default:
                return new Message.RetrievableMessage(Message.MessageType.DATA, input);
        }
    }

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
