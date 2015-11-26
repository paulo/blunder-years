package ActorChat;

import java.nio.ByteBuffer;
import java.io.IOException;
import java.net.InetSocketAddress;
import co.paralleluniverse.actors.*;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.*;

import java.util.*;

public class ChatServer {

    static int MAXLEN = 1024;

    //enum com os diferentes tipos de mensagens (facilita um bocado no selective receiving)
    static enum Type {
        DATA, EOF, IOE, ENTER, LEAVE, LINE
    }

    //classe da mensagem, com o tipo e o conteudo (basicamente um object)
    static class Msg {
        
        final Type type;
        final Object o;  // careful with mutable objects, such as the byte array

        Msg(Type type, Object o) {
            this.type = type;
            this.o = o;
        }
    }

    //actor responsavel por ler as mensagens dos sockets, processar, e enviar para o actor do utilizador
    static class LineReader extends BasicActor<Msg, Void> {

        //user actor com o qual esta associado, assim como socket e respectivos bytebuffers
        final ActorRef<Msg> dest;
        final FiberSocketChannel socket;
        ByteBuffer in = ByteBuffer.allocate(MAXLEN);
        ByteBuffer out = ByteBuffer.allocate(MAXLEN);

        LineReader(ActorRef<Msg> dest, FiberSocketChannel socket) {
            this.dest = dest;
            this.socket = socket;
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
                            dest.send(new Msg(Type.DATA, ba));
                        }
                    }
                    if (eof && !in.hasRemaining()) {
                        break;
                    }
                    in.compact();
                }
                dest.send(new Msg(Type.EOF, null));
                return null;
            } catch (IOException e) {
                dest.send(new Msg(Type.IOE, null));
                return null;
            }
        }

    }

    static class User extends BasicActor<Msg, Void> {

        final ActorRef room;
        final FiberSocketChannel socket;

        User(ActorRef room, FiberSocketChannel socket) {
            this.room = room;
            this.socket = socket;
        }

        protected Void doRun() throws InterruptedException, SuspendExecution {
            //criar um novo actor responsavel por processar a info vinda do socket
            new LineReader(self(), socket).spawn();
            //regista-se no grupo
            room.send(new Msg(Type.ENTER, self()));
            //criar um novo messageprocessor para fazer o selective receive das mensagens que recebe (em loop) do actor linereader
            //ou mesmo do actor room
            while (receive(msg -> {
                try {
                    switch (msg.type) {
                        case DATA:
                            room.send(new Msg(Type.LINE, msg.o));
                            return true;
                        case EOF:
                        case IOE:
                            room.send(new Msg(Type.LEAVE, self()));
                            socket.close();
                            return false;
                        case LINE:
                            socket.write(ByteBuffer.wrap((byte[]) msg.o));
                            return true;
                    }
                } catch (IOException e) {
                    room.send(new Msg(Type.LEAVE, self()));
                }
                return false;  // stops the actor if some unexpected message is received
            }));
            return null;
        }
    }

    static class Room extends BasicActor<Msg, Void> {

        //Set dos useres conectados, que basicamente sao actores
        private Set<ActorRef> users = new HashSet();

        //Utiliza o método receive de basicActor que recebe um messageprocessor. Basicamente isto serve para implementar um
        //selective receive. 
        protected Void doRun() throws InterruptedException, SuspendExecution {
            while (receive(msg -> {
                switch (msg.type) {
                    case ENTER:
                        users.add((ActorRef) msg.o);
                        return true;
                    case LEAVE:
                        users.remove((ActorRef) msg.o);
                        return true;
                    case LINE:
                        for (ActorRef u : users) {
                            u.send(msg);
                        }
                        return true;
                }
                return false;
            }));
            return null;
        }
    }

    //este é o actor encarregado de aceitar as conecções por socket dos clientes. A cada conecção cria um novo actor cliente
    //(e portanto, uma nova fiber) e associa-o ao room criado anteriormente
    static class Acceptor extends BasicActor {

        final int port;
        final ActorRef room;

        Acceptor(int port, ActorRef room) {
            this.port = port;
            this.room = room;
        }

        protected Void doRun() throws InterruptedException, SuspendExecution {
            try {
                FiberServerSocketChannel ss = FiberServerSocketChannel.open();
                ss.bind(new InetSocketAddress(port));
                while (true) {
                    FiberSocketChannel socket = ss.accept();
                    new User(room, socket).spawn();
                }
            } catch (IOException e) {
            }
            return null;
        }
    }

    public static void main(String[] args) throws Exception {
        int port = 12345; //Integer.parseInt(args[0]);
        ActorRef room = new Room().spawn();
        Acceptor acceptor = new Acceptor(port, room);
        acceptor.spawn();
        acceptor.join();
    }
}