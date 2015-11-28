package CodeExamples;

import java.nio.ByteBuffer;
import java.io.IOException;
import java.net.InetSocketAddress;
import co.paralleluniverse.actors.*;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.Suspendable;
import co.paralleluniverse.fibers.io.*;
import java.io.UnsupportedEncodingException;

import java.util.*;

public class ChatServerV2 {

    static int MAXLEN = 1024;

    static enum Type {

        DATA, EOF, IOE, LEAVE,
        ROOM_ENTER, ROOM_CHANGE, LINE //tipos para room actor
    }

    static class RetrievableMessage {

        final Type type;
        final Object o;
        final ActorRef sender;

        RetrievableMessage(Type t, Object obj, ActorRef sender) {
            this.type = t;
            this.o = obj;
            this.sender = sender;
        }

        RetrievableMessage(Type t, Object obj) {
            this.type = t;
            this.o = obj;
            this.sender = null;
        }
    }

    static class LineReader extends BasicActor<RetrievableMessage, Void> {

        final ActorRef<RetrievableMessage> dest;
        final FiberSocketChannel socket;
        ByteBuffer in = ByteBuffer.allocate(MAXLEN);
        ByteBuffer out = ByteBuffer.allocate(MAXLEN);

        LineReader(ActorRef<RetrievableMessage> dest, FiberSocketChannel socket) {
            this.dest = dest;
            this.socket = socket;
        }

        private RetrievableMessage processInput(byte[] input) throws UnsupportedEncodingException {
            String decoded = new String(input, "UTF-8");

            String[] inst = decoded.split(" ");

            switch (inst[0]) {
                case "changeroom":
                    return new RetrievableMessage(Type.ROOM_CHANGE, inst[1]);
                case "enterroom":
                    return new RetrievableMessage(Type.ROOM_ENTER, inst[1]);
                default:
                    System.out.println("Recebido: "+ input);
                    return new RetrievableMessage(Type.DATA, input);
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
                dest.send(new RetrievableMessage(Type.EOF, null));
                return null;
            } catch (IOException e) {
                dest.send(new RetrievableMessage(Type.IOE, null));
                return null;
            }
        }

    }

    //o objetivo era criar uma abstraçao do user ao manager, mas tornou-se bastante complicado e mudei de ideias
    static class User extends BasicActor<RetrievableMessage, Void> {

        private ActorRef roomManager;
        final FiberSocketChannel socket;
        private ActorRef room;

        User(ActorRef room, FiberSocketChannel socket) {
            this.roomManager = room;
            this.socket = socket;
        }

        protected Void doRun() throws InterruptedException, SuspendExecution {

            new LineReader(self(), socket).spawn();
            roomManager.send(new RetrievableMessage(Type.ROOM_ENTER, "default", self()));
            room = (ActorRef) receive().o;
            if(room != null) System.out.println("recebido");
                    else System.out.println("nao recebido");
            //room = new Room(roomManager).spawn();
            
            while (receive((RetrievableMessage msg) -> {
                try {
                    switch (msg.type) {
                        case DATA:
                            room.send(new RetrievableMessage(Type.LINE, msg.o));
                            return true;
                        case ROOM_CHANGE:
                            room.send(new RetrievableMessage(Type.ROOM_CHANGE, msg.o, self()));
                            return true;
                        case EOF:
                            return true;
                        case IOE:
                            room.send(new RetrievableMessage(Type.LEAVE, self()));
                            socket.close();
                            return false;
                        case LINE:
                            socket.write(ByteBuffer.wrap((byte[]) msg.o));
                            return true;
                    }
                } catch (IOException e) {
                    room.send(new RetrievableMessage(Type.LEAVE, self()));
                }
                return false;  // stops the actor if some unexpected message is received
            }));
            return null;
        }
    }
    
    

    //ao fazer logout estou a colocar uma entrada para null na userRoom -> pode dar problemas
    static class RoomManager extends BasicActor<RetrievableMessage, Void> {

        //map de nomes de rooms para actorrefs de roomactors
        //msg.o é o nome do room
        Map<String, ActorRef> roomPool = new HashMap<>();
        Map<ActorRef, ActorRef> userRoom = new HashMap<>();

        @Override
        protected Void doRun() throws InterruptedException, SuspendExecution {

            while (receive((RetrievableMessage msg) -> {
                switch (msg.type) {
                    case ROOM_ENTER:
                        if (roomPool.containsKey((String) msg.o)) {
                            //reenviar ao user o room com o qual se deve conectar
                            msg.sender.send(new RetrievableMessage(Type.DATA, roomPool.get((String) msg.o)));
                            userRoom.put(msg.sender, roomPool.get((String) msg.o));
                            roomPool.get((String) msg.o).send(new RetrievableMessage(Type.ROOM_ENTER, msg.sender));
                        } else {
                            ActorRef new_room = new Room(self()).spawn();
                            roomPool.put((String) msg.o, new_room);
                            userRoom.put(msg.sender, new_room);
                            msg.sender.send(new RetrievableMessage(Type.DATA, new_room));
                            new_room.send(new RetrievableMessage(Type.ROOM_ENTER, msg.sender));
                        }
                        return true;
                    case ROOM_CHANGE:
                        if (userRoom.containsKey(msg.sender)) {
                            userRoom.remove(msg.sender);
                                self().send(new RetrievableMessage(Type.ROOM_ENTER, msg.o, msg.sender));

                        }
                        return true;
                }
                return false;

            }));
            return null;
        }
    }
    
    
    //Pode receber 3 tipos de mensagens: Entrada de utilizador, Saída de Utilizador, Escrita de Mensagem para todos os utilizadores
    static class Room extends BasicActor<RetrievableMessage, Void> {

        private Set<ActorRef> users;
        private ActorRef manager;

        public Room(ActorRef room_manager) {
            this.users = new HashSet();
            this.manager = room_manager;
        }

        protected Void doRun() throws InterruptedException, SuspendExecution {

            while (receive(msg -> {
                switch (msg.type) {
                    case ROOM_ENTER:
                        users.add((ActorRef) msg.o);
                        return true;
                    case ROOM_CHANGE:
                        users.remove((ActorRef) msg.sender);
                        manager.send(msg);
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

 
    
    //Ciclo while para aceitar conexões e criar novos users
    static class Acceptor extends BasicActor<RetrievableMessage, Void> {

        final int port;
        final ActorRef roomManager;

        Acceptor(int port, ActorRef roomManager) {
            this.port = port;
            this.roomManager = roomManager;
        }

        @Override
        protected Void doRun() throws InterruptedException, SuspendExecution {
            try {
                FiberServerSocketChannel ss = FiberServerSocketChannel.open();
                ss.bind(new InetSocketAddress(port));
                while (true) {
                    FiberSocketChannel socket = ss.accept();
                    new User(roomManager, socket).spawn();
                }
            } catch (IOException e) {
            }
            return null;
        }
    }

    
    
    
    public static void main(String[] args) throws Exception {
        int port = 12345; //Integer.parseInt(args[0]);

        ActorRef roomManager = new RoomManager().spawn();
        Acceptor acceptor = new Acceptor(port, roomManager);
        acceptor.spawn();
        acceptor.join();
    }

}
