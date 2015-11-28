package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.HashMap;
import java.util.Map;

//ao fazer logout estou a colocar uma entrada para null na userRoom -> pode dar problemas
//opções: entrar num sala(user), mudar de sala(user), sair de sala/logout(user), 
//apagar sala (admin), listar sala(user e admin), adicionar sala(admin) 
//talvez: apagar user?

//mensagens privadas atraves do subscribe do zeromq? ou processar no linereader? ou deixar a logica para a sala?
//criar um canal geral onde vao ser enviadas as mensagens privadas?
public class RoomManager extends BasicActor<Message.RetrievableMessage, Void> {

    //nome da sala -> actorref da sala
    Map<String, ActorRef> roomPool = new HashMap<>();
    //actorref do utilizador, actorref da sala
    Map<ActorRef, ActorRef> userRoom = new HashMap<>();

    @Override
    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {

        while (receive((Message.RetrievableMessage msg) -> {
            switch (msg.type) {
                case ROOM_ENTER:
                    //reenviar ao user o room com o qual se deve conectar
                    if (roomPool.containsKey((String) msg.o)) {
                        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.DATA, roomPool.get((String) msg.o)));
                        userRoom.put(msg.sender, roomPool.get((String) msg.o));
                        roomPool.get((String) msg.o).send(new Message.RetrievableMessage(Message.MessageType.ROOM_ENTER, msg.sender));
                    } else {
                        ActorRef new_room = new Room(self()).spawn();
                        roomPool.put((String) msg.o, new_room);
                        userRoom.put(msg.sender, new_room);
                        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.DATA, new_room));
                        new_room.send(new Message.RetrievableMessage(Message.MessageType.ROOM_ENTER, msg.sender));
                    }
                    return true;
                case ROOM_CHANGE:
                    if (userRoom.containsKey(msg.sender)) {
                        userRoom.remove(msg.sender);
                        self().send(new Message.RetrievableMessage(Message.MessageType.ROOM_ENTER, msg.o, msg.sender));

                    }
                    return true;
                case LOG_OUT:
                    return false;
            }
            return false;

        }));
        return null;
    }
}
