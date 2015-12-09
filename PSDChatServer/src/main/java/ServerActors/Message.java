package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.strands.channels.Channels;

public class Message {

    static int USER_BOX_LIMIT = 100;
    static int ROOM_BOX_LIMIT = 1000;
    static int ROOMMANAGER_BOX_LIMIT = 10000;
    static int USERMANAGER_BOX_LIMIT = 10000;
    
    static Channels.OverflowPolicy BOX_POLICY = Channels.OverflowPolicy.DROP;
    
    
    public enum MessageType {

        DATA,
        EOF,
        IOE,
        //User actions
        USER_LOGIN,
        USER_LOGOUT,
        USER_REGISTER,
        USER_ENTER_ROOM,
        USER_LEAVE_ROOM,
        USER_CHANGE_ROOM,
        USER_LIST_ROOM,
        USER_CREATE_PUBLIC_ROOM,
        USER_CREATE_PRIVATE_ROOM,
        USER_PRIVATE_MESSAGE,
        USER_LIST_USERS,
        USER_LIST_ROOM_USERS,
        USER_LIST_MY_ROOMS,
        //Admin actions
        ADMIN_REMOVE_ROOM,
        ADMIN_LIST_ROOM,
        ADMIN_LOGIN_ACK,
        ADMIN_REMOVE_ROOM_ACK,
        //Simao
        USER_LOGIN_ACK,
        USER_REGISTER_ACK,
        USER_ENTER_ROOM_ACK,
        KO,
        LOGINCOMMAND,
        SUBSCRIBE,
        BECOME_NOTIFICATION_CONSOLE,

        LINE,
        LOG_OUT
    }

    static class RetrievableMessage {

        MessageType type;
        final Object o;
        final ActorRef sender;

        RetrievableMessage(MessageType t, Object obj, ActorRef sender) {
            this.type = t;
            this.o = obj;
            this.sender = sender;
        }

        RetrievableMessage(MessageType t, Object obj) {
            this.type = t;
            this.o = obj;
            this.sender = null;
        }
    }

    static class NonRetrievableMessage {

        MessageType type;
        final Object o;

        NonRetrievableMessage(MessageType t, Object obj) {
            this.type = t;
            this.o = obj;
        }
        
        NonRetrievableMessage(MessageType t){
            this.type = t;
            this.o = null;
        }
    }

    static class UserDataMessage {

        String username;
        Object userdata;
        ActorRef sender;

        UserDataMessage(String un, Object ud, ActorRef sender) {
            this.username = un;
            this.userdata = ud;
            this.sender = sender;
        }

        UserDataMessage(String un, Object ud) {
            this.username = un;
            this.userdata = ud;
            this.sender = null;
        }
    }
}
