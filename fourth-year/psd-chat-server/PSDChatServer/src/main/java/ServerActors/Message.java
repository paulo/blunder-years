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
        //MISC
        USER_LOGIN_ACK,
        USER_REGISTER_ACK,
        USER_ENTER_ROOM_ACK,
        KO,
        LOGINCOMMAND,
        
        //User realated Event actions
        USER_FOLLOW,
        USER_LOGIN_EVENTS,
        USER_LOGOUT_EVENTS,
        USER_CREATION_EVENTS,
        USER_REMOVAL_EVENTS,
        
        //Room related Event actions
        ROOM_PRIVATE_CREATION_EVENTS,
        ROOM_PRIVATE_REMOVAL_EVENTS,
        ROOM_PRIVATE_NEWUSER_EVENTS,
        ROOM_PRIVATE_USEREXIT_EVENTS,
        
        ROOM_PUBLIC_CREATION_EVENTS,
        ROOM_PUBLIC_REMOVAL_EVENTS,
        ROOM_PUBLIC_NEWUSER_EVENTS,
        ROOM_PUBLIC_USEREXIT_EVENTS,

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
