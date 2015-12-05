package ServerActors;

import co.paralleluniverse.actors.ActorRef;

public class Message {

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
        USER_PRIVATE_ROOM_ADD,
        USER_PRIVATE_MESSAGE,
        USER_LIST_USERS,
        USER_LIST_ROOM_USERS,
        
        //Admin actions
        ADMIN_REMOVE_ROOM,
        ADMIN_LIST_ROOM,
        
        
        
        //Simao
        USER_LOGIN_ACK,
        USER_REGISTER_ACK,
        USER_ENTER_ROOM_ACK,
        KO,
        LOGINCOMMAND,
        
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
    }
    
    static class UserDataMessage {
        String username;
        String password;
        ActorRef sender;
        
        UserDataMessage(String un, String pw, ActorRef sender) {
            this.username = un;
            this.password = pw;
            this.sender = sender;
        }  
        
        UserDataMessage(String un, String pw) {
            this.username = un;
            this.password = pw;
            this.sender = null;
        } 
    }
}
