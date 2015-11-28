package ServerActors;

import co.paralleluniverse.actors.ActorRef;

public class Message {

    public enum MessageType {
        DATA,
        EOF,
        IOE,
        LEAVE,
        ROOM_ENTER,
        ROOM_CHANGE,
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
    
    static class NonRetrivableMessage {
        MessageType type;
        final Object o;
        
        NonRetrivableMessage(MessageType t, Object obj) {
            this.type = t;
            this.o = obj;
        }
    }
    
    static class LoginMessage {
        MessageType type;
        String username;
        String password;
        
        LoginMessage(MessageType t, String un, String pw) {
            this.type = t;
            this.username = un;
            this.password = pw;
        }  
    }
}
