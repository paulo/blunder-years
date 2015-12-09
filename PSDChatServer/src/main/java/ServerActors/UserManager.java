package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.actors.MailboxConfig;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.*;

public class UserManager extends BasicActor<Message.RetrievableMessage, Void> {

    private final Map<String, UserInfo> userPool;
    ActorRef event_publisher;


    public UserManager(ActorRef event_publisher) {
        super("user_manager", new MailboxConfig(Message.USERMANAGER_BOX_LIMIT, Message.BOX_POLICY));
        this.userPool = new HashMap<>();
        this.event_publisher = event_publisher;
    }
    
    private void addUser(String user_name, UserInfo info) {
        userPool.put(user_name, info);
    }

    private void logOutUser(Message.RetrievableMessage msg) throws SuspendExecution {
        String username = (String) msg.o;
        if (userPool.containsKey(username)) {
            userPool.get(username).setIsLoggedIn(false);
            userPool.get(username).setUser_actor(null);
             msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, "Logged out sucessfully.\n".getBytes()));
        }
    }

    private void createTestUsers() {
        addUser("user1", new UserInfo("user1", "pass"));
        addUser("user2", new UserInfo("user2", "pass"));
        addUser("user3", new UserInfo("user3", "pass"));
        addUser("admin", new UserInfo("admin", "admin", false, null, true));
    }

    //fazer controlo de erros para casos em que nao tem users na frase
    //talvez mudar este tipo de metodos para usar string builder ou algo mais rapido
    @SuppressWarnings("empty-statement")
    private void sendPrivateMessage(Message.RetrievableMessage msg) throws SuspendExecution {
        String[] args = (String[]) msg.o;
        List<String> dest_users = new ArrayList<>();
        String data = "(pm)" + args[0] + ": ";

        int i = 1;

        while (i < args.length) {
            if (args[i].startsWith("@")) {
                dest_users.add(args[i]);
                i++;
            } else {
                break;
            }
        }

        while (i < args.length) {
            data = data.concat(args[i]);
            i++;
        }
        data = data.concat("\n");

        for (String s : dest_users) {
            userPool.get(s.substring(1)).getUser_actor().send(new Message.RetrievableMessage(Message.MessageType.LINE, data.getBytes()));
        }
    }

    private void userLogin(Message.RetrievableMessage msg) throws SuspendExecution {
        Message.UserDataMessage data = (Message.UserDataMessage) msg.o;
        String username = (String) data.username;
        String password = (String) data.userdata;

        if (userPool.containsKey(username) && userPool.get(username).getPassword().equals(password)) {
            if (userPool.get(username).isIsLoggedIn()==false) {  
                UserInfo ui = userPool.get(username);
                ui.setIsLoggedIn(true);
                ui.setUser_actor((ActorRef) msg.sender);
                if (userPool.get(data.username).isIsAdmin()) {
                    msg.sender.send(new Message.RetrievableMessage(Message.MessageType.ADMIN_LOGIN_ACK, "Logged in sucessfully!\n".getBytes()));
                    event_publisher.send(new Message.RetrievableMessage(Message.MessageType.DATA, "@USERMANAGER: " + username + " has logged in as administrator.\n"));
                } else {
                    msg.sender.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGIN_ACK, "Logged in sucessfully!\n".getBytes()));
                    event_publisher.send(new Message.RetrievableMessage(Message.MessageType.DATA, "@USERMANAGER: " + username + " has logged in.\n"));
                }
            } else msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, ("User "+username+" is already logged in!\n").getBytes()));
        } else {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, "Login information incorrect!\n".getBytes()));
        }
    }

    private void userRegister(Message.RetrievableMessage msg) throws SuspendExecution {
        Message.UserDataMessage data = (Message.UserDataMessage) msg.o;
        if (!userPool.containsKey(data.username)) {
            userPool.put(data.username, new UserInfo((String) data.username, (String) data.userdata));
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.USER_REGISTER_ACK, "Account Created Sucessfully.\n".getBytes()));
            event_publisher.send(new Message.RetrievableMessage(Message.MessageType.DATA, "@USERMANAGER: New account creater: " + data.username +"\n"));
        } else {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, "Error: Username already taken.\n".getBytes()));
        }
    }

    private void listOnlineUsers(Message.RetrievableMessage msg) throws SuspendExecution {
        String online = "Online users:\n";
        int i = 0;
        for (UserInfo ui : this.userPool.values()) {
            if (ui.isLoggedIn) {
                online = online.concat(ui.getUsername() + "\n");
                i++;
            }
        }
        if (i == 0) {
            online = "No users online.\n";
        } else {
            online = online.concat("Total: " + i + " users.\n");
        }
        msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, online.getBytes()));
    }

    @Override
    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {
        createTestUsers();

        while (receive(msg -> {

            switch (msg.type) {
                case USER_REGISTER:
                    userRegister(msg);
                    return true;
                case USER_LOGIN:
                    userLogin(msg);
                    return true;
                case USER_PRIVATE_MESSAGE:
                    sendPrivateMessage(msg);
                    return true;
                case USER_LIST_USERS:
                    listOnlineUsers(msg);
                    return true;
                case USER_LOGOUT:
                    logOutUser(msg);
                    return true;
            }
            return false;
        }));
        return null;
    }

}
