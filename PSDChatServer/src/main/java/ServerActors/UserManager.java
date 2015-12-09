package ServerActors;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.*;

//falta meter o user actor no userinfo sempre que o utilizador faz login
public class UserManager extends BasicActor<Message.RetrievableMessage, Void> {

    private final Map<String, UserInfo> userPool;
    ActorRef event_publisher;

    public UserManager(ActorRef event_publisher) {
        this.userPool = new HashMap<>();
        this.event_publisher = event_publisher;
    }

    public UserInfo getUser(String user_name) {
        if (userPool.containsKey(user_name)) {
            return userPool.get(user_name);
        } else {
            return null;
        }
    }

    public boolean hasUser(String user_name) {
        return userPool.containsKey(user_name);
    }

    public void addUser(String user_name, UserInfo info) {
        userPool.put(user_name, info);
    }

    //atualizar userINFO e meter loggedout e useractor a null
    public void logOutUser(String username) {
        if (userPool.containsKey(username)) {
            userPool.get(username).setIsLoggedIn(false);
            userPool.get(username).setUser_actor(null);
        }
    }

    public void createTestUsers(){
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
        String data = "(pm)"+args[0]+": ";
        
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

        for (String s : dest_users) {
            userPool.get(s.substring(1)).getUser_actor().send(new Message.RetrievableMessage(Message.MessageType.LINE, data.getBytes()));
        }
    }

    //meter controlo para saber se o user já está logged in´
    private void userLogin(Message.RetrievableMessage msg) throws SuspendExecution {
        Message.UserDataMessage data = (Message.UserDataMessage) msg.o;
        String username = (String) data.username; String password = (String) data.password;

        if (userPool.containsKey(username) && userPool.get(username).getPassword().equals(password)) {
            UserInfo ui = userPool.get(username);
            ui.setIsLoggedIn(true); 
            ui.setUser_actor((ActorRef) msg.sender);
            if(userPool.get(data.username).isIsAdmin()) {
                msg.sender.send(new Message.RetrievableMessage(Message.MessageType.ADMIN_LOGIN_ACK, "Logged in sucessfully!\n".getBytes()));
                event_publisher.send(new Message.RetrievableMessage(Message.MessageType.DATA, "@USERMANAGER: " + data.username + " has logged in as administrator.\n"));
            } 
            
            else {
                msg.sender.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGIN_ACK, "Logged in sucessfully!\n".getBytes()));
                event_publisher.send(new Message.RetrievableMessage(Message.MessageType.DATA, "@USERMANAGER: " + data.username + " has logged in.\n"));
            }
        } else {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, "Login information incorrect!\n".getBytes()));
        }
    }

    //meter controlo para ver se o user já existe ou se user ja esta logged in (nao pode registar-se quando está logged in)
    private void userRegister(Message.RetrievableMessage msg) throws SuspendExecution {
        Message.UserDataMessage data = (Message.UserDataMessage) msg.o;
        if (!userPool.containsKey(data.username)) {
            userPool.put(data.username, new UserInfo(data.username, data.password));
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.USER_REGISTER_ACK, "Account Created Sucessfully.\n".getBytes()));
            event_publisher.send(new Message.RetrievableMessage(Message.MessageType.DATA, "@USERMANAGER: New account creater: " + data.username +"\n"));
        } else {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, "Error: Username already taken.\n".getBytes()));
        }
    }
    
    private void listOnlineUsers(Message.RetrievableMessage msg) throws SuspendExecution {
        String online = "Online users:\n";
        int i = 0;
        for(UserInfo ui : this.userPool.values()){
            if(ui.isLoggedIn) {
                online = online.concat(ui.getUsername()+"\n");
                i++;
            }
        }
        if (i==0) online = "No users online.\n";
        else online = online.concat("Total: "+i+" users.\n");
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
                case USER_PRIVATE_MESSAGE: //falta testar
                    sendPrivateMessage(msg);
                    return true;
                case USER_LIST_USERS:
                    listOnlineUsers(msg);
                    return true;
                case USER_LOGOUT:
                    logOutUser((String) msg.o);
                    return true;
                /*case REMOVE:
                 if (loginMap.containsKey(data.username)
                 && loginMap.get(data.username).equals(data.password)) {
                 loginMap.remove(data.username);
                 data.sender.send(new Message.NonRetrivableMessage(Message.MessageType.OK, "Account Deleted Sucessfully"));
                 } else {
                 data.sender.send(new Message.NonRetrivableMessage(Message.MessageType.KO, "Information incorrect!"));
                 }*/

            }
            return false;
        }));
        return null;
    }

}
