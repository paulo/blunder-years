package ServerActors;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.*;

//falta meter o user actor no userinfo sempre que o utilizador faz login
public class UserManager extends BasicActor<Message.RetrievableMessage, Void> {

    private Map<String, UserInfo> userPool;

    public UserManager() {
        this.userPool = new HashMap<>();
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
        }
        
    }

    public void createTestUsers(){
        addUser("user1", new UserInfo("user1", "p"));
        addUser("user2", new UserInfo("user2", "p"));
        addUser("user3", new UserInfo("user3", "p"));        
    }
    
    //fazer controlo de erros para casos em que nao tem users na frase
    //talvez mudar este tipo de metodos para usar string builder ou algo mais rapido
    @SuppressWarnings("empty-statement")
    private void sendPrivateMessage(Message.RetrievableMessage msg) throws SuspendExecution {
        String[] args = (String[]) msg.o;
        List<String> dest_users = new ArrayList<>();
        String data = args[0] + ": ";
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
        }
        data = data.concat("\n");

        for (String s : dest_users) {
            userPool.get(s).getUser_actor().send(new Message.RetrievableMessage(Message.MessageType.DATA, data));
        }
    }

    //meter controlo para saber se o user já está logged in´
    private void userLogin(Message.RetrievableMessage msg) throws SuspendExecution {
        Message.UserDataMessage data = (Message.UserDataMessage) msg.o;

        if (userPool.containsKey(data.username)
                && userPool.get(data.username).getPassword().equals(data.password)) {
            UserInfo ui = userPool.get(data.username);
            ui.setIsLoggedIn(true); ui.setUser_actor(data.sender);
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.USER_LOGIN_ACK, "Logged in sucessfully!\n"));
        } else {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, "Login information incorrect!\n"));
        }
    }

    //meter controlo para ver se o user já existe ou se user ja esta logged in (nao pode registar-se quando está logged in)
    private void userRegister(Message.RetrievableMessage msg) throws SuspendExecution {
        Message.UserDataMessage data = (Message.UserDataMessage) msg.o;
        if (!userPool.containsKey(data.username)) {
            userPool.put(data.username, new UserInfo(data.username, data.password));
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.USER_REGISTER_ACK, "Account Created Sucessfully"));
        } else {
            msg.sender.send(new Message.RetrievableMessage(Message.MessageType.LINE, "Error: Username already taken."));
        }

    }

    @Override
    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {
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
                case USER_LIST_USERS: //implementar (talvez necessite de lock no map)
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
