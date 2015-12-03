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

    public void logOutUser(String username){
        if(userPool.containsKey(username))
            userPool.get(username).setIsLoggedIn(false);
    }
        
    //fazer controlo de erros para casos em que nao tem users na frase
    //talvez mudar este tipo de metodos para usar string builder ou algo mais rapido
    @SuppressWarnings("empty-statement")
    private void sendPrivateMessage(Message.RetrievableMessage msg) throws SuspendExecution {
        String[] args = (String[]) msg.o;
        List<String> dest_users = new ArrayList<>();
        String data = args[0]+": ";
        int i=1;
        while(i<args.length){
            if(args[i].startsWith("@")){
                dest_users.add(args[i]);
                i++;
            } else break;
        }
        while(i<args.length) {
            data = data.concat(args[i]);
        }
            data = data.concat("\n");
            
        for(String s : dest_users){
            userPool.get(s).getUser_actor().send(new Message.RetrievableMessage(Message.MessageType.DATA, data));
        }
    }
    
    @Override
    @SuppressWarnings("empty-statement")
    protected Void doRun() throws InterruptedException, SuspendExecution {
        while (receive(msg -> {
            Message.LoginMessage data = (Message.LoginMessage) msg.o;

            switch (msg.type) {
                case USER_REGISTER:
                    if (!userPool.containsKey(data.username)) {
                        //aqui é provavel que nao esteja bem a criação de userinfo
                        userPool.put(data.username, new UserInfo(data.username, data.password));
                        data.sender.send(new Message.NonRetrivableMessage(Message.MessageType.OK, "Account Created Sucessfully"));
                    } else {
                        data.sender.send(new Message.NonRetrivableMessage(Message.MessageType.KO, "Account not created!"));
                    }
                    return true;
                case USER_LOGIN:
                    if (userPool.containsKey(data.username)
                            && userPool.get(data.username).equals(data.password)) {
                        data.sender.send(new Message.NonRetrivableMessage(Message.MessageType.OK, "Logged in sucessful!"));
                    } else {
                        data.sender.send(new Message.NonRetrivableMessage(Message.MessageType.KO, "Login information incorrect!"));
                    }
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
