package ServerActors;

import co.paralleluniverse.actors.ActorRef;

public class UserInfo {

    private String username;
    private String password;
    public boolean isLoggedIn, isAdmin;
    ActorRef user_actor;

    public UserInfo(String username, String password, boolean isLoggedIn, ActorRef user_actor) {
        this.username = username;
        this.password = password;
        this.isLoggedIn = isLoggedIn;
        this.user_actor = user_actor;
    }

    public UserInfo(String username, String password) {
        this.username = username;
        this.password = password;
        this.isLoggedIn = false;
        this.user_actor = null;
    }
    
    public UserInfo(String username, String password, ActorRef user_actor) {
        this.username = username;
        this.password = password;
        this.isLoggedIn = false;
        this.user_actor = user_actor;
    }
    
    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public boolean isIsAdmin() {
        return isAdmin;
    }

    public void setIsAdmin(boolean isAdmin) {
        this.isAdmin = isAdmin;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public boolean isIsLoggedIn() {
        return isLoggedIn;
    }

    public void setIsLoggedIn(boolean isLoggedIn) {
        this.isLoggedIn = isLoggedIn;
    }

    public ActorRef getUser_actor() {
        return user_actor;
    }

    public void setUser_actor(ActorRef user_actor) {
        this.user_actor = user_actor;
    }

    
    
    
}
