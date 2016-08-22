package Users;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Grupo 30
 */


public class User {
    
    /**
     * @param username Nome de utilizador
     * @param password Password do utilizador
     * @param state Estado da sessão do utilizador
     */
    
    private String username;
    private String password;
    boolean state; //para verificar se o user está online ou offline
    Lock changeState;
    
    public User(){
        this.username="";
        this.password="";
        this.state=false;
        changeState = new ReentrantLock();
    }
    
    public User(String u, String p){
        this.username=u;
        this.password=p;
        this.state=false;
        changeState = new ReentrantLock();
    }
    
    public User(User u){
        this.username=u.getName();
        this.password=u.getPass();
        this.state=u.getState();
        changeState = new ReentrantLock();
    }
    
    public String getName(){return this.username;}
    public String getPass(){return this.password;}
    public boolean getState(){return this.state;}
    
    public void setName(String n){this.username=n;}
    public void setPass(String p){this.password=p;}
    public void logIn(){this.state=true;}
    public void logOut(){this.state=false;}
    public Lock getLock(){return this.changeState;}
    
      
    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append("Username: "+this.username+"\n");
        sb.append("Password: "+this.password+"\n");
        
        return sb.toString();
    }
    

    @Override
    public User clone(){
        return new User(this);
    }
    
    @Override
    public boolean equals(Object o){
        if (this == o) return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        User u = (User) o;
        return (this.username.equals(u.getName()) && this.password.equals(u.getPass()));
    }
}