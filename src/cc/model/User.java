/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.model;

/**
 *
 * @author ruioliveiras
 */
public class User {
    /**
     * User name.
     */
    private String name;
    /**
     * User password.
     */
    private String pass;
    /**
     * User nick name.
     */
    private String nick;
    /**
     * The current Rating of the user.
     * This rating can be local or global.
     */
    private int rating;
    
    public User() {
    }

    /** Main constructor
     * 
     * @param name
     * @param pass
     * @param nick 
     */
    public User(String name, String pass, String nick) {
        this.name = name;
        this.pass = pass;
        this.nick = nick;
        rating = 0;
    }

    public String getName() {
        return name;
    }

    public String getPass() {
        return pass;
    }

    public String getNick() {
        return nick;
    }

    public int getRating() {
        return rating;
    }

    public void setRating(int rating) {
        this.rating = rating;
    }
    
}
