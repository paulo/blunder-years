/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.model;

import java.net.InetAddress;

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
    private byte [] pass;
    /**
     * User nick name.
     */
    private String nick;
    /**
     * The current Rating of the user.
     * This rating can be local or global.
     */
    private int rating;
    /**
     * The ActualChallenge name where this users are subscribed.
     * null if is not in any challenge.
     */
    private String actualChallenge;
    
    private InetAddress currentIP;
    private int currentPort;
    
    public User() {
        rating = 0;
    }

    /** Main constructor
     * 
     * @param name
     * @param nick 
     */
    public User(String name, String nick) {
        this.name = name;
        this.nick = nick;
        rating = 0;
    }
    
    /** Main constructor
     * 
     * @param name
     * @param pass
     * @param nick 
     */
    public User(String name, byte [] pass, String nick) {
        this.name = name;
        this.pass = pass;
        this.nick = nick;
        rating = 0;
    }

    public String getName() {
        return name;
    }

    public byte [] getPass() {
        return pass;
    }

    public String getNick() {
        return nick;
    }

    public int getRating() {
        return rating;
    }
    
    public InetAddress getIP() {
        return currentIP;
    }
    
    public int getPort() {
        return currentPort;
    }

    public void setRating(int rating) {
        this.rating = rating;
    }
    
    public void setIP(InetAddress ip) {
        this.currentIP = ip;
    }
    
    public void setPort(int port) {
        this.currentPort = port;
    }

    public void setActualChallenge(String actualChallenge) {
        this.actualChallenge = actualChallenge;
    }

    public String getActualChallenge() {
        return actualChallenge;
    }

    public int addRating(int score) {
       rating += score;
       return rating;
    }
    
    
}
