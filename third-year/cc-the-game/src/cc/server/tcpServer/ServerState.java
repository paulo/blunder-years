/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.server.tcpServer;

import cc.model.Challenge;
import cc.model.Question;
import cc.model.User;
import cc.server.ServerToServerFacade;
import cc.server.tcpServer.facade.TcpClient;
import java.net.InetAddress;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 *
 * @author ruioliveiras
 */
public class ServerState {

    /**
     * Map to save the servers neighbors, this goes from IP to the facade
     * client.
     */
    private final Map<String, ServerToServerFacade> neighbors;

    /**
     * Map to save the local users, goes from IP to User.
     */
    private final Map<String, User> sessions;
    
    /**
     * Map to save the local users, goes from NickName to User.
     */
    private final Map<String, User> localUsers;

    /**
     * Map to save the All users for global rating. (goes from nick to User)
     */
    private final Map<String, User> globalUsers;
    
    /**
     * Map to store all the current challenges in the System.
     * (Goes from challenge name to challenge)
     * @see challangeOwner#challengeOwner
     */
    private final Map<String, Challenge> challenges;
    
    /**
     * Map to store all 
     */
    private final Map<Integer,Question> questions;

    /**
     * Map to store what challenges are from the current server, Goes from
     * challenge name to server ip or localhost.
     *
     * if is the current Why? because when we do an answer if is in the current
     * server we store, else we forward to the server
     */
    private final Map<String, String> challengeOwner;

    private final InetAddress localIp;
    private final int localTcpPort;
    
    public ServerState(InetAddress localIp, int localPort ) {
        neighbors = new ConcurrentHashMap<>();
        localUsers = new ConcurrentHashMap<>();
        challenges = new ConcurrentHashMap<>();
        challengeOwner = new ConcurrentHashMap<>();
        globalUsers = new ConcurrentHashMap<>();
        questions = new ConcurrentHashMap<>();
        sessions = new ConcurrentHashMap<>();
        this.localIp = localIp;
        this.localTcpPort = localPort;
    }

    /**
     *  Check if this neighbor exist
     * @param who
     * @return 
     */
    public boolean hasNeighbors(String who) {
        return neighbors.containsKey(who);
    }

    /**
     * Add a new neighbor.
     * @param ip
     * @param port 
     */
    public void addNeighbors(InetAddress ip, int port) {
        neighbors.put(ip.toString(), new TcpClient(ip, port, localIp));
    }
   
    /**
     * Get a neighbor from is ip
     * @param who
     * @return 
     */
    public ServerToServerFacade getNeighbor(String who) {
        return neighbors.get(who);
    }

    /**
     * get all neighbors
     * @return 
     */
    public Collection<ServerToServerFacade> getNeighbors() {
        return neighbors.values();
    }

    /**
     * Get all Ips of neightboars
     * @return 
     */
    public Collection<String> getNeighborIps() {
        return neighbors.keySet();
    }

    
        /**
     * Get a User from his ip
     * @param ip
     * @return 
     */
    public User getSession(String ip) {
        return sessions.get(ip);
    }
    
    /**
     * Add a new User.
     * @param ip
     * @param u
     */
    public void addSession(String ip, User u) {
        sessions.put(ip, u);
    }
    
    /**
     * Remove a session.
     * @param ip
     */
    public void removeSession(String ip) {
        sessions.remove(ip);
    }

    /**
     *  Check if this User exist
     * @param who
     * @return 
     */
    public boolean hasLocalUser(String who) {
        return localUsers.containsKey(who);
    }

    /**
     * Add a new User.
     * @param who
     * @param u
     */
    public void addLocalUser(String who, User u) {
        localUsers.put(who, u);
    }
    
    /**
     * Get a User from is nick
     * @param who
     * @return 
     */
    public User getLocalUser(String who) {
        return localUsers.get(who);
    }

    /**
     * get all User
     * @return 
     */
    public Collection<User> getLocalUsers() {
        return localUsers.values();
    }

    /**
     *  Check if this User exist
     * @param who
     * @return 
     */
    public boolean hasGlobalUser(String who) {
        return globalUsers.containsKey(who);
    }

    /**
     * Add a new User.
     * @param who
     * @param u
     */
    public void addGlobalUser(String who, User u) {
        globalUsers.put(who, u);
    }

    /**
     * Get a User from is nick
     * @param who
     * @return 
     */
    public User getGlobalUser(String who) {
        return globalUsers.get(who);
    }

    /**
     * get all Global users
     * @return 
     */
    public Collection<User> getGlobalUsers() {
        return globalUsers.values();
    }

    /**
     *  Check if this challenge exist
     * @param who
     * @return 
     */
    public boolean hasChallenge(String who) {
        return challenges.containsKey(who);
    }

    /**
     * Add a new challenge.
     * @param who
     * @param c
     */
    public void addChallenge(String who, Challenge c) {
        challenges.put(who, c);
    }

    /**
     * Get a challenge from is name
     * @param who
     * @return 
     */
    public Challenge getChallenge(String who) {
        return challenges.get(who);
    }

    /**
     * get all challenges
     * @return 
     */
    public Collection<Challenge> getChallenges() {
        return challenges.values();
    }

        
    /**
     * Get the Ip owner of the challange
     * @param challengeName 
     * @return 
     */
    public String getOwnerIp(String challengeName){
        return this.challengeOwner.get(challengeName);
    }
    
    public void addOwner(String challengeName, String ip){
        this.challengeOwner.put(challengeName, ip);
    }
    
    /**
     *  Check if this question exist
     * @param question
     * @return 
     */
    public boolean hasQuestion(Question question) {
        return questions.containsValue(question);
    }

    /**
     * Add a new question.
     * @param question
     */
    public void addQuestion(Question question) {
        int lastId = questions.size();
        questions.put(lastId+1, question);
    }

    /**
     * Get a question from its id
     * @param id
     * @return 
     */
    public Question getQuestion(Integer id) {
        return questions.get(id);
    }

    /**
     * get all questions
     * @return 
     */
    public Collection<Question> getQuestions() {
        return questions.values();
    }

}
