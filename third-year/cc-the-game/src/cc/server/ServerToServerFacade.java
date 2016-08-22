/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.server;

import java.net.InetAddress;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

/**
 * This interface declare all the business that can be done in server to server
 * communication.
 *
 * @author ruioliveiras
 */
public interface ServerToServerFacade {

    /**
     * This function is to ask the server to registry a new server,
     *
     * @param ip
     * @param port
     */
    public void registerServer(InetAddress ip, int port);

    /**
     * Register a Challenge to a server
     *
     * @param challeName
     * @param d
     * @param time
     * @param user
     * @param nick
     */
    public void registerChallenge(String challeName, LocalDate d, LocalTime time,
            String user, String nick);

    /**
     * Accept challenge, and ask to receive the challenge info. This request
     * should be send only to the server owner of the Challenge
     *
     * @param challeName
     * @param name
     * @param nick
     */
     public void registerAcceptChallenge(String challeName, String name, String nick);

    /**
     * In the end of challenge should register the score of each person
     *
     * @param challeName
     * @param nick
     * @param score
     */
    public void registerScore(String challeName, String nick, int score);
    
    
    /**
     * Send a new question for a certain challenge
     *
     * @param challengeName
     * @param nQuestion
     * @param question
     * @param correct
     * @param music
     * @param img
     */
    public void question(String challengeName, int nQuestion, String question, 
            int correct, String[] answers, byte[] img, List<byte[]> music);

}
