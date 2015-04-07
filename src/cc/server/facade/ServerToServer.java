/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.server.facade;

import cc.model.Challenge;
import cc.model.User;
import cc.server.ServerState;
import cc.server.ServerToServerFacade;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ruioliveiras
 */
public class ServerToServer implements ServerToServerFacade {

    private ServerState state;

    public ServerToServer(ServerState s) {
        this.state = s;
    }

    @Override
    public boolean registerMySelfServer(byte[] ip, int port) {
        for (ServerToServerFacade sts : state.getNeighbors()) {
            sts.registerServer(ip, port);
        }
        return true;
    }

    @Override
    public boolean registerServer(byte[] ip, int port) {
        try {
            InetAddress inet;
            inet = InetAddress.getByAddress(ip);
            state.addNeighbors(inet.getHostAddress(), inet.getHostAddress(), port);
        } catch (UnknownHostException ex) {
            throw new RuntimeException();
        }
        return true;
    }

    @Override
    public boolean registerChallenge(String challeName, LocalDate d, LocalTime time, String user, String nick) {
        state.addChallenge(challeName, new Challenge(challeName, d, time));
        state.addOwner(challeName, "localhost"); //????
        return true;
    }
    
     
    @Override
    public boolean registerAcceptChallenge(String challeName, String nick) {
        String ip = state.getOwnerIp(challeName);
        if (ip.equals("localhost")){
            state.getChallenge(challeName)
                    .addSubscribers(new User(nick, nick));
        }else {
            state.getNeighbor(ip).registerAcceptChallenge(challeName, nick);
        }
        return true;
    }

    @Override
    public boolean registerScore(String nick, int score) {
        
        return true;
    }

    // this guy will has a copy of the serverState
    // for each action recieve the currect arguments, this don't know that is a PDUServer
}
