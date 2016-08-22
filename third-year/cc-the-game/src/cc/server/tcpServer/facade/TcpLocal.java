/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.server.tcpServer.facade;

import cc.model.Challenge;
import cc.model.Question;
import cc.model.User;
import cc.server.tcpServer.ServerState;
import cc.server.ServerToServerFacade;
import cc.server.udpServer.UDPChallengeProvider;
import java.net.InetAddress;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

/**
 *
 * @author ruioliveiras
 */
public class TcpLocal implements ServerToServerFacade {

    private final UDPChallengeProvider challengeProvider;
    private final ServerState state;

    public TcpLocal(ServerState s, UDPChallengeProvider challengeProvider) {
        this.state = s;
        this.challengeProvider = challengeProvider;
    }

    @Override
    public void registerServer(InetAddress ip, int port) {
        state.addNeighbors(ip, port);
    }

    @Override
    public void registerChallenge(String challeName, LocalDate d, LocalTime time, String user, String nick) {
        state.addChallenge(challeName, new Challenge(challeName, d, time));
    }

    @Override
    public void registerAcceptChallenge(String challeName, String name, String nick) {
        String ip = state.getOwnerIp(challeName);
        if (ip.equals("localhost")) {
            Challenge challenge = state.getChallenge(challeName);
            challenge.addSubscribers(new User(name, nick));
        } else {
            state.getNeighbor(ip).registerAcceptChallenge(challeName, name, nick);
        }
    }

    @Override
    public void registerScore(String challengeName, String nick, int score) {
        state.getChallenge(challengeName).addScore(nick,score);
        if (!state.hasGlobalUser(nick)){
            state.addGlobalUser(nick, new User(nick, nick));
        }
        state.getGlobalUser(nick).addRating(score);
    }

    @Override
    public void question(String challengeName, int nQuestion, String question,
            int correct, String[] answers, byte[] img, List<byte[]> music) {
        state.getChallenge(challengeName).addQuestion(new Question(question, answers, correct, img, music));
        challengeProvider.sendQuestion(challengeName, nQuestion, question, correct, answers, img, music);
        
    }

    // this guy will has a copy of the serverState
    // for each action recieve the currect arguments, this don't know that is a PDUServer
}
