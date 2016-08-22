/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.server.tcpServer.facade;

import cc.pdu.PDU;
import cc.pdu.PDUType;
import cc.server.ServerToServerFacade;
import cc.server.tcpServer.communication.TCPCommunication;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

/**
 *
 * @author ruioliveiras
 */
public class TcpClient implements ServerToServerFacade {

    private TCPCommunication comm;

    public TcpClient(InetAddress ip, int port, InetAddress locaAddress) {
        comm = new TCPCommunication(ip, port, locaAddress);
    }

    @Override
    synchronized public void registerServer(InetAddress ip, int port) {
        PDU pdu = new PDU(PDUType.INFO);
        pdu.addParameter(PDUType.INFO_IPSERVER, ip);
        pdu.addParameter(PDUType.INFO_PORT, port);
        comm.sendPDU(pdu);
    }

    @Override
    synchronized public void registerChallenge(String challeName, LocalDate d, LocalTime time, String user, String nick) {
        PDU pdu = new PDU(PDUType.INFO);
        pdu.addParameter(PDUType.INFO_CHALLE, challeName);
        pdu.addParameter(PDUType.INFO_DATE, d);
        pdu.addParameter(PDUType.INFO_HOUR, time);
        pdu.addParameter(PDUType.INFO_NICK, user);
        pdu.addParameter(PDUType.INFO_NAME, nick);

        comm.sendPDU(pdu);
    }

    @Override
    synchronized public void registerAcceptChallenge(String challeName, String name, String nick) {
        PDU pdu = new PDU(PDUType.INFO);
        pdu.addParameter(PDUType.INFO_CHALLE, challeName);
        pdu.addParameter(PDUType.INFO_NAME, name);
        pdu.addParameter(PDUType.INFO_NICK, nick);
        comm.sendPDU(pdu);
    }

    @Override
    synchronized public void registerScore(String challengeName, String nick, int score) {
        PDU pdu = new PDU(PDUType.INFO);
        pdu.addParameter(PDUType.INFO_CHALLE, challengeName);
        pdu.addParameter(PDUType.INFO_NICK, nick);
        pdu.addParameter(PDUType.INFO_SCORE, score);
        comm.sendPDU(pdu);
    }

    synchronized public void question(String challengeName, int nQuestion, String question,
            int correct, String[] answers, byte[] img, List<byte[]> musics) {

        PDU pdu = new PDU(PDUType.REPLY);

        pdu.addParameter(PDUType.REPLY_CHALLE, challengeName);
        pdu.addParameter(PDUType.REPLY_NUM_QUESTION, (byte) nQuestion);
        pdu.addParameter(PDUType.REPLY_QUESTION, question);
        pdu.addParameter(PDUType.REPLY_CORRECT, (byte) correct);
        for (int i = 0; i < answers.length; i++) {
            pdu.addParameter(PDUType.REPLY_NUM_ANSWER, (byte) (i + 1));
            pdu.addParameter(PDUType.REPLY_ANSWER, answers[i]);
        }
        //@todo fazer o loadImage na classe Question
        pdu.addParameter(PDUType.REPLY_IMG, img);
        pdu.addParameter(PDUType.CONTINUE, (byte) 0);
        comm.sendPDU(pdu);
        for (int i = 0; i < musics.size(); i++) {
            pdu = new PDU(PDUType.REPLY);
            pdu.addParameter(PDUType.REPLY_NUM_BLOCK, (byte) i);
            pdu.addParameter(PDUType.REPLY_BLOCK, musics.get(i));
            if (i + 1 < musics.size()) {
                pdu.addParameter(PDUType.CONTINUE, (byte) 0);
            }
            comm.sendPDU(pdu);
        }
    }

    /**
     * Get this port where this Client are connected
     *
     * @return the port number
     */
    synchronized public int getServerPort() {
        return comm.getPort();
    }

    /**
     * Get Ip as String
     *
     * @return
     */
    synchronized public String getServerIp() {
        return comm.getIp();
    }

    /**
     * Get ip As
     *
     * @return
     */
    synchronized public InetAddress getServerIpByte() {
        return comm.getIpByte();
    }

}
