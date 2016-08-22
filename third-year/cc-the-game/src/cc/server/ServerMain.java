/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.server;

import cc.server.tcpServer.ServerState;
import cc.model.Question;
import cc.pdu.PDU;
import cc.server.tcpServer.facade.TcpLocal;
import cc.server.tcpServer.communication.ServerHandler;
import cc.server.tcpServer.facade.TcpHub;
import cc.server.udpServer.UDPClientHandler;
import cc.server.udpServer.UDPComunication;
import cc.server.udpServer.UDPChallengeProvider;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ruioliveiras
 */
public class ServerMain {

    public final static int DEFAULT_TCP_PORT = 8081;
    public final static int DEFAULT_UDP_PORT = 5050;


    private final String name;
    private final ServerState state;
    /* TCP */
    private final ServerSocket tcpSS;
    private final TcpLocal tcpLocal;
    private final TcpHub tcpHub;

    /* UDP */
    private final UDPComunication udpCom;
    private final UDPClientHandler udpHandler;
    private final UDPChallengeProvider udpChallenge;

    //vai estar declarado static aqui um server state, e um server facade.
    //vai ter um metodo para começar o server handler 
    public ServerMain(int udpPort, int tcpListingPort, InetAddress address) throws IOException {
        this.state = new ServerState(address, tcpListingPort);
        this.name = "" + tcpListingPort;
        this.parseChallengeFile("desafio-000001.txt");

        this.udpCom = new UDPComunication(udpPort, address, 0, null);
        this.udpCom.setLabelMode(false);
        this.udpChallenge = new UDPChallengeProvider(udpCom, state);

        
        this.tcpSS = new ServerSocket(tcpListingPort, 0, address);
        this.tcpLocal = new TcpLocal(state, udpChallenge);
        this.tcpHub = new TcpHub(state);

        udpHandler = new UDPClientHandler(state, tcpLocal, tcpHub, udpCom, udpChallenge);
    }
    

    public void init(String initIp, String initPort) throws UnknownHostException {
        tcpLocal.registerServer(InetAddress.getByName(initIp), Integer.parseInt(initPort));
        state.getNeighbor(InetAddress.getByName(initIp).toString())
                .registerServer(this.tcpSS.getInetAddress(), this.tcpSS.getLocalPort());
    }

    public void startTCP() throws IOException {
        while (true) {
            Socket cn = tcpSS.accept();
            ServerHandler handler = new ServerHandler(name, state, cn, tcpLocal, tcpHub);
            Thread t = new Thread(handler, name + "|handle:" + cn.getRemoteSocketAddress().toString());
            t.start();
        }
    }

    public void startUdp() throws IOException {
        while (true) {
            PDU pdu = udpCom.nextPDU();
            //create thread? no
            pdu = udpHandler.decodePacket(pdu, udpCom.getDestIp(), udpCom.getDestPort());
            udpCom.sendPDU(pdu);
        }
    }

    private void parseChallengeFile(String filepath) {
        String assetsFolder = "assets";
        String imgPath, imgDir, musicPath, musicDir, questionText, line;
        String[] answers, aux;
        int nrQuestions, correctAnsIndex;
        BufferedReader reader;
        Question question;

        answers = new String[3];
        imgDir = musicDir = null;

        try {
            reader = new BufferedReader( new InputStreamReader(
                      new FileInputStream(new File("assets/" + filepath)), "UTF8"));
            
            
            while ((line = reader.readLine()) != null) {
                if (line.contains("music_DIR=")) {
                    musicDir = line.split("=")[1];
                } else if (line.contains("images_DIR=")) {
                    imgDir = line.split("=")[1];
                } else if (line.contains("questions_#=")) {
                    nrQuestions = Integer.parseInt(line.split("=")[1]);
                } else {
                    aux = line.split(",(?=([^\\\"]*\\\"[^\\\"]*\\\")*[^\\\"]*$)");
                    if (musicDir != null) {
                        musicPath = musicDir + "/" + aux[0];
                    } else {
                        musicPath = aux[0];
                    }
                    if (imgDir != null) {
                        imgPath = imgDir + "/" + aux[1];
                    } else {
                        imgPath = aux[1];
                    }
                    aux[2] = aux[2].trim();
                    questionText = aux[2].substring(1, aux[2].length() - 1);
                    aux[3] = aux[3].trim();
                    answers[0] = aux[3].substring(1, aux[3].length() - 1);
                    aux[4] = aux[4].trim();
                    answers[1] = aux[4].substring(1, aux[4].length() - 1);
                    aux[5] = aux[5].trim();
                    answers[2] = aux[5].substring(1, aux[5].length() - 1);
                    correctAnsIndex = Integer.parseInt(aux[6]);
                    question = new Question(questionText, answers.clone(), correctAnsIndex - 1, assetsFolder + "/" + imgPath, assetsFolder + "/" + musicPath);
                    state.addQuestion(question);
                }
            }

        } catch (FileNotFoundException ex) {
            Logger.getLogger(ServerMain.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(ServerMain.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NumberFormatException ex) {
            Logger.getLogger(ServerMain.class.getName()).log(Level.SEVERE, null, ex);
        }

    }
    
    
    public static void main(String[] args) throws IOException {
        ServerMain sm = new ServerMain(5050, DEFAULT_TCP_PORT, InetAddress.getByName("192.168.0.47"));
        new Thread(()->{ try {
            sm.startTCP();
            } catch (IOException ex) {
                Logger.getLogger(ServerMain.class.getName()).log(Level.SEVERE, null, ex);
            }
        }).start();
                new Thread(()->{ try {
            sm.startUdp();
            } catch (IOException ex) {
                Logger.getLogger(ServerMain.class.getName()).log(Level.SEVERE, null, ex);
            }
        }).start();

    }
}
