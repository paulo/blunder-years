/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.server.tcpServer.communication;

import cc.server.tcpServer.ServerState;
import cc.pdu.PDU;
import cc.pdu.PDUType;
import cc.server.tcpServer.facade.TcpLocal;
import cc.server.tcpServer.facade.TcpClient;
import cc.server.tcpServer.facade.TcpHub;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

/**
 * This class meets a client and call the correct functions has need.
 *
 * @author ruioliveiras
 */
public class ServerHandler implements Runnable {

    private final TCPCommunication comm;
    private final ServerState state;
    private final TcpLocal facadeMem;
    private final TcpHub facadeHub;
    private final String name;

    /**
     * Main Constructor of ServerHandler
     *
     * @param name
     * @param state
     * @param socket
     * @param facadeMem
     * @param facadeHub
     * @throws IOException
     */
    public ServerHandler(String name, ServerState state, Socket socket,
            TcpLocal facadeMem, TcpHub facadeHub
    ) throws IOException {
        this.name = name;
        this.state = state;
        this.facadeMem = facadeMem;
        this.facadeHub = facadeHub;
        comm = new TCPCommunication(socket);
    }

    @Override
    public void run() {
        try {
            PDU pdu;

            while ((pdu = comm.nextPDU()) != null) {
                if (pdu.getVersion() != 0) {
                    // somefucking unsoported error
                }
                //if this pdu are fragmented, don't wory, because the the serverConnunication work on this and this pdu area already done
                System.out.println("i'm:" + name + ",attending:" + comm.getIp() + " - " + pdu);
                if (pdu.getType() == PDUType.INFO) {
                    foward_InfoV1(pdu);
                } else {
                    foward_OtherV1(pdu);
                }
            }
        }catch (SocketTimeoutException ste){
                System.out.println("sadasdddddddddddddddddddddddddddddddddddddddddddddddd");
               // do nothing just close
                        comm.close();
        }catch (IOException ex) {
            ex.printStackTrace(System.err);
            comm.close();
        } finally {
    
        }
    }

    /**
     * This function receive a PDU and call the correct function, also there are
     * some exceptional cases - where can't only call a method
     *
     * Note this function works for the version 01 of the PDU.
     *
     * @param pdu
     * @throws UnknownHostException
     */
    protected void foward_InfoV1(PDU pdu) throws UnknownHostException {
        if (pdu.getType().getId() != PDUType.INFO.getId()) {
            // error this is just INFOS 
        }

        PDUType[] newChallenge = {PDUType.INFO_CHALLE, PDUType.INFO_DATE, PDUType.INFO_HOUR, PDUType.INFO_NAME, PDUType.INFO_NICK};
        PDUType[] newServer = {PDUType.INFO_IPSERVER, PDUType.INFO_PORT};
        PDUType[] registerAcceptChallenge = {PDUType.INFO_CHALLE, PDUType.INFO_NAME, PDUType.INFO_NICK};
        PDUType[] registerScore = {PDUType.INFO_CHALLE, PDUType.INFO_NICK, PDUType.INFO_SCORE};
        Object[] p;

        if ((p = checkRequest(pdu, newChallenge)) != null) {
            facadeMem.registerChallenge(
                    (String) p[0],
                    (LocalDate) p[1],
                    (LocalTime) p[2],
                    (String) p[3],
                    (String) p[4]
            );
            state.addOwner((String) p[0], comm.getIp());
        } else if ((p = checkRequest(pdu, newServer)) != null) {
            final InetAddress ip = (InetAddress) p[0];
            final Short port = (Short) p[1];

            // Test if it is exceptional cases
            if (state.hasNeighbors(comm.getIp())) {
                facadeMem.registerServer(ip, port);
            } else {
                // if the server that are announcing don't exist (are not in the neighbors):
                //     -> announce the newServer to my neighbors

                // register in other servers
                facadeHub.registerServer(ip, port);
                // register in the current server
                facadeMem.registerServer(ip, port);
                // register all server in the newServer
                state.getNeighbors().stream()
                        .filter((server) -> (server instanceof TcpClient))
                        .map(server -> (TcpClient) server)
                        // Because the new server are my neighbor, test it.
                        .filter((server) -> (!server.getServerIp().equals(ip.toString())))
                        .forEach((server) -> {
                            state.getNeighbor(ip.toString())
                            .registerServer(server.getServerIpByte(), server.getServerPort());
                        });

            }
        } else if ((p = checkRequest(pdu, registerAcceptChallenge)) != null) {
            String challengeName = (String) p[0];
            facadeMem.registerAcceptChallenge(
                    challengeName,
                    (String) p[1],
                    (String) p[2]
            );
            state.getChallenge(challengeName)
                    .addServer((TcpClient) state.getNeighbor(comm.getIp()));
        } else if ((p = checkRequest(pdu, registerScore)) != null) {
            facadeMem.registerScore(
                    (String) p[0],
                    (String) p[1],
                    (Integer) p[2]
            );
        } else {
           throw new RuntimeException("Unknow PDU request type");
        }
    }

    protected void foward_OtherV1(PDU pdu) {
        if (pdu.getType().getId() != PDUType.REPLY.getId()) {
            //errro
        }
        PDUType[] newQuestion = {PDUType.REPLY_CHALLE, PDUType.REPLY_NUM_QUESTION,
            PDUType.REPLY_QUESTION, PDUType.REPLY_CORRECT, PDUType.REPLY_NUM_ANSWER,
            PDUType.REPLY_ANSWER, PDUType.REPLY_IMG, PDUType.REPLY_NUM_BLOCK,
            PDUType.REPLY_BLOCK};
        Object[] p;
        if ((p = checkRequest(pdu, newQuestion)) != null) {
            String aux[] = new String[((List) p[4]).size()];
            facadeMem.question(
                    (String) p[0], (byte) p[1], (String) p[2], (byte) p[3],
                    ((List<String>) p[5]).toArray(aux),
                    (byte[]) p[6],
                    (List<byte[]>) (List) p[8]
            );
        } else {
           throw new RuntimeException("Unknow PDU request type");
        }
    }

    /**
     * Simple auxiliary function
     *
     * @param pdu
     * @param required
     * @return
     */
    private Object[] checkRequest(PDU pdu, PDUType[] required) {
        Object[] ret = new Object[required.length];
        int i = 0;
        for (PDUType p : required) {
            if (!pdu.hasParameter(p)) {
                return null;
            } else {
                List<Object> r = pdu.getAllParameter(p);
                if (r.size() == 1) {
                    ret[i++] = r.get(0);
                } else {
                    ret[i++] = r;
                }
            }
        }
        return ret;
    }

}
