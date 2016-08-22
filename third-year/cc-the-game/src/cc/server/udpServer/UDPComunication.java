/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.server.udpServer;

import cc.pdu.PDU;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;

/**
 *
 * @author ruioliveiras
 */
public class UDPComunication {

    private DatagramSocket socket;
    private int currentLabel;
    private boolean labelMode;
    private InetAddress sourceIp;
    private InetAddress destIp;
    private int destPort;
    

    public UDPComunication(int sourcePort, InetAddress sourceIp, int destPort, InetAddress destIp) {
        try {
            if (sourcePort == 0 || sourceIp == null) {
                socket = new DatagramSocket();
            } else {
                socket = new DatagramSocket(sourcePort, sourceIp);
                socket.setReceiveBufferSize(1024 * 1024 * 50);
//                System.out.print("HOW MAY KBBYTES: " + socket.getReceiveBufferSize() / 1024);
            }
            this.sourceIp = sourceIp;
            this.destPort = destPort;
            this.destIp = destIp;
            labelMode = true;
        } catch (SocketException ex) {
            System.out.println("Não foi possível criar Cliente.");
        }
    }

    /**
     * Constructor to server case when there are not destiny
     */
    public UDPComunication() {
        try {
            socket = new DatagramSocket();
            labelMode = false;
        } catch (SocketException ex) {
            System.out.println("Não foi possível criar Cliente.");
        }
    }

    public void sendPDU(PDU pdu, InetAddress destIp, int destPort) {
        byte b[];
        pdu.setLabel(currentLabel);
        int i = 0;
        System.out.println("I'm["+sourceIp +"] sending to ["+destIp+"] PDU: "+pdu.toString());
        try {
            if (destIp == null){
                return;
            }
            b = pdu.toByte();
            DatagramPacket send_packet = new DatagramPacket(b, pdu.getSizeBytes(), destIp, destPort);
//            System.out.println("sendPDU fragment: " + Thread.currentThread().getName() + "|" + i++);
            // if (i>1) Thread.sleep(1000);
            this.socket.send(send_packet);
        } catch (IOException exception) {
            System.out.println("Erro ao enviar o datagrama para o servidor");
        }
    }

    public void sendPDU(PDU send_pdu) {
        sendPDU(send_pdu, destIp, destPort);
    }
    
    public PDU nextPDU() {
        byte[] pData;
        boolean hasNext = true;
        PDU lastPDU = new PDU(), pduAux;
        int i = 0;
        do {
            hasNext = false;
            pduAux = new PDU();
            pData = connectionReceiveBytes();
            //reading the pdu just to see the label
            pduAux.initHeaderFromBytes(pData, 0);
//            System.out.println("read fragment: " + Thread.currentThread().getName() + "|" + i++);

            // if is the currect response
            if (!labelMode) {
                currentLabel = pduAux.getLabel();
            }

            if (pduAux.getLabel() == currentLabel) {
                lastPDU.initHeaderFromBytes(pData, 0);
                lastPDU.initParametersFromBytes(pData, 8);
                hasNext = lastPDU.hasContinue();
            } else {
                //try again until has the currect label
                hasNext = true;
            }
        } while (hasNext);

        if (labelMode) {
            currentLabel++;
        }

        return lastPDU;
    }

    private byte[] connectionReceiveBytes() {
        byte[] dadosReceber = new byte[1024 * 128];
        DatagramPacket receive_packet = new DatagramPacket(dadosReceber, dadosReceber.length);
        
        try {
            this.socket.receive(receive_packet);
            destPort = receive_packet.getPort();
            destIp = receive_packet.getAddress();
        } catch (IOException ex) {
            System.out.println("Erro ao receber o pacote no UDPClientCommunication.");
        }

        return dadosReceber;
    }

    public void setLabelMode(boolean labelMode) {
        this.labelMode = labelMode;
    }

    public void setDest(InetAddress destIp, int destPort) {
        this.destIp = destIp;
        this.destPort = destPort;
    }

    public int getCurrentLabel() {
        return currentLabel;
    }

    public InetAddress getDestIp() {
        return destIp;
    }

    public int getDestPort() {
        return destPort;
    }

    public void close() {
        socket.close();
    }
}
