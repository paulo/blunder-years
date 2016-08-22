/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.server.tcpServer.communication;

import cc.pdu.PDU;
import cc.pdu.PDUType;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;

/**
 * This class as purpose encapsulate a socket - to use PDU. So it will read PDU
 * objects, and send PDU objects
 *
 * @author ruioliveiras
 */
public class TCPCommunication {

    public static int TIMEOUT_DEFAULT = 10*60*1000; // milisecods;
    public static int TTL_DEFAULT = 3;
    private Socket socket;
    private InputStream is;
    private OutputStream os;

    public InetAddress localAddress;

    public TCPCommunication(InetAddress ip, int port, InetAddress locaAddress) {
        try {
            this.localAddress = locaAddress;
            socket = new Socket(ip, port, locaAddress, 0);
            init();
        } catch (IOException ex) {
            throw new RuntimeException();
        }
    }

    /**
     * Creates a new ServerCommunication using a pre-initialized socket.
     *
     * @param socket
     */
    public TCPCommunication(Socket socket) {
        this.socket = socket;
        init();

    }

    private void init() {
        try {
            socket.setSoTimeout(TIMEOUT_DEFAULT);
            is = socket.getInputStream();
            os = socket.getOutputStream();
        } catch (IOException ex) {
            throw new RuntimeException("a");
        }
    }

    /**
     * Get the Next PDU, if there aren't wait timeout milliseconds.
     *
     * @param timeout in milliseconds
     * @return
     * @throws SocketTimeoutException
     * @throws SocketException
     */
    public PDU nextPDU(int timeout) throws SocketTimeoutException, SocketException {
        socket.setSoTimeout(timeout);
        PDU p = nextPDU();
        socket.setSoTimeout(TIMEOUT_DEFAULT);
        return p;
    }

    /**
     * Get the Next PDU, if there aren't wait TIMEOUT_DEFAULT .
     *
     * @return
     * @throws java.net.SocketTimeoutException
     * @throws java.net.SocketException
     */
    public PDU nextPDU() throws SocketTimeoutException, SocketException {
        PDU pdu = new PDU();
        byte[] headerBuffer = new byte[8];
        byte[] bodyBuffer;
        try {
            do {
                if (is.read(headerBuffer, 0, 8) == 8) {
                    pdu.initHeaderFromBytes(headerBuffer, 0);

                    bodyBuffer = new byte[pdu.getSizeBytes()];
                    if (pdu.getParameterSizeBytes() > bodyBuffer.length) {

                    }

                    if (is.read(bodyBuffer, 0, pdu.getParameterSizeBytes()) != pdu.getParameterSizeBytes()) {
                        //error
                    }
                    pdu.initParametersFromBytes(bodyBuffer, 0);
                } else {
                    //error
                }
            } while (pdu.hasContinue());
        } catch (SocketTimeoutException s) {
            throw s;
        } catch (IOException ex) {
            throw new SocketException("IOExecption reading inputstream: " + ex.getMessage());
        }

        return pdu;
    }

    /**
     * This method sends a PDU
     *
     * @param p
     */
    public void sendPDU(PDU p) {
        sendPDU(p, TTL_DEFAULT);
    }

    /**
     * Protected method to send a pdu, but bec
     *
     * @param p
     * @param ttl
     */
    protected void sendPDU(PDU p, int ttl) {
        try {
            if (is.available() > 0) {
                PDU pdu = nextPDU();
                if (pdu.getVersion() == 0) {
                    closeSocket();
                    throw new SocketException("Received close pdu");
                }
            }
            os.write(p.toByte());
            os.flush();
        } catch (SocketException se) {
            // Socket Exception Try to reconnect ttl times.
            try {
                this.reconnect();
            } catch (IOException ex) {
                throw new RuntimeException(ex);
            }
            if (ttl > 0) {
                sendPDU(p, ttl - 1);
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    /**
     * End this connection
     */
    public void close() {
        PDU close = new PDU(0, PDUType.TCP_CONTROL);
        close.setLabel(1);
        try {
            sendPDU(close, 0);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        closeSocket();
    }

    private void closeSocket() {
        try {
            is.close();
        } catch (Exception ex) {
        }
        try {
            os.close();
        } catch (Exception ex) {
        }
        try {
            socket.close();
        } catch (Exception ex) {
        }
    }

    public void reconnect() throws IOException {
        this.closeSocket();
        Socket newSocket = new Socket(
                socket.getInetAddress(), socket.getPort(),
                localAddress, 0
        );
        this.socket = newSocket;
        this.socket.setSoTimeout(TIMEOUT_DEFAULT);
        this.is = socket.getInputStream();
        this.os = socket.getOutputStream();
    }

    /**
     * Get the port number where the socket are connected
     *
     * @return
     */
    public int getPort() {
        return socket.getPort();
    }

    /**
     * Get Ip as String, where the socket are connected
     *
     * @return
     */
    public String getIp() {
        return getIpByte().toString();
    }

    /**
     * Get Ip as InetAdress, where the socket are connected
     *
     * @return
     */
    public InetAddress getIpByte() {
        if (socket.getRemoteSocketAddress() instanceof InetSocketAddress) {
            return ((InetSocketAddress) socket.getRemoteSocketAddress()).getAddress();
        }
        return null;
    }

}
