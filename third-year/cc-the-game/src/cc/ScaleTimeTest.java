/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc;

import cc.client.ClientBash;
import cc.server.ServerMain;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ruioliveiras
 */
public class ScaleTimeTest {

    private static ServerMain s1;
    private static ServerMain s2;

    private static ClientBash cRui;
    private static ClientBash cOrlando;
    private static ClientBash cPaulo;

    public static void main(String[] args) throws UnknownHostException, IOException, InterruptedException {
        s1 = new ServerMain(5050, 8080, InetAddress.getByName("127.0.0.2"));
        s2 = new ServerMain(5050, 8080, InetAddress.getByName("127.0.0.3"));
        
        s2.init("127.0.0.2", 8080 + "");
        
        SystemTest.startTCPServer_OtherThread(s1, "S1");
        SystemTest.startTCPServer_OtherThread(s2, "S2");

        cRui = new ClientBash("127.0.0.65", "127.0.0.2", 5050);
        cOrlando = new ClientBash("127.0.0.66", "127.0.0.2", 5050);
        cPaulo = new ClientBash("127.0.0.67", "127.0.0.3", 5050);

        
        registerLogin();
        makeChallengeAccept();

    }

    public static void makeChallengeAccept() throws IOException, InterruptedException {
        new Thread(() -> {
            try {
                cPaulo.execute("MAKE_CHALLENGE GrandaFesta 2015-05-02 15:00");
            } catch (IOException ex) {
                throw new RuntimeException();
            }
        }).start();
        Thread.sleep(500);
        new Thread(() -> {
            try {
                cRui.execute("ACCEPT_CHALLENGE GrandaFesta");
            } catch (IOException ex) {
                throw new RuntimeException();

            }
        }).start();
        new Thread(() -> {
            try {
                cOrlando.execute("ACCEPT_CHALLENGE GrandaFesta");
            } catch (IOException ex) {
                throw new RuntimeException();

            }
        }).start();

    }

    public static void registerLogin() throws IOException {
        cRui.execute("REGISTER rui ruioliveiras 123");
        cRui.execute("LOGIN ruioliveiras 123");

        cPaulo.execute("REGISTER paulo prc 123");
        cPaulo.execute("LOGIN prc 123");

        cOrlando.execute("REGISTER orlando orlando 123");
        cOrlando.execute("LOGIN orlando 123");
    }

}
