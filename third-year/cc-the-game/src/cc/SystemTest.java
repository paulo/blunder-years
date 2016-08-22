/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc;

import cc.client.ClientBash;
import cc.model.User;
import cc.server.ServerMain;
import static cc.server.ServerMain.DEFAULT_TCP_PORT;
import static cc.server.ServerMain.DEFAULT_UDP_PORT;
import cc.server.udpServer.UDPChallengeProvider;
import java.io.IOException;
import java.net.InetAddress;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.util.Pair;

/**
 *
 * @author ruioliveiras
 */
public class SystemTest {

    private static ServerMain mainMain;


    public static void main(String[] args) throws IOException, InterruptedException {
        int portTcp, portUdp;
        if (args.length >= 3) {
            portTcp = Integer.parseInt(args[1]);
            portUdp = Integer.parseInt(args[2]);
        } else {
            portTcp = DEFAULT_TCP_PORT;
            portUdp = DEFAULT_UDP_PORT;
        }
        ServerMain main = new ServerMain(portUdp, portTcp, InetAddress.getByName("127.0.0.1"));
        mainMain = main;
        startTCPServer_OtherThread(main, "MainServer1");
        testServers();
        testClient();
        testFragmentation();

        //main2.facadeHub.registerChallenge(, LocalDate.MIN, LocalTime.MIN, null, null);
    }

    public static void testServers() throws IOException, InterruptedException {
        int portTcp = DEFAULT_TCP_PORT;
        int portUdp = 5050;

        Thread.sleep(400);
        System.out.println("2-------");

        // comment this is want to testServers
        ServerMain main2 = new ServerMain(portUdp + 1, portTcp + 1, InetAddress.getByName("127.0.0.2"));
        main2.init("127.0.0.1", portTcp + "");
        startTCPServer_OtherThread(main2, "MainServer2");

        Thread.sleep(1000);
        System.out.println("3-------");

        ServerMain main3 = new ServerMain(portUdp + 2, portTcp + 2, InetAddress.getByName("127.0.0.3"));
        main3.init("127.0.0.1", portTcp + "");
        startTCPServer_OtherThread(main3, "MainServer2");

        Thread.sleep(1000);
        System.out.println("4-------");

        ServerMain main4 = new ServerMain(portUdp + 3, portTcp + 3, InetAddress.getByName("127.0.0.4"));
        main4.init("127.0.0.1", portTcp + "");
        startTCPServer_OtherThread(main4, "MainServer4");
    }

    public static void testClient() throws IOException {
        testClient01();
        testClient02();
        testClient03();
        testClient04();
        testClient05();

    }

    public static void testFragmentation() {
//        PDU a = udpHandler.makeQuestion("Circo", 1);
//        PDU b = new PDU();
//       do {
//            byte[] arr = a.toByte();
//            b.initHeaderFromBytes(arr, 0);
//            b.initParametersFromBytes(arr, 8);
//        } while(b.hasParameter(PDUType.CONTINUE));
        //System.out.print("PDU test: " + b);
    }

    public static void testClient01() throws IOException {
        ClientBash c1 = new ClientBash("127.0.0.65", "127.0.0.1", 5050);
        //cb.execute("REGISTAR nome nick pass");
        c1.execute("REGISTER rui ruioliveiras 123");
        c1.execute("LOGIN ruioliveiras 123");
        c1.execute("LOGOUT");

        c1.execute("REGISTER paulo prc 123");
        c1.execute("LOGIN prc 123");
        c1.execute("LOGOUT");

        c1.execute("REGISTER orlando orlando 123");
        c1.execute("LOGIN orlando 123");
        c1.execute("LOGOUT");
    }

    public static void testClient02() throws IOException {
        ClientBash c1 = new ClientBash("127.0.0.66", "127.0.0.1", 5050);
        ClientBash c2 = new ClientBash("127.0.0.67", "127.0.0.1", 5050);
        c1.setWithoutInterface(true);c2.setWithoutInterface(true);
        LocalDate d = LocalDate.now();
        LocalTime t = LocalTime.now().plus(1, ChronoUnit.MINUTES);

        c2.execute("LOGIN prc 123");
        c2.execute("MAKE_CHALLENGE Circo 2015-05-02 15:00");
        c2.execute("LOGOUT");

        //cb.execute("REGISTAR nome nick pass");
        c1.execute("LOGIN ruioliveiras 123");
        c1.execute("MAKE_CHALLENGE oliveirasChallenge 2015-05-02 15:00");
        c1.execute("LIST_CHALLENGES");
        c1.execute("LOGOUT");
    }

    public static void testClient03() throws IOException {
        ClientBash c1 = new ClientBash("127.0.0.68", "127.0.0.1", 5050);
        ClientBash c2 = new ClientBash("127.0.0.69", "127.0.0.1", 5050);
        c1.setWithoutInterface(true);c2.setWithoutInterface(true);
        
        c2.execute("LOGIN ruioliveiras 123");
        c2.execute("ACCEPT_CHALLENGE Circo");
        c2.execute("LOGOUT");

        //cb.execute("REGISTAR nome nick pass");
        c1.execute("LOGIN orlando 123");
        c1.execute("ACCEPT_CHALLENGE Circo");
        c1.execute("LOGOUT");
    }

    public static void testClient04() throws IOException {
        final ClientBash c1 = new ClientBash("127.0.0.70", "127.0.0.1", 5050);
        final ClientBash c2 = new ClientBash("127.0.0.71", "127.0.0.1", 5050);
        final ClientBash c3 = new ClientBash("127.0.0.72", "127.0.0.1", 5050);
        c1.setWithoutInterface(true);c2.setWithoutInterface(true);c3.setWithoutInterface(true);
        
        c1.execute("LOGIN prc 123");
        c2.execute("LOGIN ruioliveiras 123");
        c3.execute("LOGIN orlando 123");

        startUDPChallengeClient_OtherThread("ThreadPaulo", c1, "Circo", Arrays.asList(0, 2, 2)); // one correct
        startUDPChallengeClient_OtherThread("ThreadRui", c2, "Circo", Arrays.asList(2, 1, 2)); // all questions anwsers are correct
        startUDPChallengeClient_OtherThread("ThreadOrlando", c3, "Circo", Arrays.asList(0, 1, 2)); // two correcct

    }

    public static void testClient05() throws IOException {
        ClientBash c1 = new ClientBash("127.0.0.73", "127.0.0.2", 5051);
        ClientBash c2 = new ClientBash("127.0.0.74", "127.0.0.2", 5051);
        ClientBash c3 = new ClientBash("127.0.0.75", "127.0.0.2", 5051);
        c1.setWithoutInterface(true);c2.setWithoutInterface(true);c3.setWithoutInterface(true);
                
        //cb.execute("REGISTAR nome nick pass");
        c1.execute("REGISTER tiago mct 123");
        c1.execute("LOGIN mct 123");
        c1.execute("ACCEPT_CHALLENGE Circo");

        c2.execute("REGISTER fernando fmendes 123");
        c2.execute("LOGIN fmendes 123");
        c2.execute("ACCEPT_CHALLENGE Circo");

        c3.execute("REGISTER joao rodrigues 123");
        c3.execute("LOGIN rodrigues 123");
        c3.execute("ACCEPT_CHALLENGE Circo");

        startUDPChallengeClient_OtherThread("ThreadTiago", c1, "Circo", Arrays.asList(2, 0, 0));
        startUDPChallengeClient_OtherThread("ThreadMendes", c2, "Circo", Arrays.asList(0, 0,0));
        startUDPChallengeClient_OtherThread("ThreadRodrigues", c3, "Circo", Arrays.asList(2, 1, 2));
    }

    public static void startUDPChallengeClient_OtherThread(String threadName, ClientBash cb, String challenge, List<Integer> anwsers) {
        new Thread(() -> {
            try {
                for (int i = 0; i < UDPChallengeProvider.CHALLENGE_NUMQUESTION; i++) {
                    cb.getUDPClient().getNextQuestion();
                    cb.execute("ANSWER " + anwsers.get(i) + " " + challenge + " " + i);
                }
                Thread.sleep(200);
                cb.execute("END");
                cb.execute("LIST_RANKING");
            } catch (IOException ex) {
                ex.printStackTrace();
            } catch (InterruptedException ex) {
                Logger.getLogger(ServerMain.class.getName()).log(Level.SEVERE, null, ex);
            }
        }, threadName).start();
    }

    public static void startTCPServer_OtherThread(final ServerMain main, String name) {
        Thread t1 = new Thread(() -> {
            try {
                main.startTCP();
            } catch (IOException ex) {
                throw new RuntimeException("howad");
            }
        }, "TCP" + name);
        t1.start();
        Thread t2 = new Thread(() -> {
            try {
                main.startUdp();
            } catch (IOException ex) {
                throw new RuntimeException("howad");
            }
        }, "UDP" + name);

        t2.start();
    }
}
