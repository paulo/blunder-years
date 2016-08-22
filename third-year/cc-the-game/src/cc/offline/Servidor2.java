/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.offline;

import cc.server.ServerMain;
import static cc.server.ServerMain.DEFAULT_TCP_PORT;
import java.io.IOException;
import java.net.InetAddress;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ruioliveiras
 */
public class Servidor2 {
        public static void main(String[] args) throws IOException {
         ServerMain sm = new ServerMain(5050, DEFAULT_TCP_PORT, InetAddress.getByName("127.0.0.48"));
         sm.init("127.0.0.47", DEFAULT_TCP_PORT + "");
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
