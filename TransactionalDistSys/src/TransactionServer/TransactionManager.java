package TransactionServer;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class TransactionManager {

    public static void main(String[] args) throws Exception {
        /* initDBConnection();
         initTServerConnection();

         int received = receiveFromBank();
         insertIntoTable(received);

         connection.close();
         */

        ServerSocket srv = null;

        try {
            srv = new ServerSocket(55555);

            while (true) {
                Socket cli = srv.accept();
                new TransactionHandler(cli).start();
            }

        } finally {
            if (srv != null) {
                try {
                    srv.close();
                } catch (IOException e) {
                    System.out.println(e.getMessage());
                }
            }
        }

    }
}
