package TransactionServer;

import java.net.Socket;
import java.sql.SQLException;
import java.sql.Statement;

public class TransactionManager {

    public static void main(String[] args) throws Exception {
        initDBConnection();
        initTServerConnection();

        int received = receiveFromBank();
        insertIntoTable(received);

        connection.close();

        Socket cli = srv.accept();
        new Thread(new ClientHandler(cli, cHub)).start();
        
    }
}
