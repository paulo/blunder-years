package TransactionServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.sql.SQLException;

public class TransactionServer {

    private static TServerLog server_log;
    private static TransactionManager t_manager;
    
    TransactionServer() throws SQLException {
        server_log = new TServerLog();
        server_log.initDBConnection();
        t_manager = new TransactionManager(server_log);
    }

    
    public String setTServerInfo() throws IOException{
        BufferedReader userIn = new BufferedReader(new InputStreamReader(System.in));   
        System.out.print("Set server port: ");
 
        return userIn.readLine();
    }
    
    //depois meter aqui a ler a porta do stdin
    public static void main(String[] args) throws Exception {

        TransactionServer ts = new TransactionServer();
        ServerSocket srv = null;
        
        
        try {
            srv = new ServerSocket(55555);

            while (true) {
                Socket cli = srv.accept();
                System.out.println("Client connection received");
                new ClientHandler(cli, t_manager).start();
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
