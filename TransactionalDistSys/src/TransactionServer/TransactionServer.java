package TransactionServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;

public class TransactionServer {

    private static TServerLog server_log;
    private static TransactionManager t_manager;
    
    TransactionServer() {
        server_log = new TServerLog();
        t_manager = new TransactionManager(server_log);
    }

    
    public static String setTServerInfo() throws IOException{
        BufferedReader userIn = new BufferedReader(new InputStreamReader(System.in));   
        System.out.print("Set server port: ");
 
        return userIn.readLine();
    }
    
    //depois meter aqui a ler a porta do stdin
    public static void main(String[] args) throws Exception {

        ServerSocket srv = null;
        server_log.initDBConnection();

        try {
            srv = new ServerSocket(55555);

            while (true) {
                Socket cli = srv.accept();
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
