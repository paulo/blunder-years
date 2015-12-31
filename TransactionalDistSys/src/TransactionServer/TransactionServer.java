package TransactionServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.sql.SQLException;

public class TransactionServer {

    private static TServerLog server_log;
    private final TransactionManager t_manager;

    TransactionServer() throws SQLException, RemoteException {
        server_log = new TServerLog();
        server_log.initDBConnection();
        t_manager = new TransactionManager(server_log);
    }

    public String setTServerInfo() throws IOException {
        BufferedReader userIn = new BufferedReader(new InputStreamReader(System.in));
        System.out.print("Set server port: ");

        return userIn.readLine();
    }

    public void bindRMI() {
        try {
            Registry registry = LocateRegistry.getRegistry(3333);

            registry.rebind("transactionManager", t_manager);
        } catch (Exception e) {
            System.err.println("ComputeEngine exception:");
            e.printStackTrace();
        }
    }

    public void createClientHandler(Socket cli) throws IOException {
        new ClientHandler(cli, t_manager).start();
    }

    //depois meter aqui a ler a porta do stdin
    public static void main(String[] args) throws RemoteException, Exception {

        TransactionServer ts = new TransactionServer();
        ServerSocket srv = null;

        try {
            srv = new ServerSocket(55555);
            ts.bindRMI();
            
            while (true) {
                Socket cli = srv.accept();
                System.out.println("1. Client connection received");
                ts.createClientHandler(cli);
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
