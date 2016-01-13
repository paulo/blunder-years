package TransactionServer;

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

    /**
     * Bind transaction manager object to RMI registry
     */
    public void bindRMI() {
        try {
            Registry registry = LocateRegistry.getRegistry(3333);

            registry.rebind("transactionManager", t_manager);
        } catch (Exception e) {
            System.err.println("ComputeEngine exception:");
            e.printStackTrace();
        }
    }

    public static void main(String[] args) throws RemoteException, Exception {

        TransactionServer ts = new TransactionServer();

        ts.bindRMI();
        //recover uncommited transactions
        ts.t_manager.recoverTransactions();
        
        while (true);
    }
}
