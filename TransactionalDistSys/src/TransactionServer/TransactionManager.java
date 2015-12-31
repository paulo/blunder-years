package TransactionServer;

import BankServer.TwoPCIf;
import java.io.IOException;
import java.net.Socket;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class TransactionManager extends UnicastRemoteObject implements ResourceRecordIf {

    TServerLog server_log;
    int transaction_number;

    TransactionManager(TServerLog server_log) throws RemoteException{
        this.server_log = server_log;
        transaction_number = server_log.getCurrentTransactionNumber();
    }

    //meter controlo de erros para quando o resource ainda não está lá
    String commitTransaction(String TxId) {
        try {
            Registry registry = LocateRegistry.getRegistry(3333);
            TwoPCIf source_bank = (TwoPCIf) registry.lookup(server_log.getResource(TxId, 1));
            TwoPCIf target_bank = (TwoPCIf) registry.lookup(server_log.getResource(TxId, 2));

            boolean phase1_r = phase1(source_bank, target_bank, TxId);

            return phase2(source_bank, target_bank, TxId, phase1_r);

        } catch (RemoteException | NotBoundException | SQLException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    boolean phase1(TwoPCIf source_bank, TwoPCIf target_bank, String TxId) {
        boolean r = true;

        try {
            r = r && (source_bank.prepare(TxId)) && (target_bank.prepare(TxId));
        } catch (RemoteException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }

        return r;
    }

    //meter para so fazer rollback do que falhou
    private String phase2(TwoPCIf source_bank, TwoPCIf target_bank, String TxId, boolean phase1_r) {

        try {
            if (phase1_r) {
                source_bank.commit(TxId);
                target_bank.commit(TxId);
                System.out.println("6. Commit sucessful");
                return "sucess";
            } else {
                source_bank.rollback(TxId);
                source_bank.rollback(TxId);
                return "rollback occured";
            }
        } catch (RemoteException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
        return "phase 2 failed";
    }

    public synchronized String createNewTContext(Socket c_socket) throws IOException {
        int new_t_number = this.transaction_number;
        String new_txn_id = "TXN" + transaction_number++;

        server_log.insertNewLog(new_txn_id, c_socket);

        return new_txn_id;
    }

    @Override
    public void registerResource(String Txid, int type, String resource_id) throws RemoteException {
        server_log.logResource(Txid, type, resource_id);
        System.out.println("4. Source "+resource_id+" registered");
    }
}
