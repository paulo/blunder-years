package TransactionServer;

import BankServer.TwoPCIf;
import java.io.IOException;
import static java.lang.Thread.sleep;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class TransactionManager extends UnicastRemoteObject implements ResourceRecordIf, TransactionControlIf {

    TServerLog server_log;
    int transaction_number;

    TransactionManager(TServerLog server_log) throws RemoteException {
        this.server_log = server_log;
        transaction_number = server_log.getCurrentTransactionNumber();
    }

    //meter controlo de erros para quando o resource ainda não está lá
    @Override
    public void commitTransaction(String TxId) throws RemoteException {
        try {
            Registry registry = LocateRegistry.getRegistry(3333);
            TwoPCIf source_bank = (TwoPCIf) registry.lookup(server_log.getResource(TxId, 1));
            TwoPCIf target_bank = (TwoPCIf) registry.lookup(server_log.getResource(TxId, 2));

            boolean phase1_r = phase1(source_bank, target_bank, TxId);

            if (phase1_r) {
                server_log.updateStatus(TxId, "commit");
            } else {
                server_log.updateStatus(TxId, "abort");
            }

            phase2(source_bank, target_bank, TxId, phase1_r);

        } catch (NotBoundException | SQLException | InterruptedException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        } catch (RemoteException ex) {
            System.out.println("Transaction aborted");
            abortTransaction(TxId);
        }
    }

    //se o prepare falha ele assume que nao esta prepared e aborta
    //mudar para fazer prepare a um de cada vez
    boolean phase1(TwoPCIf source_bank, TwoPCIf target_bank, String TxId) throws SQLException {
        boolean r = true;

        server_log.updateStatus(TxId, "prepare");

        try {
            r = r && (source_bank.prepare(TxId)) && (target_bank.prepare(TxId));
        } catch (RemoteException ex) {
            return false;
        }

        return r;
    }

    //ciclo de 5 em 5 segundos para sempre no commit
    //mudar para fazer apenas rollback do que nao falhou se tiver a dar problemas
    private void phase2(TwoPCIf source_bank, TwoPCIf target_bank, String TxId, boolean phase1_r) throws InterruptedException, SQLException {

        try {
            if (phase1_r) {
                source_bank.commit(TxId);
                target_bank.commit(TxId);
                server_log.removeLog(TxId);
                System.out.println("6. Commit sucessful");
            } else {
                source_bank.rollback(TxId);
                source_bank.rollback(TxId);
                abortTransaction(TxId);
                System.out.println("RollBack");
            }
        } catch (RemoteException ex) {
            System.out.println("Phase 2 failed, trying again");
            sleep(5000);
            phase2(source_bank, target_bank, TxId, true);
        }
    }

    @Override
    public synchronized String beginTransaction() throws RemoteException {
        int new_t_number = this.transaction_number;
        String new_txn_id = "TXN" + transaction_number++;

        try {
            server_log.insertNewLog(new_txn_id);
        } catch (SQLException | IOException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
        return new_txn_id;
    }

    @Override
    public void registerResource(String Txid, int type, String resource_id) throws RemoteException {
        try {
            server_log.logResource(Txid, type, resource_id);
            System.out.println("4. Source " + resource_id + " registered");
        } catch (SQLException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    @Override
    public void abortTransaction(String Txid) throws RemoteException {
        try {
            server_log.removeLog(Txid);
        } catch (SQLException ex) {
            System.out.println("Error aborting transaction");
        }
    }

    public void recoverTransactions() throws RemoteException, NotBoundException {
        ResultSet res;
        try {
            res = server_log.getActiveTransactions();
            System.out.println("List of log entrys: ");
            while (res.next()) {
                System.out.println("TxId: " + res.getString("TXID")
                        + "\nResourceN1: " + res.getString("RESOURCEN1")
                        + "\nResourceN2: " + res.getString("RESOURCEN2")
                        + "\nStatus: " + res.getString("STATUS"));
                String status = res.getString("STATUS");
                switch (status) {
                    case "aborted":
                        recoverPhase2(res.getString("RESOURCEN1"), res.getString("RESOURCEN2"), res.getString("TXID"), false);
                        break;
                    case "prepare":
                        abortPhase1(res.getString("RESOURCEN1"), res.getString("RESOURCEN2"), res.getString("TXID"));
                        break;
                    case "commit":
                        recoverPhase2(res.getString("RESOURCEN1"), res.getString("RESOURCEN2"), res.getString("TXID"), true);
                        break;
                }

            }
        } catch (SQLException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    private void recoverPhase2(String source, String target, String TxId, boolean phase1_r) throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(3333);
        try {
            TwoPCIf source_bank = (TwoPCIf) registry.lookup(server_log.getResource(TxId, 1));
            TwoPCIf target_bank = (TwoPCIf) registry.lookup(server_log.getResource(TxId, 2));

            phase2(source_bank, target_bank, TxId, phase1_r);
        } catch (SQLException | InterruptedException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    //falta implementar aqui
    private void abortPhase1(String source, String target, String TxId) throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(3333);
        try {
            TwoPCIf source_bank = (TwoPCIf) registry.lookup(server_log.getResource(TxId, 1));
            TwoPCIf target_bank = (TwoPCIf) registry.lookup(server_log.getResource(TxId, 2));

            //phase2(source_bank, target_bank, TxId, phase1_r);
        } catch (SQLException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
