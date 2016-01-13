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

    @Override
    public synchronized void commitTransaction(String TxId) throws RemoteException {
        TwoPCIf source_bank = null; TwoPCIf target_bank = null;
        try {
            Registry registry = LocateRegistry.getRegistry(3333);
            source_bank = (TwoPCIf) registry.lookup(server_log.getResource(TxId, 1));
            target_bank = (TwoPCIf) registry.lookup(server_log.getResource(TxId, 2));

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
            if(source_bank==null) abortTransaction(TxId,0);
            else if(target_bank==null) abortTransaction(TxId, 1);
            else abortTransaction(TxId, 2);
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
                System.out.println("Commit sucessful");
            } else {
                abortTransaction(TxId, 2);
                System.out.println("RollBack");
            }
        } catch (RemoteException ex) {
            System.out.println("Phase 2 failed, trying again.");
            sleep(5000);
            phase2(source_bank, target_bank, TxId, phase1_r);
        }
    }

    /**
     * Initiate transaction. Creates new context (synchronized).
     * @return New transaction context id.
     * @throws RemoteException 
     */
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
    
    /**
     * Register resource at Transactional Server.
     * @param Txid Transaction context id
     * @param type Value 1 if resource/bank is the one where the withdraw occurs, 2 otherwise
     * @param resource_id Resource id
     * @throws RemoteException 
     */
    @Override
    public void registerResource(String Txid, int type, String resource_id) throws RemoteException {
        try {
            server_log.logResource(Txid, type, resource_id);
            System.out.println(resource_id + " registered");
        } catch (SQLException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * Abort transaction, rollback on given resources and remove transaction context from log
     * @param Txid Transaction context id
     * @param resource_type Value 1 if rollback only needed at source resource, 2 if needed on both, 0
     * if needed on neither
     * @throws RemoteException 
     */
    @Override
    public void abortTransaction(String Txid, int resource_type) throws RemoteException {
        Registry registry = null;
        TwoPCIf source_bank = null;
        TwoPCIf target_bank = null;

        try {
            switch (resource_type) {
                case 0:
                    break;
                case 1:
                    registry = LocateRegistry.getRegistry(3333);
                    source_bank = (TwoPCIf) registry.lookup(server_log.getResource(Txid, 1));
                    source_bank.rollback(Txid);
                    break;
                case 2:
                    registry = LocateRegistry.getRegistry(3333);
                    source_bank = (TwoPCIf) registry.lookup(server_log.getResource(Txid, 1));
                    target_bank = (TwoPCIf) registry.lookup(server_log.getResource(Txid, 2));
                    source_bank.rollback(Txid);
                    target_bank.rollback(Txid);
                    break;
            }
            server_log.removeLog(Txid);

            System.out.println("Transaction aborted");
        } catch (SQLException | NotBoundException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * Recover unfinished transactions at transactional server start. 
     * - "aborted" status: continue phase 2 of TPC with rollback marker
     * - "commit" status: continue phase 2 of TPC with commit marker
     * - "prepare" status: abort work done and rollback on both resources
     */
    public void recoverTransactions() {
        ResultSet res;
        try {
            res = server_log.getActiveTransactions();
            while (res.next()) {
                /*System.out.println("TxId: " + res.getString("TXID")
                 + "\nResourceN1: " + res.getString("RESOURCEN1")
                 + "\nResourceN2: " + res.getString("RESOURCEN2")
                 + "\nStatus: " + res.getString("STATUS"));*/
                String status = res.getString("STATUS");
                switch (status) {
                    case "abort":
                        recoverPhase2(res.getString("RESOURCEN1"), res.getString("RESOURCEN2"), res.getString("TXID"), false);
                        break;
                    case "prepare":
                        abortTransaction(res.getString("TXID"), 2);
                        break;
                    case "commit":
                        recoverPhase2(res.getString("RESOURCEN1"), res.getString("RESOURCEN2"), res.getString("TXID"), true);
                        break;
                }
            }
            res.close();
        } catch (SQLException | RemoteException | NotBoundException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * Recover unfinished transactions with abort or commit marker.
     * @param source Resource id where the operation withdraw was to be made
     * @param target Resource id where the operation deposit was to be made
     * @param TxId Transaction context id
     * @param phase1_r true if commit marker, false otherwise
     * @throws RemoteException
     * @throws NotBoundException 
     */
    private void recoverPhase2(String source, String target, String TxId, boolean phase1_r) throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(3333);
        try {
            TwoPCIf source_bank = (TwoPCIf) registry.lookup(source);
            TwoPCIf target_bank = (TwoPCIf) registry.lookup(target);

            phase2(source_bank, target_bank, TxId, phase1_r);
        } catch (SQLException | InterruptedException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }


    /*private void abortPhase1(String source, String target, String TxId) throws RemoteException, NotBoundException, SQLException {
        Registry registry = LocateRegistry.getRegistry(3333);

        TwoPCIf source_bank = (TwoPCIf) registry.lookup(source);
        TwoPCIf target_bank = (TwoPCIf) registry.lookup(target);

        source_bank.rollback(TxId);
        target_bank.rollback(TxId);
        
        server_log.removeLog(TxId);
    }*/
}
