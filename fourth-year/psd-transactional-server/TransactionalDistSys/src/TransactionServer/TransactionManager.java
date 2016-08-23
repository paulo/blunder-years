package TransactionServer;

import BankServer.TXid;
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

    /**
     * Commit transaction. If the transaction is to be done on the same resource, only on commit is sent.
     * @param TxId Transaction context id
     * @throws RemoteException 
     */
    @Override
    public void commitTransaction(TXid TxId) throws RemoteException {
        TwoPCIf source_bank = null;
        TwoPCIf target_bank = null;
        boolean same_bank;
        try {
            Registry registry = LocateRegistry.getRegistry(3333);
            String s_s_bank = server_log.getResource(TxId, 1);
            String s_t_bank = server_log.getResource(TxId, 2);

            same_bank = s_s_bank.equals(s_t_bank);

            source_bank = (TwoPCIf) registry.lookup(s_s_bank);
            if (!same_bank) {
                target_bank = (TwoPCIf) registry.lookup(s_t_bank);
            }

            boolean phase1_r = phase1(source_bank, target_bank, TxId, same_bank);

            if (phase1_r) {
                server_log.updateStatus(TxId, "commit");
            } else {
                server_log.updateStatus(TxId, "abort");
            }

            phase2(source_bank, target_bank, TxId, phase1_r, same_bank);

        } catch (NotBoundException | SQLException | InterruptedException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        } catch (RemoteException ex) {
            System.out.println("Transaction aborted");
            if (source_bank == null || target_bank == null) {
                abortTransaction(TxId);
            } 
        }
    }

    /**
     * Start of phase 1 of TPC. Insert prepare marker in transaction log.
     * @param source_bank Remote interface from the bank where the withdraw will be made
     * @param target_bank Remote interface from the bank where the deposit will be made
     * @param TxId Transaction context id
     * @param same_bank True if transaction is to be made on accounts pertaining to the same resource, false otherwise
     * @return True if both resources prepared successfully, false otherwise
     * @throws SQLException 
     */
    boolean phase1(TwoPCIf source_bank, TwoPCIf target_bank, TXid TxId, boolean same_bank) throws SQLException {
        boolean r = true;

        server_log.updateStatus(TxId, "prepare");

        try {
            r = same_bank ? (r && source_bank.prepare(TxId)) : (r && source_bank.prepare(TxId) && target_bank.prepare(TxId));
        } catch (RemoteException ex) {
            return false;
        }

        return r;
    }

    /**
     * Start of phase 2 of TPC. 
     * @param source_bank Remote interface from the bank where the withdraw will be made
     * @param target_bank Remote interface from the bank where the deposit will be made
     * @param TxId Transaction context id
     * @param phase1_r True if to commit transaction, false to abort
     * @param same_bank True if transaction is to be made on accounts pertaining to the same resource, false otherwise
     * @throws InterruptedException
     * @throws SQLException 
     */
    private void phase2(TwoPCIf source_bank, TwoPCIf target_bank, TXid TxId, boolean phase1_r, boolean same_bank) throws InterruptedException, SQLException {

        try {
            if (phase1_r) {
                source_bank.commit(TxId);
                if (!same_bank) {
                    target_bank.commit(TxId);
                }
                server_log.removeLog(TxId);
                System.out.println("Commit sucessful");
            } else {
                abortTransaction(TxId);
                System.out.println("RollBack");
            }
        } catch (RemoteException ex) {
            System.out.println("Phase 2 failed, trying again.");
            sleep(5000);
            phase2(source_bank, target_bank, TxId, phase1_r, same_bank);
        }
    }

    /**
     * Initiate transaction. Creates new context (synchronized).
     *
     * @return New transaction context id.
     * @throws RemoteException
     */
    @Override
    public synchronized TXid beginTransaction() throws RemoteException {
        int new_t_number = this.transaction_number;
        TXid new_tx = new TXid( "TXN" + transaction_number++);
        try {
            server_log.insertNewLog(new_tx);
        } catch (SQLException | IOException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
        return new_tx;
    }

    /**
     * Register resource at Transactional Server.
     *
     * @param Txid Transaction context id
     * @param type Value 1 if resource/bank is the one where the withdraw occurs, 2 otherwise
     * @param resource_id Resource id
     * @throws RemoteException
     */
    @Override
    public void registerResource(TXid Txid, int type, String resource_id) throws RemoteException {
        try {
            server_log.logResource(Txid, type, resource_id);
            System.out.println(resource_id + " registered");
        } catch (SQLException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * Abort transaction, rollback on resources and remove transaction context from log
     * @param Txid Transaction context id
     * @throws RemoteException
     */
    @Override
    public void abortTransaction(TXid Txid) throws RemoteException {
        Registry registry;
        TwoPCIf source_bank;
        TwoPCIf target_bank;

        try {
            registry = LocateRegistry.getRegistry(3333);
            String s_bank = server_log.getResource(Txid, 1);
            String t_bank = server_log.getResource(Txid, 2);
            if (s_bank != null) {
                source_bank = (TwoPCIf) registry.lookup(s_bank);
                source_bank.rollback(Txid);
            }
            if (t_bank != null) {
                target_bank = (TwoPCIf) registry.lookup(t_bank);
                target_bank.rollback(Txid);
            }

            server_log.removeLog(Txid);

            System.out.println("Transaction aborted");
        } catch (SQLException | NotBoundException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * Recover unfinished transactions at transactional server start. -
     * "aborted" status: continue phase 2 of TPC with rollback marker - "commit"
     * status: continue phase 2 of TPC with commit marker - "prepare" status:
     * abort work done and rollback on both resources
     */
    public void recoverTransactions() {
        ResultSet res;
        try {
            res = server_log.getActiveTransactions();
            while (res.next()) {
                String status = res.getString("STATUS");
                switch (status) {
                    case "abort":
                        recoverPhase2(res.getString("RESOURCEN1"), res.getString("RESOURCEN2"), new TXid(res.getString("TXID")), false);
                        break;
                    case "prepare":
                        abortTransaction(new TXid(res.getString("TXID")));
                        break;
                    case "commit":
                        recoverPhase2(res.getString("RESOURCEN1"), res.getString("RESOURCEN2"), new TXid(res.getString("TXID")), true);
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
     *
     * @param source Resource id where the operation withdraw was to be made
     * @param target Resource id where the operation deposit was to be made
     * @param TxId Transaction context id
     * @param phase1_r true if commit marker, false otherwise
     * @throws RemoteException
     * @throws NotBoundException
     */
    private void recoverPhase2(String source, String target, TXid TxId, boolean phase1_r) throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(3333);
        TwoPCIf source_bank, target_bank = null;
        try {
            boolean same_bank = source.equals(target);

            source_bank = (TwoPCIf) registry.lookup(source);
            if (!same_bank) {
                target_bank = (TwoPCIf) registry.lookup(target);
            }

            phase2(source_bank, target_bank, TxId, phase1_r, same_bank);
        } catch (SQLException | InterruptedException ex) {
            Logger.getLogger(TransactionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
