package BankServer;

import TransactionServer.ResourceRecordIf;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.transaction.xa.XAException;

public class Bank extends UnicastRemoteObject implements BankIf, TwoPCIf {

    BankDataOperator bdo;
    String bank_id;

    Bank(BankDataOperator bank_operator, String bank_id) throws RemoteException {
        this.bdo = bank_operator;
        this.bank_id = bank_id;
    }

    /**
     * Make withdraw in the resource, using XAResource
     *
     * @param Txid Transaction context id
     * @param amount Amount of the withdraw
     * @param account_nmr Account number upon witch to do the operation
     * @return true if operation successful
     * @throws RemoteException
     */
    @Override
    public boolean deposit(TXid Txid, int amount, String account_nmr) throws RemoteException {
        System.out.println("New deposit");
        boolean operation = false;

        try {
            operation = bdo.beginDeposit(Txid, amount, account_nmr);
        } catch (SQLException | XAException ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }

        if (operation) {
            try {
                registerBank(Txid, 2);
            } catch (NotBoundException ex) {
                return false;
            }
        }
        return operation;
    }

    /**
     * Make withdraw in the resource, using XAResource
     *
     * @param Txid Transaction context id
     * @param amount Amount of the withdraw
     * @param account_nmr Account number upon witch to do the operation
     * @return true if operation successful
     * @throws RemoteException
     */
    @Override
    public boolean withdraw(TXid Txid, int amount, String account_nmr) throws RemoteException {
        System.out.println("New withdraw");
        boolean operation = false;

        try {
            operation = bdo.beginWithdraw(Txid, amount, account_nmr);
        } catch (SQLException | XAException ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }

        if (operation) {
            try {
                registerBank(Txid, 1);
            } catch (NotBoundException ex) {
                return false;
            }
        } 
        return operation;
    }

    /**
     * Call prepare method at BankDataOperator and return result
     * @param Txid Transaction context id
     * @return True if prepare successful, false otherwise
     * @throws RemoteException 
     */
    @Override
    public boolean prepare(TXid Txid) throws RemoteException {
        try {
            return bdo.phase1prepare(Txid);
        } catch (Exception ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }
        return false;
    }

    /**
     * Call commit method at BankDataOperator
     * @param Txid
     * @throws RemoteException 
     */
    @Override
    public void commit(TXid Txid) throws RemoteException {
        try {
            bdo.phase2Commit(Txid);
        } catch (Exception ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * Call rollback method at BankDataOperator
     * @param Txid Transaction context id
     * @throws RemoteException 
     */
    @Override
    public void rollback(TXid Txid) throws RemoteException {
        try {
            bdo.rollbackTransaction(Txid);
        } catch (Exception ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * Register resource (bank) at TransactionManager through RMI
     * @param Txid Transaction context
     * @param i Value 1 if withdraw is to be made at this bank, 2 otherwise
     * @throws RemoteException
     * @throws NotBoundException 
     */
    private void registerBank(TXid Txid, int i) throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(3333);

        ResourceRecordIf rr = (ResourceRecordIf) registry.lookup("transactionManager");
        rr.registerResource(Txid, i, bank_id);
    }
}
