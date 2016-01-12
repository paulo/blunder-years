package BankServer;

import TransactionServer.ResourceRecordIf;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.transaction.xa.XAException;

public class Bank extends UnicastRemoteObject implements BankIf, TwoPCIf {

    BankDataOperator bdo;
    Map<String, TXid> t_ids;
    String bank_id;

    Bank(BankDataOperator bank_operator, String bank_id) throws RemoteException {
        this.bdo = bank_operator;
        this.t_ids = new HashMap<>();
        this.bank_id = bank_id;
    }

    @Override
    public boolean deposit(String Txid, int amount, String account_nmr) throws RemoteException {
        System.out.println("New deposit");
        TXid xid = null;
        
        try {
            xid = bdo.beginDeposit(Txid, amount, account_nmr);
        } catch (SQLException | XAException ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }

        if (xid != null) {
            t_ids.put(Txid, xid);
            registerBank(Txid, 2);
            return true;
        } else {
            return false;
        }
    }

    //vai fazer start da operação
    @Override
    public boolean withdraw(String Txid, int amount, String account_nmr) throws RemoteException {
        System.out.println("New withdraw");
        TXid xid = null;

        try {
            xid = bdo.beginWithraw(Txid, amount, account_nmr);
        } catch (SQLException | XAException ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }
        if (xid != null) {
            t_ids.put(Txid, xid);
            //meter para dar false se não for possível inserir no banco
            registerBank(Txid, 1);
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean prepare(String Txid) throws RemoteException {
        try {
            return bdo.phase1prepare(t_ids.get(Txid));
        } catch (Exception ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }
        return false;
    }

    @Override
    public void commit(String Txid) throws RemoteException {
        try {
            bdo.phase2Commit(t_ids.get(Txid));
        } catch (Exception ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Override
    public void rollback(String Txid) throws RemoteException {
        try {
            bdo.rollbackTransaction(t_ids.get(Txid));
        } catch (Exception ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private void registerBank(String Txid, int i) {
        try {
            Registry registry = LocateRegistry.getRegistry(3333);
            
            ResourceRecordIf rr = (ResourceRecordIf) registry.lookup("transactionManager");
            rr.registerResource(Txid, i, bank_id);
            
        } catch (RemoteException | NotBoundException ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
