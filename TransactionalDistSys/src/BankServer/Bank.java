package BankServer;

import java.rmi.RemoteException;
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

    Bank(BankDataOperator bank_operator) throws RemoteException {
        this.bdo = bank_operator;
        this.t_ids = new HashMap<>();
    }

    @Override
    public boolean deposit(String Txid, int amount, String account_nmr) throws RemoteException {
        System.out.println("New deposit");
        TXid xid = null;
        
        try {
            bdo.beginDeposit(Txid, amount, account_nmr);
        } catch (SQLException | XAException ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }

        if (xid != null) {
            t_ids.put(Txid, xid);
            return true;
        } else return false;
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
            return true;
        } else return false;
    }

    //talvez seja para passar o xid entre os servidores e nao a string do xid
    @Override
    public boolean prepare(String Txid) throws RemoteException {
        try {
            return false;//bdo.phase1prepare(t_ids.get(Txid));
        } catch (Exception ex) {
            Logger.getLogger(Bank.class.getName()).log(Level.SEVERE, null, ex);
        }
        return false;
    }

    @Override
    public boolean commit(String Txid) throws RemoteException {
        return false;

    }

    @Override
    public boolean rollbackPhase1(String Txid) throws RemoteException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean rollbackPhase2(String Txid) throws RemoteException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

}
