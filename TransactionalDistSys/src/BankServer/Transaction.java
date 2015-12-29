package BankServer;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Transaction extends UnicastRemoteObject implements OperationIf, TwoPCIf {

    BankDataOperator bdo;
    Map<String, TXid> t_ids;
    
    Transaction(BankDataOperator bank_operator) throws RemoteException {
        this.bdo = bank_operator;
        this.t_ids = new HashMap<>();
    }

    @Override
    public boolean deposit(String Txid, int amount, String account_nmr) throws RemoteException {
        TXid xid = new TXid(Txid);
                
//int currentBalance = account.getBalance();
        //account.setBalance(currentBalance + amount);
        System.out.println("New deposit");
        return false;
    }

    //vai fazer start da operação
    @Override
    public boolean withdraw(String Txid, int amount, String account_nmr) throws RemoteException {
        /*int currentBalance = account.getBalance();
        if(currentBalance - amount > 0){
            account.setBalance(currentBalance - amount);
            return true;
        }
        else{
            return false;
        }*/
        System.out.println("New withdraw");
        return false;
    }

    //talvez seja para passar o xid entre os servidores e nao a string do xid
    @Override
    public boolean prepare(String Txid) throws RemoteException {
        try {
            return bdo.phase1prepare(t_ids.get(Txid));
        } catch (Exception ex) {
            Logger.getLogger(Transaction.class.getName()).log(Level.SEVERE, null, ex);
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
