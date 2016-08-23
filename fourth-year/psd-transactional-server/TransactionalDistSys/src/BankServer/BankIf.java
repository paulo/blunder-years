package BankServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface BankIf extends Remote{

    public boolean withdraw(TXid Txid, int amount, String account_nmr) throws RemoteException;
    public boolean deposit(TXid Txid, int amount, String account_nmr) throws RemoteException;
}
