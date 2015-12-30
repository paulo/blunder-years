package BankServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface BankIf extends Remote{
    public boolean withdraw(String Txid, int amount, String account_nmr) throws RemoteException;
    public boolean deposit(String Txid, int amount, String account_nmr) throws RemoteException;
}
