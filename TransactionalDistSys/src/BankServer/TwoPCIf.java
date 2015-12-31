package BankServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface TwoPCIf extends Remote {
    public boolean prepare(String Txid) throws RemoteException;
    public void commit(String Txid) throws RemoteException;
    public void rollback(String Txid) throws RemoteException;
}
