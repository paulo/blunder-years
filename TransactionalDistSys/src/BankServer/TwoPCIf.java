package BankServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface TwoPCIf extends Remote {
    public boolean prepare(TXid Txid) throws RemoteException;
    public void commit(TXid Txid) throws RemoteException;
    public void rollback(TXid Txid) throws RemoteException;
}